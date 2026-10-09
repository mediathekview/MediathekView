package mediathek.tool.notification

import org.apache.logging.log4j.LogManager
import java.lang.foreign.*
import java.lang.invoke.MethodHandle
import java.lang.invoke.MethodHandles
import java.lang.invoke.MethodType
import java.nio.charset.StandardCharsets
import java.util.concurrent.ExecutionException
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference

class MacNotificationCenter(
    private val fallbackNotificationCenter: NotificationBackend = GenericNotificationCenter()
) : NotificationBackend {
    private val lifecycleLock = Any()
    private val active = AtomicBoolean(true)
    private var userNotificationsStarted = false

    override fun publish(notification: NotificationMessage) {
        synchronized(lifecycleLock) {
            if (!active.get()) {
                return
            }
            try {
                UserNotifications.show(notification, fallbackNotificationCenter, active)
                userNotificationsStarted = true
            } catch (exception: RuntimeException) {
                logger.error("Failed to initialize macOS notifications", exception)
                fallbackNotificationCenter.publish(notification)
            } catch (error: LinkageError) {
                logger.error("Failed to load macOS notification support", error)
                fallbackNotificationCenter.publish(notification)
            }
        }
    }

    override fun close() {
        synchronized(lifecycleLock) {
            if (!active.compareAndSet(true, false)) {
                return
            }
            try {
                if (userNotificationsStarted) {
                    UserNotifications.cancel(active)
                }
            } finally {
                fallbackNotificationCenter.close()
            }
        }
    }

    private companion object {
        private val logger = LogManager.getLogger()
    }

    private object UserNotifications {
        private const val BLOCK_HAS_SIGNATURE = 1 shl 30
        private const val BLOCK_IS_GLOBAL = 1 shl 28
        private const val UN_AUTHORIZATION_OPTION_SOUND = 1L shl 1
        private const val UN_AUTHORIZATION_OPTION_ALERT = 1L shl 2
        private const val NS_UTF8_STRING_ENCODING = 4L
        private const val POINTER_SIZE = 8L
        private const val BLOCK_SIZE = POINTER_SIZE * 3 + 8

        private val logger = LogManager.getLogger()
        private val arena = Arena.global()
        private val linker = Linker.nativeLinker()
        private val methodHandles = MethodHandles.lookup()
        private val notificationThread = AtomicReference<Thread>()
        private val notificationExecutor: ExecutorService = Executors.newSingleThreadExecutor { command ->
            Thread(command, "MacNotificationCenter").apply {
                isDaemon = true
                notificationThread.set(this)
            }
        }
        private val lookup = SymbolLookup.libraryLookup("/usr/lib/libobjc.dylib", arena)
            .or(SymbolLookup.libraryLookup("/System/Library/Frameworks/Foundation.framework/Foundation", arena))
            .or(SymbolLookup.libraryLookup("/System/Library/Frameworks/UserNotifications.framework/UserNotifications", arena))
            .or(linker.defaultLookup())
        private val msgSendPointer: MemorySegment = lookup.findOrThrow("objc_msgSend")
        private val getClass: MethodHandle = downcallPointer("objc_getClass")
        private val registerSelector: MethodHandle = downcallPointer("sel_registerName")
        private val globalBlockClass: MemorySegment = lookup.findOrThrow("_NSConcreteGlobalBlock")
        private val authorizationBlock = ObjcBlock(
            authorizationUpcall(),
            "v@?B@"
        )
        private val pendingNotifications = mutableListOf<PendingNotification>()
        private var authorizationRequestInFlight = false
        private var authorizationGranted = false
        private var unsupportedLaunchLogged = false

        @Suppress("unused")
        @JvmStatic
        private fun authorizationCallback(block: MemorySegment, granted: Boolean, error: MemorySegment) {
            notificationExecutor.execute(AuthorizationResult(granted))
        }

        fun show(
            notification: NotificationMessage,
            fallbackNotificationCenter: NotificationBackend,
            active: AtomicBoolean,
        ) {
            notificationExecutor.execute(
                ShowNotification(PendingNotification(notification, active), fallbackNotificationCenter)
            )
        }

        fun cancel(active: AtomicBoolean) {
            runOnNotificationThreadAndWait {
                pendingNotifications.removeAll { it.belongsTo(active) }
            }
        }

        private fun showOnNotificationThread(
            message: PendingNotification,
            fallbackNotificationCenter: NotificationBackend,
        ) {
            if (!message.isActive()) {
                return
            }

            try {
                if (!isRunningFromAppBundle()) {
                    logUnsupportedLaunch()
                    fallbackNotificationCenter.publish(message.toNotificationMessage())
                    return
                }

                pendingNotifications += message

                if (authorizationGranted) {
                    deliverPending()
                } else if (!authorizationRequestInFlight) {
                    authorizationRequestInFlight = true
                    requestAuthorization()
                }
            } catch (t: Throwable) {
                logger.error("Failed to display macOS notification", t)
                if (message.isActive()) {
                    try {
                        fallbackNotificationCenter.publish(message.toNotificationMessage())
                    } catch (fallbackError: RuntimeException) {
                        logger.error("Failed to display fallback notification", fallbackError)
                    }
                }
            }
        }

        private fun handleAuthorizationResult(granted: Boolean) {
            try {
                if (authorizationCompleted(granted)) {
                    deliverPending()
                } else {
                    clearPending()
                }
            } catch (t: Throwable) {
                logger.error("Failed to handle macOS notification authorization result", t)
            }
        }

        private fun isRunningFromAppBundle(): Boolean {
            val mainBundle = msgPtr(cls("NSBundle"), "mainBundle")
            val bundlePath = nsStringToString(msgPtr(mainBundle, "bundlePath")) ?: return false

            return bundlePath.endsWith(".app") || bundlePath.contains(".app/")
        }

        private fun logUnsupportedLaunch() {
            if (!unsupportedLaunchLogged) {
                unsupportedLaunchLogged = true
                logger.warn(
                    "macOS UserNotifications are only available when MediathekView is launched from an application bundle"
                )
            }
        }

        private fun requestAuthorization() {
            val center = msgPtr(cls("UNUserNotificationCenter"), "currentNotificationCenter")
            requestAuthorization(center, authorizationBlock.pointer)
        }

        private fun authorizationCompleted(granted: Boolean): Boolean {
            authorizationRequestInFlight = false
            authorizationGranted = granted
            return granted
        }

        private fun deliverPending() {
            val notifications = pendingNotifications.filter(PendingNotification::isActive)
            pendingNotifications.clear()

            notifications.forEach { message ->
                deliverModern(message.title, message.body)
            }
        }

        private fun clearPending() {
            pendingNotifications.clear()
        }

        private fun runOnNotificationThreadAndWait(action: () -> Unit) {
            if (Thread.currentThread() === notificationThread.get()) {
                action()
                return
            }

            try {
                notificationExecutor.submit(action).get()
            } catch (exception: InterruptedException) {
                Thread.currentThread().interrupt()
                throw IllegalStateException("Notification shutdown was interrupted", exception)
            } catch (exception: ExecutionException) {
                throw IllegalStateException("Notification shutdown failed", exception.cause)
            }
        }

        private fun deliverModern(title: String, body: String) {
            val pool = msgPtr(msgPtr(cls("NSAutoreleasePool"), "alloc"), "init")

            try {
                val content = msgPtr(msgPtr(cls("UNMutableNotificationContent"), "alloc"), "init")
                msgVoid(content, "setTitle:", nsString(title))
                msgVoid(content, "setBody:", nsString(body))
                msgVoid(content, "setSound:", msgPtr(cls("UNNotificationSound"), "defaultSound"))

                val request = createNotificationRequest(
                    cls("UNNotificationRequest"),
                    nsString("mediathekview-${System.nanoTime()}"),
                    content,
                )

                val center = msgPtr(cls("UNUserNotificationCenter"), "currentNotificationCenter")
                addNotificationRequest(center, request)
            } finally {
                drain(pool)
            }
        }

        private fun cls(name: String): MemorySegment {
            return getClass.invokeExact(arena.allocateFrom(name, StandardCharsets.UTF_8)) as MemorySegment
        }

        private fun selector(name: String): MemorySegment {
            return registerSelector.invokeExact(arena.allocateFrom(name, StandardCharsets.UTF_8)) as MemorySegment
        }

        private fun nsString(value: String): MemorySegment {
            return createNSString(cls("NSString"), arena.allocateFrom(value, StandardCharsets.UTF_8))
        }

        private fun nsStringToString(value: MemorySegment): String? {
            if (value == MemorySegment.NULL) {
                return null
            }

            val length = utf8Length(value)
            Arena.ofConfined().use { callArena ->
                val buffer = callArena.allocate(length + 1)
                copyUtf8String(value, buffer, length + 1)
                return buffer.getString(0, StandardCharsets.UTF_8)
            }
        }

        private fun msgPtr(receiver: MemorySegment, selector: String): MemorySegment {
            return downcall(
                msgSendPointer,
                FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.ADDRESS)
            ).invokeExact(receiver, selector(selector)) as MemorySegment
        }

        private fun createNSString(receiver: MemorySegment, cString: MemorySegment): MemorySegment {
            return downcall(
                msgSendPointer,
                FunctionDescriptor.of(
                    ValueLayout.ADDRESS,
                    ValueLayout.ADDRESS,
                    ValueLayout.ADDRESS,
                    ValueLayout.ADDRESS
                )
            ).invokeExact(receiver, selector("stringWithUTF8String:"), cString) as MemorySegment
        }

        private fun createNotificationRequest(
            receiver: MemorySegment,
            identifier: MemorySegment,
            content: MemorySegment,
        ): MemorySegment {
            return downcall(
                msgSendPointer,
                FunctionDescriptor.of(
                    ValueLayout.ADDRESS,
                    ValueLayout.ADDRESS,
                    ValueLayout.ADDRESS,
                    ValueLayout.ADDRESS,
                    ValueLayout.ADDRESS,
                    ValueLayout.ADDRESS
                )
            ).invokeExact(
                receiver,
                selector("requestWithIdentifier:content:trigger:"),
                identifier,
                content,
                MemorySegment.NULL,
            ) as MemorySegment
        }

        private fun drain(receiver: MemorySegment) {
            downcallVoid(ValueLayout.ADDRESS, ValueLayout.ADDRESS)
                .invokeExact(receiver, selector("drain"))
        }

        private fun msgVoid(receiver: MemorySegment, selector: String, arg: MemorySegment) {
            downcallVoid(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.ADDRESS)
                .invokeExact(receiver, selector(selector), arg)
        }

        private fun requestAuthorization(receiver: MemorySegment, completionHandler: MemorySegment) {
            downcallVoid(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.JAVA_LONG, ValueLayout.ADDRESS)
                .invokeExact(
                    receiver,
                    selector("requestAuthorizationWithOptions:completionHandler:"),
                    UN_AUTHORIZATION_OPTION_ALERT or UN_AUTHORIZATION_OPTION_SOUND,
                    completionHandler,
                )
        }

        private fun utf8Length(receiver: MemorySegment): Long {
            return downcall(
                msgSendPointer,
                FunctionDescriptor.of(
                    ValueLayout.JAVA_LONG,
                    ValueLayout.ADDRESS,
                    ValueLayout.ADDRESS,
                    ValueLayout.JAVA_LONG
                )
            ).invokeExact(receiver, selector("lengthOfBytesUsingEncoding:"), NS_UTF8_STRING_ENCODING) as Long
        }

        private fun copyUtf8String(receiver: MemorySegment, buffer: MemorySegment, maxLength: Long) {
            downcallVoid(
                ValueLayout.ADDRESS,
                ValueLayout.ADDRESS,
                ValueLayout.ADDRESS,
                ValueLayout.JAVA_LONG,
                ValueLayout.JAVA_LONG
            ).invokeExact(
                receiver,
                selector("getCString:maxLength:encoding:"),
                buffer,
                maxLength,
                NS_UTF8_STRING_ENCODING,
            )
        }

        private fun addNotificationRequest(receiver: MemorySegment, request: MemorySegment) {
            downcallVoid(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.ADDRESS)
                .invokeExact(
                    receiver,
                    selector("addNotificationRequest:withCompletionHandler:"),
                    request,
                    MemorySegment.NULL,
                )
        }

        private fun downcallPointer(symbol: String): MethodHandle {
            return downcall(
                lookup.findOrThrow(symbol),
                FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.ADDRESS)
            )
        }

        private fun downcallVoid(vararg args: ValueLayout): MethodHandle {
            return downcall(msgSendPointer, FunctionDescriptor.ofVoid(*args))
        }

        private fun downcall(symbol: MemorySegment, descriptor: FunctionDescriptor): MethodHandle {
            return linker.downcallHandle(symbol, descriptor)
        }

        private fun authorizationUpcall(): MemorySegment {
            val methodHandle = methodHandles.findStatic(
                UserNotifications::class.java,
                "authorizationCallback",
                MethodType.methodType(
                    Void.TYPE,
                    listOf(MemorySegment::class.java, java.lang.Boolean.TYPE, MemorySegment::class.java),
                ),
            )

            val descriptor =
                FunctionDescriptor.ofVoid(ValueLayout.ADDRESS, ValueLayout.JAVA_BOOLEAN, ValueLayout.ADDRESS)
            return linker.upcallStub(methodHandle, descriptor, arena)
        }

        private class ObjcBlock(invoke: MemorySegment, signature: String) {
            @Suppress("unused")
            private val signatureMemory: MemorySegment = arena.allocateFrom(signature, StandardCharsets.US_ASCII)
            private val descriptor: MemorySegment = arena.allocate(POINTER_SIZE * 3, POINTER_SIZE)
            private val block: MemorySegment

            val pointer: MemorySegment
                get() = block

            init {
                descriptor.set(ValueLayout.JAVA_LONG, 0, 0)
                descriptor.set(ValueLayout.JAVA_LONG, POINTER_SIZE, BLOCK_SIZE)
                descriptor.set(ValueLayout.ADDRESS, POINTER_SIZE * 2, signatureMemory)

                block = arena.allocate(BLOCK_SIZE, POINTER_SIZE)
                block.set(ValueLayout.ADDRESS, 0, globalBlockClass)
                block.set(ValueLayout.JAVA_INT, POINTER_SIZE, BLOCK_HAS_SIGNATURE or BLOCK_IS_GLOBAL)
                block.set(ValueLayout.JAVA_INT, POINTER_SIZE + 4, 0)
                block.set(ValueLayout.ADDRESS, POINTER_SIZE + 8, invoke)
                block.set(ValueLayout.ADDRESS, POINTER_SIZE * 2 + 8, descriptor)
            }
        }

        private class ShowNotification(
            private val message: PendingNotification,
            private val fallbackNotificationCenter: NotificationBackend
        ) : Runnable {
            override fun run() {
                showOnNotificationThread(message, fallbackNotificationCenter)
            }
        }

        private data class PendingNotification(
            val title: String,
            val body: String,
            val type: MessageType,
            private val active: AtomicBoolean,
        ) {
            constructor(message: NotificationMessage, active: AtomicBoolean) :
                this(message.title, message.message, message.type, active)

            fun belongsTo(owner: AtomicBoolean): Boolean = active === owner

            fun isActive(): Boolean = active.get()

            fun toNotificationMessage(): NotificationMessage = NotificationMessage(title, body, type)
        }

        private class AuthorizationResult(private val granted: Boolean) : Runnable {
            override fun run() {
                handleAuthorizationResult(granted)
            }
        }
    }
}
