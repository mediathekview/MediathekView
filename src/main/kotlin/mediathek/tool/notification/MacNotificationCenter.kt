package mediathek.tool.notification

import org.apache.logging.log4j.LogManager
import java.io.IOException
import java.lang.foreign.*
import java.lang.invoke.MethodHandle
import java.lang.invoke.MethodHandles
import java.lang.invoke.MethodType
import java.nio.charset.StandardCharsets
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

class MacNotificationCenter(
    private val fallbackNotificationCenter: INotificationCenter = GenericNotificationCenter()
) : INotificationCenter {
    override fun displayNotification(msg: NotificationMessage) {
        UserNotifications.show(msg, fallbackNotificationCenter)
    }

    @Throws(IOException::class)
    override fun close() {
        fallbackNotificationCenter.close()
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
        private val notificationExecutor: ExecutorService = Executors.newSingleThreadExecutor { command ->
            Thread(command, "MacNotificationCenter").apply {
                isDaemon = true
            }
        }
        private val lookup = SymbolLookup.libraryLookup("/usr/lib/libobjc.dylib", arena)
            .or(SymbolLookup.libraryLookup("/System/Library/Frameworks/Foundation.framework/Foundation", arena))
            .or(SymbolLookup.libraryLookup("/System/Library/Frameworks/UserNotifications.framework/UserNotifications", arena))
            .or(linker.defaultLookup())
        private val msgSendPointer: MemorySegment = lookup.findOrThrow("objc_msgSend")
        private val getClass: MethodHandle = downcallPointer("objc_getClass", ValueLayout.ADDRESS)
        private val registerSelector: MethodHandle = downcallPointer("sel_registerName", ValueLayout.ADDRESS)
        private val globalBlockClass: MemorySegment = lookup.findOrThrow("_NSConcreteGlobalBlock")
        private val authorizationBlock = ObjcBlock(
            upcall(
                "authorizationCallback",
                FunctionDescriptor.ofVoid(ValueLayout.ADDRESS, ValueLayout.JAVA_BOOLEAN, ValueLayout.ADDRESS),
                MemorySegment::class.java,
                java.lang.Boolean.TYPE,
                MemorySegment::class.java
            ),
            "v@?B@"
        )
        private val pendingNotifications = mutableListOf<PendingNotification>()
        private var authorizationRequestInFlight = false
        private var authorizationGranted = false
        private var unsupportedLaunchLogged = false

        @Suppress("unused")
        @JvmStatic
        private fun authorizationCallback(_block: MemorySegment, granted: Boolean, _error: MemorySegment) {
            notificationExecutor.execute(AuthorizationResult(granted))
        }

        fun show(message: NotificationMessage, fallbackNotificationCenter: INotificationCenter) {
            notificationExecutor.execute(ShowNotification(PendingNotification(message), fallbackNotificationCenter))
        }

        private fun showOnNotificationThread(
            message: PendingNotification,
            fallbackNotificationCenter: INotificationCenter
        ) {
            try {
                if (!isRunningFromAppBundle()) {
                    logUnsupportedLaunch()
                    fallbackNotificationCenter.displayNotification(message.toNotificationMessage())
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
            msgVoid(
                center,
                "requestAuthorizationWithOptions:completionHandler:",
                UN_AUTHORIZATION_OPTION_ALERT or UN_AUTHORIZATION_OPTION_SOUND,
                authorizationBlock.pointer
            )
        }

        private fun authorizationCompleted(granted: Boolean): Boolean {
            authorizationRequestInFlight = false
            authorizationGranted = granted
            return granted
        }

        private fun deliverPending() {
            val notifications = pendingNotifications.toList()
            pendingNotifications.clear()

            notifications.forEach { message ->
                deliverModern(message.title, message.body)
            }
        }

        private fun clearPending() {
            pendingNotifications.clear()
        }

        private fun deliverModern(title: String, body: String) {
            val pool = msgPtr(msgPtr(cls("NSAutoreleasePool"), "alloc"), "init")

            try {
                val content = msgPtr(msgPtr(cls("UNMutableNotificationContent"), "alloc"), "init")
                msgVoid(content, "setTitle:", nsString(title))
                msgVoid(content, "setBody:", nsString(body))
                msgVoid(content, "setSound:", msgPtr(cls("UNNotificationSound"), "defaultSound"))

                val request = msgPtr(
                    cls("UNNotificationRequest"),
                    "requestWithIdentifier:content:trigger:",
                    nsString("mediathekview-${System.nanoTime()}"),
                    content,
                    MemorySegment.NULL
                )

                val center = msgPtr(cls("UNUserNotificationCenter"), "currentNotificationCenter")
                msgVoid(center, "addNotificationRequest:withCompletionHandler:", request, MemorySegment.NULL)
            } finally {
                msgVoid(pool, "drain")
            }
        }

        private fun cls(name: String): MemorySegment {
            return getClass.invokeExact(arena.allocateFrom(name, StandardCharsets.UTF_8)) as MemorySegment
        }

        private fun selector(name: String): MemorySegment {
            return registerSelector.invokeExact(arena.allocateFrom(name, StandardCharsets.UTF_8)) as MemorySegment
        }

        private fun nsString(value: String): MemorySegment {
            return msgPtr(cls("NSString"), "stringWithUTF8String:", arena.allocateFrom(value, StandardCharsets.UTF_8))
        }

        private fun nsStringToString(value: MemorySegment): String? {
            if (value == MemorySegment.NULL) {
                return null
            }

            val length = msgLong(value, "lengthOfBytesUsingEncoding:", NS_UTF8_STRING_ENCODING)
            Arena.ofConfined().use { callArena ->
                val buffer = callArena.allocate(length + 1)
                msgVoid(value, "getCString:maxLength:encoding:", buffer, length + 1, NS_UTF8_STRING_ENCODING)
                return buffer.getString(0, StandardCharsets.UTF_8)
            }
        }

        private fun msgPtr(receiver: MemorySegment, selector: String): MemorySegment {
            return downcall(
                msgSendPointer,
                FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.ADDRESS)
            ).invokeExact(receiver, selector(selector)) as MemorySegment
        }

        private fun msgPtr(receiver: MemorySegment, selector: String, arg: MemorySegment): MemorySegment {
            return downcall(
                msgSendPointer,
                FunctionDescriptor.of(
                    ValueLayout.ADDRESS,
                    ValueLayout.ADDRESS,
                    ValueLayout.ADDRESS,
                    ValueLayout.ADDRESS
                )
            ).invokeExact(receiver, selector(selector), arg) as MemorySegment
        }

        private fun msgPtr(
            receiver: MemorySegment,
            selector: String,
            arg1: MemorySegment,
            arg2: MemorySegment,
            arg3: MemorySegment
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
            ).invokeExact(receiver, selector(selector), arg1, arg2, arg3) as MemorySegment
        }

        private fun msgVoid(receiver: MemorySegment, selector: String) {
            downcallVoid(ValueLayout.ADDRESS, ValueLayout.ADDRESS)
                .invokeExact(receiver, selector(selector))
        }

        private fun msgVoid(receiver: MemorySegment, selector: String, arg: MemorySegment) {
            downcallVoid(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.ADDRESS)
                .invokeExact(receiver, selector(selector), arg)
        }

        private fun msgVoid(receiver: MemorySegment, selector: String, arg1: Long, arg2: MemorySegment) {
            downcallVoid(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.JAVA_LONG, ValueLayout.ADDRESS)
                .invokeExact(receiver, selector(selector), arg1, arg2)
        }

        private fun msgLong(receiver: MemorySegment, selector: String, arg: Long): Long {
            return downcall(
                msgSendPointer,
                FunctionDescriptor.of(ValueLayout.JAVA_LONG, ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.JAVA_LONG)
            ).invokeExact(receiver, selector(selector), arg) as Long
        }

        private fun msgVoid(receiver: MemorySegment, selector: String, arg1: MemorySegment, arg2: Long, arg3: Long) {
            downcallVoid(
                ValueLayout.ADDRESS,
                ValueLayout.ADDRESS,
                ValueLayout.ADDRESS,
                ValueLayout.JAVA_LONG,
                ValueLayout.JAVA_LONG
            ).invokeExact(receiver, selector(selector), arg1, arg2, arg3)
        }

        private fun msgVoid(receiver: MemorySegment, selector: String, arg1: MemorySegment, arg2: MemorySegment) {
            downcallVoid(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.ADDRESS)
                .invokeExact(receiver, selector(selector), arg1, arg2)
        }

        private fun downcallPointer(symbol: String, vararg args: ValueLayout): MethodHandle {
            return downcall(
                lookup.findOrThrow(symbol),
                FunctionDescriptor.of(ValueLayout.ADDRESS, *args)
            )
        }

        private fun downcallVoid(vararg args: ValueLayout): MethodHandle {
            return downcall(msgSendPointer, FunctionDescriptor.ofVoid(*args))
        }

        private fun downcall(symbol: MemorySegment, descriptor: FunctionDescriptor): MethodHandle {
            return linker.downcallHandle(symbol, descriptor)
        }

        private fun upcall(methodName: String, descriptor: FunctionDescriptor, vararg parameterTypes: Class<*>): MemorySegment {
            val methodHandle = methodHandles.findStatic(
                UserNotifications::class.java,
                methodName,
                MethodType.methodType(Void.TYPE, parameterTypes.toList())
            )

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
            private val fallbackNotificationCenter: INotificationCenter
        ) : Runnable {
            override fun run() {
                showOnNotificationThread(message, fallbackNotificationCenter)
            }
        }

        private data class PendingNotification(
            val title: String,
            val body: String,
            val type: MessageType
        ) {
            constructor(message: NotificationMessage) : this(message.title, message.message, message.type)

            fun toNotificationMessage(): NotificationMessage {
                return NotificationMessage().also {
                    it.title = title
                    it.message = body
                    it.type = type
                }
            }
        }

        private class AuthorizationResult(private val granted: Boolean) : Runnable {
            override fun run() {
                handleAuthorizationResult(granted)
            }
        }
    }
}
