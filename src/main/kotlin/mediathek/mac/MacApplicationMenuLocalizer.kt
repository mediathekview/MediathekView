package mediathek.mac

import org.apache.logging.log4j.LogManager
import java.lang.foreign.*
import java.lang.invoke.MethodHandles
import java.lang.invoke.MethodType
import java.nio.charset.StandardCharsets
import java.util.*

/** Localizes the application menu supplied by the macOS JVM's English DefaultApp.nib. */
internal object MacApplicationMenuLocalizer {
    private val logger by lazy { LogManager.getLogger(MacApplicationMenuLocalizer::class.java) }

    fun localize() {
        if (Locale.getDefault(Locale.Category.DISPLAY).language != Locale.GERMAN.language) {
            return
        }

        try {
            NativeMenu.localizeGerman()
        } catch (failure: Throwable) {
            logger.warn("Could not localize the macOS application menu", failure)
        }
    }

    private object NativeMenu {
        private const val NS_UTF8_STRING_ENCODING = 4L

        private val linker = Linker.nativeLinker()
        private val lookup = SymbolLookup.libraryLookup("/usr/lib/libobjc.dylib", Arena.global())
            .or(linker.defaultLookup())
        private val msgSend = lookup.findOrThrow("objc_msgSend")
        private val objcGetClass = linker.downcallHandle(
            lookup.findOrThrow("objc_getClass"),
            FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.ADDRESS),
        )
        private val selRegisterName = linker.downcallHandle(
            lookup.findOrThrow("sel_registerName"),
            FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.ADDRESS),
        )
        private val pointerMessage = linker.downcallHandle(
            msgSend,
            FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.ADDRESS),
        )
        private val indexedPointerMessage = linker.downcallHandle(
            msgSend,
            FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.JAVA_LONG),
        )
        private val integerMessage = linker.downcallHandle(
            msgSend,
            FunctionDescriptor.of(ValueLayout.JAVA_LONG, ValueLayout.ADDRESS, ValueLayout.ADDRESS),
        )
        private val utf8LengthMessage = linker.downcallHandle(
            msgSend,
            FunctionDescriptor.of(
                ValueLayout.JAVA_LONG,
                ValueLayout.ADDRESS,
                ValueLayout.ADDRESS,
                ValueLayout.JAVA_LONG,
            ),
        )
        private val copyStringMessage = linker.downcallHandle(
            msgSend,
            FunctionDescriptor.of(
                ValueLayout.JAVA_BOOLEAN,
                ValueLayout.ADDRESS,
                ValueLayout.ADDRESS,
                ValueLayout.ADDRESS,
                ValueLayout.JAVA_LONG,
                ValueLayout.JAVA_LONG,
            ),
        )
        private val stringMessage = linker.downcallHandle(
            msgSend,
            FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.ADDRESS),
        )
        private val setTitleMessage = linker.downcallHandle(
            msgSend,
            FunctionDescriptor.ofVoid(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.ADDRESS),
        )
        private val dispatchSync = linker.downcallHandle(
            lookup.findOrThrow("dispatch_sync_f"),
            FunctionDescriptor.ofVoid(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.ADDRESS),
        )
        private val mainQueue = lookup.findOrThrow("_dispatch_main_q")

        fun localizeGerman() {
            Arena.ofShared().use { arena ->
                val call = LocalizationCall(arena)
                val callback = MethodHandles.lookup().findVirtual(
                    LocalizationCall::class.java,
                    "run",
                    MethodType.methodType(Void.TYPE, MemorySegment::class.java),
                ).bindTo(call)
                val upcall = linker.upcallStub(callback, FunctionDescriptor.ofVoid(ValueLayout.ADDRESS), arena)
                // The Swing EDT and AppKit's main thread are different threads.
                dispatchSync.invokeExact(mainQueue, MemorySegment.NULL, upcall)
                call.failure?.let { throw it }
            }
        }

        private class LocalizationCall(private val arena: Arena) {
            @Volatile
            var failure: Throwable? = null
                private set

            @Suppress("UNUSED_PARAMETER")
            fun run(context: MemorySegment) {
                try {
                    translateMenu()
                } catch (error: Throwable) {
                    failure = error
                }
            }

            private fun translateMenu() {
                val application = sendPointer(objcClass("NSApplication"), "sharedApplication")
                val menuBar = sendPointer(application, "mainMenu")
                if (menuBar == MemorySegment.NULL || sendInteger(menuBar, "numberOfItems") == 0L) {
                    return
                }

                val appMenu = sendPointer(sendIndexedPointer(menuBar, "itemAtIndex:", 0), "submenu")
                if (appMenu == MemorySegment.NULL) {
                    return
                }

                for (index in 0 until sendInteger(appMenu, "numberOfItems")) {
                    val item = sendIndexedPointer(appMenu, "itemAtIndex:", index)
                    val englishTitle = readString(sendPointer(item, "title")) ?: continue
                    // Unknown or already localized items keep their title and native action.
                    val germanTitle = germanTitle(englishTitle) ?: continue
                    val title = sendString(objcClass("NSString"), "stringWithUTF8String:", germanTitle)
                    setTitleMessage.invokeExact(item, selector("setTitle:"), title)
                }
            }

            private fun germanTitle(englishTitle: String): String? = when (englishTitle) {
                "About MediathekView" -> "Über MediathekView"
                "Preferences...", "Preferences…", "Settings...", "Settings…" -> "Einstellungen…"
                "Services" -> "Dienste"
                "Hide MediathekView" -> "MediathekView ausblenden"
                "Hide Others" -> "Andere ausblenden"
                "Show All" -> "Alle einblenden"
                "Quit MediathekView" -> "MediathekView beenden"
                else -> null
            }

            private fun objcClass(name: String): MemorySegment =
                objcGetClass.invokeExact(arena.allocateFrom(name, StandardCharsets.UTF_8)) as MemorySegment

            private fun selector(name: String): MemorySegment =
                selRegisterName.invokeExact(arena.allocateFrom(name, StandardCharsets.UTF_8)) as MemorySegment

            private fun sendPointer(receiver: MemorySegment, name: String): MemorySegment =
                pointerMessage.invokeExact(receiver, selector(name)) as MemorySegment

            private fun sendIndexedPointer(receiver: MemorySegment, name: String, index: Long): MemorySegment =
                indexedPointerMessage.invokeExact(receiver, selector(name), index) as MemorySegment

            private fun sendInteger(receiver: MemorySegment, name: String): Long =
                integerMessage.invokeExact(receiver, selector(name)) as Long

            private fun sendString(receiver: MemorySegment, name: String, value: String): MemorySegment =
                stringMessage.invokeExact(receiver, selector(name), arena.allocateFrom(value, StandardCharsets.UTF_8)) as MemorySegment

            private fun readString(nsString: MemorySegment): String? {
                if (nsString == MemorySegment.NULL) {
                    return null
                }

                val length = utf8LengthMessage.invokeExact(
                    nsString,
                    selector("lengthOfBytesUsingEncoding:"),
                    NS_UTF8_STRING_ENCODING,
                ) as Long
                val buffer = arena.allocate(length + 1)
                val copied = copyStringMessage.invokeExact(
                    nsString,
                    selector("getCString:maxLength:encoding:"),
                    buffer,
                    length + 1,
                    NS_UTF8_STRING_ENCODING,
                ) as Boolean
                return if (copied) buffer.getString(0, StandardCharsets.UTF_8) else null
            }
        }
    }
}
