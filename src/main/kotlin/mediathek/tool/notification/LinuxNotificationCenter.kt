package mediathek.tool.notification

import es.blackleg.jlibnotify.JLibnotify
import es.blackleg.jlibnotify.core.DefaultJLibnotifyLoader
import mediathek.config.CommandLineOptions
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger

class LinuxNotificationCenter private constructor(
    private val libNotify: JLibnotify,
) : NotificationBackend {
    private val lifecycleLock = Any()
    private var closed = false

    init {
        logger.info(libNotify.serverInfo)

        if (CommandLineOptions.isDebugModeEnabled()) {
            logger.debug("Server capabilities:")
            for (capability in libNotify.serverCapabilities) {
                logger.debug("\t {}", capability)
            }
        }
    }

    override fun publish(notification: NotificationMessage) {
        synchronized(lifecycleLock) {
            if (closed) {
                return
            }
            val nativeNotification =
                libNotify.createNotification(notification.title, notification.message, "dialog-information")
            nativeNotification.show()
        }
    }

    override fun close() {
        synchronized(lifecycleLock) {
            if (closed) {
                return
            }
            closed = true
            libNotify.unInit()
        }
    }

    companion object {
        private val logger: Logger = LogManager.getLogger()

        fun create(): LinuxNotificationCenter {
            val libNotify = DefaultJLibnotifyLoader().load()
            libNotify.init("MediathekView")
            return try {
                LinuxNotificationCenter(libNotify)
            } catch (error: Throwable) {
                try {
                    libNotify.unInit()
                } catch (closeError: RuntimeException) {
                    error.addSuppressed(closeError)
                }
                throw error
            }
        }
    }
}
