package mediathek.tool.notification

import es.blackleg.jlibnotify.JLibnotify
import es.blackleg.jlibnotify.core.DefaultJLibnotifyLoader
import es.blackleg.jlibnotify.exception.JLibnotifyInitException
import es.blackleg.jlibnotify.exception.JLibnotifyLoadException
import mediathek.config.Config
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger
import java.io.Closeable

class LinuxNotificationCenter : INotificationCenter, Closeable {
    private var libNotify: JLibnotify? = null
    var nativeSupport: Boolean = false
        private set

    init {
        try {
            val loader = DefaultJLibnotifyLoader()
            libNotify = loader.load()
            libNotify!!.init("MediathekView")
            nativeSupport = true

            val serverInfo = libNotify!!.serverInfo
            logger.info(serverInfo)

            if (Config.isDebugModeEnabled()) {
                logger.debug("Server capabilities:")
                val caps = libNotify!!.serverCapabilities
                for (cap in caps) {
                    logger.debug("\t {}", cap)
                }
            }
        } catch (e: UnsatisfiedLinkError) {
            nativeSupport = false
            logger.error(MSG_INIT_FAILED, e)
        } catch (e: RuntimeException) {
            nativeSupport = false
            logger.error(MSG_INIT_FAILED, e)
        } catch (e: JLibnotifyLoadException) {
            nativeSupport = false
            logger.error(MSG_INIT_FAILED, e)
        } catch (e: JLibnotifyInitException) {
            nativeSupport = false
            logger.error(MSG_INIT_FAILED, e)
        }
    }

    override fun displayNotification(msg: NotificationMessage) {
        val notification = libNotify!!.createNotification(msg.title, msg.message, "dialog-information")
        notification.show()
    }

    override fun close() {
        libNotify!!.unInit()
    }

    companion object {
        private val logger: Logger = LogManager.getLogger()
        private const val MSG_INIT_FAILED = "Failed to initialize libNotify"
    }
}
