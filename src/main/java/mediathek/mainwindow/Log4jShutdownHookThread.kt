package mediathek.mainwindow

import mediathek.tool.Log4jShutdownCallbackRegistry.Companion.execute

/**
 * Gracefully shutdown config and log.
 * This may be necessary in case the app is not properly quit.
 */
class Log4jShutdownHookThread : Thread() {
    override fun run() {
        //shut down log4j
        execute()
    }
}