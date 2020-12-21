package mediathek.mac

import mediathek.tool.threads.IndicatorThread
import java.awt.Taskbar
import java.util.concurrent.TimeUnit

/**
 * This thread will update the percentage drawn on the dock icon on OS X.
 */
internal class OsxIndicatorThread : IndicatorThread() {
    private var oldPercentage = 0
    override fun run() {
        val taskbar = Taskbar.getTaskbar()
        taskbar.setProgressValue(0)
        try {
            while (!isInterrupted) {
                val percentage = calculateOverallPercentage().toInt()

                //update in 1pct steps...
                //if icon was already drawn, don´ do it again
                if (oldPercentage != percentage) {
                    taskbar.setProgressValue(percentage)
                }
                oldPercentage = percentage
                TimeUnit.MILLISECONDS.sleep(500)
            }
        } catch (ignored: Exception) {
        } finally {
            //reset the application dock icon
            taskbar.setProgressValue(-1)
            oldPercentage = 0
        }
    }

    init {
        name = "OsxIndicatorThread"
    }
}