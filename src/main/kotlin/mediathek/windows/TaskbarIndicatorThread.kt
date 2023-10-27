package mediathek.windows

import com.sun.jna.platform.win32.Kernel32
import com.sun.jna.platform.win32.WinBase
import mediathek.tool.threads.IndicatorThread
import org.apache.logging.log4j.LogManager
import java.awt.Taskbar
import java.util.concurrent.TimeUnit
import javax.swing.JFrame

internal class TaskbarIndicatorThread(parent: MediathekGuiWindows) : IndicatorThread() {
    private val taskbar: Taskbar
    private val parent: JFrame

    private fun disableStandby() {
        if (Kernel32.INSTANCE.SetThreadExecutionState(WinBase.ES_CONTINUOUS or WinBase.ES_SYSTEM_REQUIRED) == 0)
            logger.error("disableStandby() failed!")
    }

    private fun enableStandby() {
        if (Kernel32.INSTANCE.SetThreadExecutionState(WinBase.ES_CONTINUOUS) == 0)
            logger.error("enableStandby() failed!")
    }

    override fun run() {
        try {
            while (!isInterrupted) {
                val percentage = calculateOverallPercentage().toInt()
                taskbar.setWindowProgressValue(parent, percentage)
                taskbar.setWindowProgressState(parent, Taskbar.State.NORMAL)
                disableStandby()
                TimeUnit.MILLISECONDS.sleep(500)
            }
        } catch (ignored: InterruptedException) {
        } finally {
            //when we are finished, stop progress
            taskbar.setWindowProgressState(parent, Taskbar.State.OFF)
            enableStandby()
        }
    }

    companion object {
        private val logger = LogManager.getLogger()
    }

    init {
        name = "TaskbarIndicatorThread"
        taskbar = Taskbar.getTaskbar()
        this.parent = parent
    }
}