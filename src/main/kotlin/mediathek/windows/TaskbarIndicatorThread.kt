package mediathek.windows

import mediathek.tool.threads.IndicatorThread
import org.apache.logging.log4j.LogManager
import java.awt.Taskbar
import java.lang.foreign.*
import java.lang.invoke.MethodHandle
import java.util.concurrent.TimeUnit
import javax.swing.JFrame

internal class TaskbarIndicatorThread(parent: MediathekGuiWindows) : IndicatorThread() {
    private val parent: JFrame
    private val setThreadExecutionState: MethodHandle?
    private val arena = Arena.ofAuto()

    private fun disableStandby() {
        val res = setThreadExecutionState?.invoke(WinFlags.ES_CONTINUOUS or WinFlags.ES_SYSTEM_REQUIRED) ?: 0
        if (res as Int == 0) {
            logger.error("disableStandby() failed!")
        }
    }

    private fun enableStandby() {
        val res = setThreadExecutionState?.invoke(WinFlags.ES_CONTINUOUS) ?: 0
        if (res as Int == 0) {
            logger.error("enableStandby() failed!")
        }
    }

    override fun run() {
        val taskbar = Taskbar.getTaskbar()
        try {
            while (!isInterrupted) {
                val percentage = calculateOverallPercentage().toInt()
                taskbar.setWindowProgressValue(parent, percentage)
                taskbar.setWindowProgressState(parent, Taskbar.State.NORMAL)
                disableStandby()
                TimeUnit.MILLISECONDS.sleep(500)
            }
        } catch (_: InterruptedException) {
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
        this.parent = parent

        val linker = Linker.nativeLinker()
        val kernel32 = SymbolLookup.libraryLookup("kernel32.dll", arena)
        setThreadExecutionState = linker.downcallHandle(
            kernel32.find("SetThreadExecutionState").orElseThrow(),
            FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.JAVA_INT)
        )
    }
}