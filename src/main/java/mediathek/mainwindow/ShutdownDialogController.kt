package mediathek.mainwindow

import mediathek.gui.AppShutdownWindow
import mediathek.tool.ShutdownState
import java.util.*
import java.util.concurrent.TimeUnit

/**
 * Display a wait dialog with some status message to inform user what is happening currently.
 */
class ShutdownDialogController(private val gui: MediathekGui) {
    private val window: AppShutdownWindow = AppShutdownWindow(gui)
    private var curSteps = 0.0

    init {
        window.progress.maximum = EnumSet.allOf(ShutdownState::class.java).size
    }

    fun show() {
        gui.isEnabled = false
        window.label1.isBusy = true
        window.isVisible = true
    }

    fun setStatus(state: ShutdownState) {
        curSteps++
        window.message.text = state.toString()
        window.message.paintImmediately(0, 0, window.message.width, window.message.height)
        window.progress.value = curSteps.toInt()
        window.progress.paintImmediately(0, 0, window.progress.width, window.progress.height)
        window.label1.paintImmediately(0, 0, window.label1.width, window.label1.height)
        /*try {
            TimeUnit.MILLISECONDS.sleep(750)
        }
        catch (ignored: InterruptedException) {}*/
        if (state == ShutdownState.COMPLETE) {
            try {
                TimeUnit.MILLISECONDS.sleep(500)
                window.dispose()
            } catch (ignored: InterruptedException) {
            }
        }
    }
}