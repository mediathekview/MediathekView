package mediathek.gui.dialogEinstellungen.allgemein

import java.awt.event.ActionListener
import javax.swing.Timer
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener

/**
 * Listens to changes in a Document and finally fires the assigned Action.
 * All operations will be performed on Swing EDT.
 */
class TimedDocumentListener(taskPerformer: ActionListener) : DocumentListener {
    private val timer = Timer(1_000, taskPerformer).apply {
        isRepeats = false
    }

    private fun restartTimer() {
        timer.restart()
    }

    override fun insertUpdate(e: DocumentEvent) {
        restartTimer()
    }

    override fun removeUpdate(e: DocumentEvent) {
        restartTimer()
    }

    override fun changedUpdate(e: DocumentEvent) {
        restartTimer()
    }
}
