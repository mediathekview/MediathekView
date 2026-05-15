package mediathek.mainwindow

import javax.swing.JLabel
import javax.swing.JProgressBar

interface StatusBarProgressHandle : AutoCloseable {
    fun label(): JLabel

    fun progressBar(): JProgressBar

    override fun close()
}
