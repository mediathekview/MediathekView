package mediathek.gui.dialog

import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenDownload
import mediathek.tool.EscapeKeyHandler
import java.awt.BorderLayout
import java.awt.event.*
import javax.swing.JFrame
import javax.swing.JOptionPane
import javax.swing.Timer
import kotlin.time.Duration.Companion.seconds

class DialogContinueDownload(
    private val parent: JFrame?,
    datenDownload: DatenDownload,
    private val direkterDownload: Boolean,
) : DialogContinueDownloadBase(parent) {
    enum class DownloadResult {
        CANCELLED,
        CONTINUE,
        RESTART_WITH_NEW_NAME,
    }

    var result: DownloadResult = DownloadResult.CONTINUE
        private set

    var isNewName: Boolean = false
        private set

    private val downloadZielPanel = MVPanelDownloadZiel(null, datenDownload, false)
    private val countdownTimer = Timer(0, CountdownAction()).apply {
        isRepeats = true
    }

    init {
        if (!direkterDownload) {
            jButtonWeiter.text = "Überschreiben"
            if (!datenDownload.checkAufrufBauen()) {
                jPanelNewName.isVisible = false
            }
        }

        jPanelPath.layout = BorderLayout(0, 0)
        jPanelPath.add(downloadZielPanel, BorderLayout.CENTER)

        parent?.let { setLocationRelativeTo(it) }

        jLabel1.text =
            "<html>Der Film \"${datenDownload.title}\" existiert bereits.<br>Wie möchten Sie fortfahren?</html>"

        jButtonNeuerName.addActionListener {
            isNewName = downloadZielPanel.setPfadName_geaendert()
            if (!direkterDownload && !isNewName) {
                JOptionPane.showMessageDialog(
                    parent,
                    "Der Dateiname wurde nicht geändert!",
                    "Datei existiert bereits!",
                    JOptionPane.ERROR_MESSAGE,
                )
            } else {
                result = DownloadResult.RESTART_WITH_NEW_NAME
                close()
            }
        }

        jButtonAbbrechen.addActionListener { cancel() }

        EscapeKeyHandler.installHandler(this) { cancel() }

        addWindowListener(object : WindowAdapter() {
            override fun windowClosing(event: WindowEvent) {
                cancel()
            }
        })

        jButtonWeiter.addActionListener {
            result = DownloadResult.CONTINUE
            close()
        }

        countdownTimer.start()

        pack()

        rootPane.defaultButton = jButtonWeiter

        addComponentListener(object : ComponentAdapter() {
            override fun componentShown(event: ComponentEvent) {
                // Display dialog slightly lower than normal to prevent inadvertent presses from other dialogs (#686).
                val location = locationOnScreen
                setLocation(location.x, location.y + 70)
            }
        })
    }

    private fun cancel() {
        result = DownloadResult.CANCELLED
        close()
    }

    private fun close() {
        countdownTimer.stop()
        dispose()
    }

    private inner class CountdownAction : ActionListener {
        private var countdown = ApplicationConfiguration.getInstance().downloadContinuationTime

        override fun actionPerformed(event: ActionEvent) {
            if (countdown > 0) {
                jButtonWeiter.text = if (direkterDownload) {
                    "Weiterführen in ${countdown}s"
                } else {
                    "Überschreiben in ${countdown}s"
                }
                countdownTimer.delay = 1.seconds.inWholeMilliseconds.toInt()
            } else {
                result = DownloadResult.CONTINUE
                close()
            }
            countdown--
        }
    }
}
