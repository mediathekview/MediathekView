package mediathek.gui.dialog

import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenDownload
import mediathek.i18n.UiMessages
import mediathek.tool.EscapeKeyHandler
import org.pushingpixels.radiance.swing.ktx.addDelayedComponentListener
import org.pushingpixels.radiance.swing.ktx.addDelayedWindowListener
import java.awt.BorderLayout
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
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
            jButtonWeiter.text = UiMessages.text("download.resume.overwrite")
            if (!datenDownload.canBuildInvocation()) {
                jPanelNewName.isVisible = false
            }
        }

        jPanelPath.layout = BorderLayout(0, 0)
        jPanelPath.add(downloadZielPanel, BorderLayout.CENTER)

        parent?.let { setLocationRelativeTo(it) }

        jLabel1.text =
            UiMessages.format("download.resume.question", datenDownload.title)

        jButtonNeuerName.addActionListener {
            isNewName = downloadZielPanel.applyTargetSelection()
            if (!direkterDownload && !isNewName) {
                JOptionPane.showMessageDialog(
                    parent,
                    UiMessages.text("download.resume.unchangedName"),
                    UiMessages.text("download.resume.exists"),
                    JOptionPane.ERROR_MESSAGE,
                )
            } else {
                result = DownloadResult.RESTART_WITH_NEW_NAME
                close()
            }
        }

        jButtonAbbrechen.addActionListener { cancel() }

        EscapeKeyHandler.installHandler(this) { cancel() }

        addDelayedWindowListener(onWindowClosing = { cancel() })

        jButtonWeiter.addActionListener {
            result = DownloadResult.CONTINUE
            close()
        }

        countdownTimer.start()

        pack()

        rootPane.defaultButton = jButtonWeiter

        addDelayedComponentListener(
            onComponentShown = {
                // Display dialog slightly lower than normal to prevent inadvertent presses from other dialogs (#686).
                val location = locationOnScreen
                setLocation(location.x, location.y + 70)
        }
        )
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
                    UiMessages.format("download.resume.countdown", countdown)
                } else {
                    UiMessages.format("download.resume.overwriteCountdown", countdown)
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
