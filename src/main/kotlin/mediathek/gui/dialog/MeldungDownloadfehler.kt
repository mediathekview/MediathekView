package mediathek.gui.dialog

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.cancel
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.coroutines.swing.Swing
import mediathek.config.Konstanten
import mediathek.config.MVConfig
import mediathek.daten.DatenDownload
import mediathek.tool.EscapeKeyHandler
import java.awt.Frame
import kotlin.time.Duration.Companion.seconds

class MeldungDownloadfehler(
    parent: Frame?,
    text: String?,
    datenDownload: DatenDownload,
) : MeldungDownloadfehlerBase(parent) {
    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)

    init {
        focusableWindowState = false
        isFocusable = false
        if (parent != null) {
            setLocationRelativeTo(parent)
        }

        EscapeKeyHandler.installHandler(this) { dispose() }

        jTextArea1.text = text
        jTextFieldTitel.text = datenDownload.title
        jButtonOk.addActionListener { dispose() }

        startCountdown()
        pack()
    }

    override fun dispose() {
        uiScope.cancel()
        super.dispose()
    }

    override fun setVisible(visible: Boolean) {
        if (MVConfig.get(MVConfig.Configs.SYSTEM_DOWNLOAD_ERRORMSG).toBoolean()) {
            super.setVisible(visible)
        } else {
            dispose()
        }
    }

    private fun startCountdown() {
        uiScope.launch {
            for (secondsRemaining in Konstanten.DOWNLOAD_ERROR_DISPLAY_DURATION downTo 1) {
                jLabelTime.text = "$secondsRemaining s"
                delay(1.seconds)
            }
            dispose()
        }
    }
}
