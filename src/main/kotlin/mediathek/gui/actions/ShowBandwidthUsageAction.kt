package mediathek.gui.actions

import mediathek.gui.bandwidth.BandwidthDialog
import java.awt.Frame
import java.awt.event.ActionEvent
import java.util.*
import javax.swing.AbstractAction
import mediathek.i18n.UiMessages

class ShowBandwidthUsageAction(private val owner: Frame) : AbstractAction() {
    var dialogOptional = Optional.empty<BandwidthDialog>()

    init {
        putValue(NAME, UiMessages.text("action.showBandwidthUsageAction.text"))
    }

    override fun actionPerformed(e: ActionEvent?) {
        val dialog = BandwidthDialog(owner, this)
        dialog.isVisible = true
    }

    fun closeBandwidthMonitorForShutdown() {
        dialogOptional.ifPresent { dialog ->
            dialog.disposeForShutdown()
        }
    }
}
