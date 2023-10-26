package mediathek.gui.actions

import mediathek.gui.bandwidth.BandwidthDialog
import mediathek.mainwindow.MediathekGui
import java.awt.event.ActionEvent
import java.util.*
import javax.swing.AbstractAction

class ShowBandwidthUsageAction(private val mediathekGui: MediathekGui) : AbstractAction() {
    var dialogOptional = Optional.empty<BandwidthDialog>()

    init {
        putValue(NAME, "Bandbreitennutzung")
    }

    override fun actionPerformed(e: ActionEvent?) {
        val dialog = BandwidthDialog(mediathekGui, this)
        dialog.isVisible = true
    }
}