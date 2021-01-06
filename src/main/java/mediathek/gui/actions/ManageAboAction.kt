package mediathek.gui.actions

import jiconfont.icons.FontAwesome
import jiconfont.swing.IconFontSwing
import mediathek.gui.abo.ManageAboDialog
import mediathek.mainwindow.MediathekGui
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class ManageAboAction : AbstractAction() {
    private var dialog: ManageAboDialog? = null

    fun closeDialog() {
        dialog?.dispose()
    }

    override fun actionPerformed(e: ActionEvent?) {
        dialog = ManageAboDialog(MediathekGui.ui())
        dialog!!.isVisible = true
        dialog = null
    }

    init {
        putValue(NAME, "Abos verwalten...")
        putValue(SMALL_ICON, IconFontSwing.buildIcon(FontAwesome.DATABASE, 16f))
    }
}