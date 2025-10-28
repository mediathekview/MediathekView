package mediathek.gui.actions

import mediathek.gui.abo.ManageAboDialog
import mediathek.mainwindow.MediathekGui
import mediathek.swing.IconUtils
import org.kordamp.ikonli.materialdesign2.MaterialDesignD
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
        putValue(SMALL_ICON, IconUtils.windowBarSpecificToolbarIcon(MaterialDesignD.DATABASE))
        putValue(SHORT_DESCRIPTION, "Abos verwalten")
    }
}