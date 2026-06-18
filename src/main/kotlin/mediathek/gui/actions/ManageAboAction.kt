package mediathek.gui.actions

import mediathek.gui.abo.ManageAboDialog
import mediathek.mainwindow.MainWindowHandle
import mediathek.swing.IconUtils
import org.kordamp.ikonli.materialdesign2.MaterialDesignD
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class ManageAboAction(
    private val owner: MainWindowHandle,
) : AbstractAction() {
    private var dialog: ManageAboDialog? = null

    fun closeDialog() {
        dialog?.dispose()
    }

    override fun actionPerformed(e: ActionEvent?) {
        dialog = ManageAboDialog(owner.ownerFrame())
        dialog!!.isVisible = true
        dialog = null
    }

    init {
        putValue(NAME, "Abos verwalten...")
        putValue(SMALL_ICON, IconUtils.windowBarSpecificToolbarIcon(MaterialDesignD.DATABASE))
        putValue(SHORT_DESCRIPTION, "Abos verwalten")
    }
}
