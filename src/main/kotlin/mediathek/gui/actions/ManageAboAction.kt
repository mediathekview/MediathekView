package mediathek.gui.actions

import mediathek.daten.DatenPset
import mediathek.daten.ProgramSetRepository
import mediathek.daten.abo.AboServices
import mediathek.filmlisten.FilmCatalog
import mediathek.filmlisten.FilmeLaden
import mediathek.gui.abo.ManageAboDialog
import mediathek.swing.IconUtils
import org.kordamp.ikonli.materialdesign2.MaterialDesignD
import java.awt.event.ActionEvent
import java.util.function.BiConsumer
import javax.swing.AbstractAction
import javax.swing.JFrame

class ManageAboAction(
    private val parent: JFrame,
    private val programSets: ProgramSetRepository,
    private val filmCatalog: FilmCatalog,
    private val abos: AboServices,
    private val filmListLoader: FilmeLaden,
    private val programSetExporter: BiConsumer<Array<DatenPset>, String>,
) : AbstractAction() {
    private var dialog: ManageAboDialog? = null

    fun closeDialog() {
        dialog?.dispose()
    }

    override fun actionPerformed(e: ActionEvent?) {
        dialog = ManageAboDialog(parent, programSets, filmCatalog, abos, filmListLoader, programSetExporter)
        dialog!!.isVisible = true
        dialog = null
    }

    init {
        putValue(NAME, "Abos verwalten...")
        putValue(SMALL_ICON, IconUtils.windowBarSpecificToolbarIcon(MaterialDesignD.DATABASE))
        putValue(SHORT_DESCRIPTION, "Abos verwalten")
    }
}
