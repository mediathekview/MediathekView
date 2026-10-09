package mediathek.gui.dialogEinstellungen.pset

import ca.odell.glazedlists.swing.AdvancedTableModel
import ca.odell.glazedlists.swing.eventTableModelWithThreadProxyList
import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.audiothek.ui.table.TriStateTableRowSorter
import mediathek.config.Konstanten
import mediathek.config.application.ApplicationConfiguration
import mediathek.controller.starter.RuntimeExec
import mediathek.daten.*
import mediathek.gui.dialog.HelpTextDialog
import mediathek.gui.messages.ProgramSetChangedEvent
import mediathek.tool.*
import mediathek.tool.cellrenderer.PsetNameCellRenderer
import mediathek.tool.table.MVPsetTable
import mediathek.tool.table.MVTable
import net.engio.mbassy.listener.Handler
import org.apache.commons.lang3.SystemUtils
import java.awt.Component
import java.awt.Container
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths
import java.util.function.BiConsumer
import java.util.function.Consumer
import javax.swing.*
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener
import javax.swing.table.TableModel
import javax.swing.text.JTextComponent

class PanelPsetLang(
    private val parentComponent: JFrame?,
    private val programSets: ProgramSetRepository,
    private val listePset: ListePset,
    private val programSetExporter: BiConsumer<Array<DatenPset>, String>,
    private val replacementRules: ReplacementRules? = null,
) : PanelPsetLangBase() {
    private var neuZaehler = 0
    private val tabellePset: MVTable = MVPsetTable()
    private val tabelleProgramme = JTable()
    private val psetNameRenderer = PsetNameCellRenderer()
    private val emptyProgramList = ListeProg()
    private val panelScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private var programTableSorter: TriStateTableRowSorter<TableModel>? = null
    private var currentProgramList: ListeProg? = null
    private var programCheckJob: Job? = null
    private var stopBeob = false

    init {
        jScrollPane3.setViewportView(tabellePset)
        tabelleProgramme.autoCreateRowSorter = false
        tabelleProgramme.autoResizeMode = JTable.AUTO_RESIZE_OFF
        jScrollPane1.setViewportView(tabelleProgramme)
        initPanel()
    }

    @Handler
    private fun handleProgramSetChanged(@Suppress("UNUSED_PARAMETER") event: ProgramSetChangedEvent) {
        panelScope.launch {
            if (!stopBeob) {
                tabellePset()
            }
        }
    }

    override fun removeNotify() {
        MessageBus.messageBus.unsubscribe(this)
        programCheckJob?.cancel()
        panelScope.cancel()
        super.removeNotify()
    }

    private fun initPanel() {
        configureIcons()
        MessageBus.messageBus.subscribe(this)

        configureProgramTables()
        installProgramFieldListeners()
        installProgramFieldPopupMenus()
        disableProgramFields()
        installProgramActions()

        installProgramSetActions()
        installProgramSetDocumentListeners()
        installHelpAndCheckActions()
        installTableSelectionListeners()

        tabellePset()
        selectFirstProgramSet()
    }

    private fun configureIcons() {
        jButtonHilfe.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg")
        jButtonGruppePfad.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg")
        jButtonProgPlus.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/plus.svg")
        jButtonProgMinus.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/minus.svg")
        jButtonProgAuf.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/arrow-up.svg")
        jButtonProgAb.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/arrow-down.svg")
        jButtonProgPfad.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg")
        jButtonGruppeNeu.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/plus.svg")
        jButtonGruppeLoeschen.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/minus.svg")
        jButtonGruppeAuf.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/arrow-up.svg")
        jButtonGruppeAb.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/arrow-down.svg")

        val exclamationIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/triangle-exclamation.svg")
        jLabelMeldungAbspielen.icon = exclamationIcon
        jLabelMeldungSeichern.icon = exclamationIcon
    }

    private fun configureProgramTables() {
        tabellePset.autoResizeMode = JTable.AUTO_RESIZE_OFF
    }

    private fun installProgramFieldListeners() {
        installDocumentListener(
            BeobProgDoc(),
            jTextFieldProgPfad,
            jTextFieldProgSchalter,
            jTextFieldProgName,
            jTextFieldProgZielDateiName,
            jTextFieldProgPraefix,
            jTextFieldProgSuffix
        )
    }

    private fun installProgramFieldPopupMenus() {
        installTextPopupMenus(
            jTextFieldProgPfad,
            jTextFieldProgSchalter,
            jTextFieldProgName,
            jTextFieldProgZielDateiName,
            jTextFieldProgPraefix,
            jTextFieldProgSuffix
        )
    }

    private fun disableProgramFields() {
        setProgramFieldsEnabled(false)
    }

    private fun installProgramActions() {
        jButtonProgPfad.addActionListener { chooseProgramPath() }
        jButtonProgPlus.addActionListener { progNeueZeile(DatenProg()) }
        jButtonProgMinus.addActionListener { deleteSelectedProgramEntries() }
        jButtonProgDuplizieren.addActionListener { duplicateSelectedProgramEntry() }
        jButtonProgAuf.addActionListener { progAufAb(true) }
        jButtonProgAb.addActionListener { progAufAb(false) }

        jButtonProgPfad.isEnabled = false
        updateProgramMoveButtons(null)
        jCheckBoxRestart.addActionListener {
            updateSelectedProgramFlag(DatenProg.PROGRAMM_RESTART, jCheckBoxRestart.isSelected)
        }
        jCheckBoxRemoteDownload.addActionListener {
            updateSelectedProgramFlag(DatenProg.PROGRAMM_DOWNLOADMANAGER, jCheckBoxRemoteDownload.isSelected)
        }
    }

    private fun installProgramSetActions() {
        jButtonAbspielen.addActionListener {
            getPset()?.let { pset ->
                programSets.activateAsPlayer(pset)
                nurtabellePset()
            }
        }
        jCheckBoxSpeichern.addActionListener {
            updateSelectedProgramSet({ it.setSpeichern(jCheckBoxSpeichern.isSelected) }, true)
        }
        jCheckBoxButton.addActionListener {
            updateSelectedProgramSet({ it.setButton(jCheckBoxButton.isSelected) }, true)
        }
        jCheckBoxAbo.addActionListener {
            updateSelectedProgramSet({ it.setAbo(jCheckBoxAbo.isSelected) }, true)
        }
        jCheckBoxLaenge.addActionListener {
            updateSelectedProgramSet({ it.isLaengeBeschraenken = jCheckBoxLaenge.isSelected }, false)
        }
        jCheckBoxField.addActionListener {
            updateSelectedProgramSet({ it.isLaengeFieldBeschraenken = jCheckBoxField.isSelected }, false)
        }
        jCheckBoxThema.addActionListener {
            updateSelectedProgramSet({ it.isThemaAnlegen = jCheckBoxThema.isSelected }, false)
        }
        jSpinnerLaenge.addChangeListener {
            val value = jSpinnerLaenge.model.value
            if (value is Number) {
                getPset()?.maxLaenge = value.toInt()
            }
        }
        jSpinnerField.addChangeListener {
            val value = jSpinnerField.model.value
            if (value is Number) {
                getPset()?.maxLaengeField = value.toInt()
            }
        }
        jCheckBoxInfodatei.addActionListener {
            updateSelectedProgramSet({ it.setInfodatei(jCheckBoxInfodatei.isSelected) }, false)
        }
        jCheckBoxSubtitle.addActionListener {
            updateSelectedProgramSet({ it.setSubtitle(jCheckBoxSubtitle.isSelected) }, false)
        }
        jCheckBoxMp4Metadata.addActionListener {
            updateSelectedProgramSet({ it.setMp4Metadata(jCheckBoxMp4Metadata.isSelected) }, false)
        }

        jCheckBoxSpotlight.isEnabled = SystemUtils.IS_OS_MAC_OSX
        jCheckBoxSpotlight.addActionListener {
            updateSelectedProgramSet({ it.isSpotlight = jCheckBoxSpotlight.isSelected }, false)
        }

        jButtonGruppeNeu.addActionListener { setNeu() }
        jButtonGruppeLoeschen.addActionListener { setLoeschen() }
        jButtonGruppeFarbe.addActionListener { chooseProgramSetColor() }
        jButtonGruppeStandardfarbe.addActionListener { clearProgramSetColor() }
        jButtonGruppeAuf.addActionListener { setAufAb(true) }
        jButtonGruppeAb.addActionListener { setAufAb(false) }
        jButtonGruppeDuplizieren.addActionListener { duplicateSelectedProgramSet() }
        jButtonExport.addActionListener { setExport() }
        jButtonGruppePfad.addActionListener { chooseProgramSetTargetPath() }
    }

    private fun installProgramSetDocumentListeners() {
        jTextAreaSetBeschreibung.document.addDocumentListener(
            BeobDoc(
                jTextAreaSetBeschreibung,
                DatenPset.PROGRAMMSET_BESCHREIBUNG
            )
        )
        installTextPopupMenus(jTextAreaSetBeschreibung)

        tfGruppeDirektSuffix.document.addDocumentListener(
            BeobDoc(
                tfGruppeDirektSuffix,
                DatenPset.PROGRAMMSET_SUFFIX_DIREKT,
                false
            )
        )
        tfGruppeDirektPraefix.document.addDocumentListener(
            BeobDoc(
                tfGruppeDirektPraefix,
                DatenPset.PROGRAMMSET_PRAEFIX_DIREKT,
                false
            )
        )
        tfGruppeZielName.document.addDocumentListener(
            BeobDoc(
                tfGruppeZielName,
                DatenPset.PROGRAMMSET_ZIEL_DATEINAME,
                false
            )
        )
        tfGruppeZielPfad.document.addDocumentListener(BeobDoc(tfGruppeZielPfad, DatenPset.PROGRAMMSET_ZIEL_PFAD, false))

        jTextFieldSetName.document.addDocumentListener(DuplicatePsetNameCheckListener(jTextFieldSetName, listePset))
        jTextFieldSetName.document.addDocumentListener(BeobDoc(jTextFieldSetName, DatenPset.PROGRAMMSET_NAME))

        installTextPopupMenus(
            jTextFieldSetName,
            tfGruppeDirektSuffix,
            tfGruppeDirektPraefix,
            tfGruppeZielName,
            tfGruppeZielPfad
        )
    }

    private fun installDocumentListener(documentListener: DocumentListener, vararg textComponents: JTextComponent) {
        textComponents.forEach { it.document.addDocumentListener(documentListener) }
    }

    private fun installTextPopupMenus(vararg textComponents: JTextComponent) {
        textComponents.forEach { textComponent ->
            val handler = TextCopyPasteHandler(textComponent)
            textComponent.componentPopupMenu = handler.getPopupMenu()
        }
    }

    private fun installHelpAndCheckActions() {
        jButtonHilfe.addActionListener {
            val str = GetFile.getHilfeSuchen(Konstanten.PFAD_HILFETEXT_PRGRAMME).trim()
            HelpTextDialog.show(this, str)
        }
        jRadioButtonAufloesungKlein.addActionListener { setAufloesung() }
        jRadioButtonAufloesungNormal.addActionListener { setAufloesung() }
        jRadioButtonAufloesungHD.addActionListener { setAufloesung() }
        jButtonPruefen.addActionListener { programmePruefen() }
    }

    private fun installTableSelectionListeners() {
        tabelleProgramme.selectionModel.addListSelectionListener { event ->
            if (!event.valueIsAdjusting && !stopBeob) {
                fillTextProgramme()
            }
        }
        tabellePset.selectionModel.addListSelectionListener { event ->
            if (!stopBeob && !event.valueIsAdjusting) {
                tabelleProgramme()
                val row = tabellePset.selectedRow
                if (row != -1) {
                    val modelRow = tabellePset.convertRowIndexToModel(row)
                    val datenPset = listePset[modelRow]
                    tabellePset.model.setValueAt(jTextFieldSetName.text, modelRow, DatenPset.PROGRAMMSET_NAME)
                    jTabbedPane.setTitleAt(0, "Set Name: ${datenPset.name}")
                }
            }
        }
    }

    private fun selectFirstProgramSet() {
        if (tabellePset.rowCount > 0) {
            tabellePset.setRowSelectionInterval(0, 0)
            tabellePset.scrollRectToVisible(tabellePset.getCellRect(0, 0, false))
        }
    }

    private fun chooseProgramPath() {
        val initialFile = jTextFieldProgPfad.text.takeIf { it.isNotEmpty() }.orEmpty()
        val destFile = FileDialogs.chooseLoadFileLocation(parentFrame(), "Programm auswählen", initialFile)
        if (destFile != null) {
            jTextFieldProgPfad.text = destFile.absolutePath
        }
    }

    private fun deleteSelectedProgramEntries() {
        val modelRows = getSelectedProgramModelRows()
        if (modelRows.isEmpty()) {
            NoSelectionErrorDialog.show(this)
            return
        }

        val programList = getCurrentProgramList()
        val text = if (modelRows.size == 1) {
            val modelRow = modelRows[0]
            val prog = programList[modelRow]
            if (isEmptyProgramEntry(prog)) {
                programList.removeEntryAtIndex(modelRow)
                tabelleProgramme()
                return
            }
            prog.name
        } else {
            "${modelRows.size} Programme löschen?"
        }

        val ret = JOptionPane.showConfirmDialog(parentComponent, text, "Löschen?", JOptionPane.YES_NO_OPTION)
        if (ret == JOptionPane.OK_OPTION) {
            val progsToDelete = modelRows.map { programList[it] }
            programList.removeAllEntries(progsToDelete)
            tabelleProgramme()
        }
    }

    private fun getSelectedProgramModelRows(): IntArray =
        tabelleProgramme.selectedRows.map { tabelleProgramme.convertRowIndexToModel(it) }.toIntArray()

    private fun duplicateSelectedProgramEntry() {
        val rows = tabelleProgramme.selectedRow
        if (rows != -1) {
            val row = tabelleProgramme.convertRowIndexToModel(rows)
            val prog = getCurrentProgramList()[row]
            progNeueZeile(prog.copy())
        } else {
            NoSelectionErrorDialog.show(this)
        }
    }

    private fun updateSelectedProgramFlag(dataIndex: Int, selected: Boolean) {
        if (stopBeob) return

        val rows = tabelleProgramme.selectedRow
        if (rows != -1) {
            val modelIndex = tabelleProgramme.convertRowIndexToModel(rows)
            val listeProg = getCurrentProgramList()
            val prog = listeProg[modelIndex]
            if (dataIndex == DatenProg.PROGRAMM_RESTART) {
                prog.isRestart = selected
            } else if (dataIndex == DatenProg.PROGRAMM_DOWNLOADMANAGER) {
                prog.isDownloadManager = selected
            }
            listeProg.fireEntryChanged(modelIndex)
            updateProgramMoveButtons(prog)
        }
    }

    private fun chooseProgramSetColor() {
        getPset()?.let { pSet ->
            val selectedColor = JColorChooser.showDialog(this, "Farbe auswählen", pSet.farbe)
            if (selectedColor != null) {
                pSet.farbe = selectedColor
                tabellePset()
                notifyProgramSetChanged()
            }
        }
    }

    private fun clearProgramSetColor() {
        getPset()?.let { pSet ->
            pSet.clearFarbe()
            tabellePset()
            notifyProgramSetChanged()
        }
    }

    private fun duplicateSelectedProgramSet() {
        val row = tabellePset.selectedRow
        if (row != -1) {
            val gruppe = listePset[tabellePset.convertRowIndexToModel(row)]
            programSets.addProgramSet(gruppe.copy())
            tabellePset()
        } else {
            NoSelectionErrorDialog.show(this)
        }
    }

    private fun chooseProgramSetTargetPath() {
        val initialFile = tfGruppeZielPfad.text.takeIf { it.isNotEmpty() }.orEmpty()
        val destDirectory = FileDialogs.chooseDirectoryLocation(parentFrame(), "Filme speichern unter", initialFile)
        if (destDirectory != null) {
            tfGruppeZielPfad.text = destDirectory.absolutePath
        }
    }

    private fun updateSelectedProgramSet(update: Consumer<DatenPset>, notifyChange: Boolean) {
        getPset()?.let { pset ->
            update.accept(pset)
            nurtabellePset()
            if (notifyChange) {
                notifyProgramSetChanged()
            }
        }
    }

    fun programmePruefen() {
        programCheckJob?.cancel()
        programCheckJob = panelScope.launch {
            val inputs = createProgramCheckInputs()
            jButtonPruefen.isEnabled = false
            try {
                val result = withContext(Dispatchers.IO) { checkProgramSets(inputs) }
                showProgramCheckResult(result)
            } finally {
                jButtonPruefen.isEnabled = true
                programCheckJob = null
            }
        }
    }

    private fun createProgramCheckInputs(): List<ProgramSetCheckInput> =
        listePset.asSequence()
            .filterNot(DatenPset::isFreeLine)
            .filterNot(DatenPset::isLabel)
            .map { pset ->
                ProgramSetCheckInput(
                    name = pset.name,
                    targetPath = pset.zielPfad,
                    containsProgramPath = pset.progsContainPath(),
                    programs = pset.listeProg.map { prog ->
                        ProgramCheckInput(
                            name = prog.name,
                            path = prog.programPath
                        )
                    }
                )
            }
            .toList()

    private fun checkProgramSets(inputs: List<ProgramSetCheckInput>): ProgramCheckResult {
        val results = inputs.asSequence()
            .map(::checkProgramSet)
            .toList()
        val allValid = results.all(ProgramSetCheck::isValid)
        return ProgramCheckResult(allValid, results.joinToString(separator = "") { it.details })
    }

    private fun checkProgramSet(programSet: ProgramSetCheckInput): ProgramSetCheck {
        val details = StringBuilder()
        var valid = true

        details.append(SEPARATOR).append('\n')
        details.append(PIPE).append("Programmgruppe: ").append(programSet.name).append('\n')

        if (programSet.containsProgramPath && !checkTargetPath(programSet.targetPath, details)) {
            valid = false
        }

        for (program in programSet.programs) {
            if (!checkProgramPath(program, details)) {
                valid = false
            }
        }

        if (valid) {
            details.append(PIPE).append(ARROW).append("Ok!\n")
        }
        details.append(SEPARATOR).append("\n\n\n")

        return ProgramSetCheck(valid, details.toString())
    }

    private fun checkTargetPath(zielPfad: String, details: StringBuilder): Boolean {
        if (zielPfad.isEmpty()) {
            details.append(PIPE).append(INDENT).append("Zielpfad fehlt!\n")
            return false
        }
        if (!GuiFunktionenProgramme.checkPathWriteable(zielPfad)) {
            details.append(PIPE).append(INDENT).append("Falscher Zielpfad!\n")
            details.append(PIPE).append(INDENT).append(ARROW).append("Zielpfad \"")
                .append(zielPfad).append("\" nicht beschreibbar!\n")
            return false
        }
        return true
    }

    private fun checkProgramPath(program: ProgramCheckInput, details: StringBuilder): Boolean {
        val programPath = program.path
        val programName = program.name
        if (programPath.isEmpty()) {
            appendProgramPathError(details, "Kein Programm angegeben!", programName, programPath)
            return false
        }
        if (Files.isExecutable(Paths.get(programPath))) {
            return true
        }

        val process = RuntimeExec(programPath).exec(false)
        if (process != null) {
            process.destroy()
            return true
        }

        appendProgramPathError(details, "Falscher Programmpfad!", programName, programPath)
        if (!programPath.contains(File.separator)) {
            details.append(PIPE).append(INDENT).append(ARROW)
                .append("Wenn das Programm nicht im Systempfad liegt, \n")
            details.append(PIPE).append(INDENT).append(INDENT).append("wird der Start nicht klappen!\n")
        }
        return false
    }

    private fun appendProgramPathError(
        details: StringBuilder,
        message: String,
        programName: String,
        programPath: String
    ) {
        details.append(PIPE).append(INDENT).append(message).append('\n')
        details.append(PIPE).append(INDENT).append(ARROW).append("Programmname: ").append(programName).append('\n')
        details.append(PIPE).append(INDENT).append(INDENT).append("Pfad: ").append(programPath).append('\n')
    }

    private fun showProgramCheckResult(result: ProgramCheckResult) {
        if (result.allValid) {
            JOptionPane.showMessageDialog(
                this,
                "Alle Programm-Sets sind in Ordnung.",
                Konstanten.PROGRAMMNAME,
                JOptionPane.INFORMATION_MESSAGE
            )
        } else {
            JOptionPane.showMessageDialog(this, result.details, Konstanten.PROGRAMMNAME, JOptionPane.WARNING_MESSAGE)
        }
    }

    private fun setAufloesung() {
        getPset()?.let { pset ->
            if (jRadioButtonAufloesungNormal.isSelected) {
                pset.aufloesung = FilmResolution.Enum.NORMAL
            } else if (jRadioButtonAufloesungHD.isSelected) {
                pset.aufloesung = FilmResolution.Enum.HIGH_QUALITY
            } else if (jRadioButtonAufloesungKlein.isSelected) {
                pset.aufloesung = FilmResolution.Enum.LOW
            }
        }
    }

    private fun tabellePset() {
        nurtabellePset()
        tabelleProgramme()
    }

    private fun nurtabellePset() {
        stopBeob = true
        tabellePset.getSpalten()
        tabellePset.model = listePset.createModel()
        tabellePset.setSpalten()
        spaltenSetzen()
        jLabelMeldungAbspielen.isVisible = listePset.psetAbspielen == null
        jLabelMeldungSeichern.isVisible = listePset.listeSpeichern.isEmpty()
        stopBeob = false
    }

    private fun spaltenSetzen() {
        val columnModel = tabellePset.columnModel
        for (i in 0 until tabellePset.columnCount) {
            val column = columnModel.getColumn(tabellePset.convertColumnIndexToView(i))
            when (i) {
                DatenPset.PROGRAMMSET_NAME -> {
                    column.cellRenderer = psetNameRenderer
                    column.minWidth = 10
                    column.preferredWidth = 120
                    column.maxWidth = 1000
                }

                DatenPset.PROGRAMMSET_IST_ABSPIELEN,
                DatenPset.PROGRAMMSET_IST_SPEICHERN -> {
                    column.minWidth = 10
                    column.preferredWidth = 80
                    column.maxWidth = 1000
                }

                else -> {
                    column.minWidth = 0
                    column.preferredWidth = 0
                    column.maxWidth = 0
                }
            }
        }
    }

    private fun tabelleProgramme() {
        val pSet = getPset()
        stopBeob = true

        enableComponents(jTabbedPane, pSet != null)
        jButtonAbspielen.background = null
        if (pSet != null) {
            jTabbedPane.setTitleAt(0, "Set Name: ${pSet.name}")
            if (pSet.maxLaenge == null) {
                jSpinnerLaenge.value = Konstanten.LAENGE_DATEINAME
                pSet.maxLaenge = Konstanten.LAENGE_DATEINAME
            } else {
                jSpinnerLaenge.value = pSet.maxLaenge
            }
            if (pSet.maxLaengeField == null) {
                jSpinnerField.value = Konstanten.LAENGE_FELD
                pSet.maxLaengeField = Konstanten.LAENGE_FELD
            } else {
                jSpinnerField.value = pSet.maxLaengeField
            }
            jCheckBoxLaenge.isSelected = pSet.isLaengeBeschraenken
            jCheckBoxField.isSelected = pSet.isLaengeFieldBeschraenken
            jCheckBoxThema.isSelected = pSet.isThemaAnlegen
            jCheckBoxInfodatei.isSelected = pSet.shouldCreateInfofile()
            jCheckBoxSubtitle.isSelected = pSet.shouldDownloadSubtitle()
            jCheckBoxMp4Metadata.isSelected = pSet.shouldWriteMp4Metadata()
            jCheckBoxSpotlight.isEnabled = SystemUtils.IS_OS_MAC_OSX
            jCheckBoxSpotlight.isSelected = pSet.isSpotlight
            jScrollPane1.border = BorderFactory.createTitledBorder(
                null,
                "Set Name: ${pSet.name}",
                javax.swing.border.TitledBorder.LEFT,
                javax.swing.border.TitledBorder.TOP
            )
            jTextFieldSetName.text = pSet.name
            tfGruppeDirektSuffix.text = pSet.getSuffixDirekt()
            tfGruppeDirektPraefix.text = pSet.getPraefixDirekt()
            tfGruppeZielName.text = pSet.zielDateiname
            tfGruppeZielPfad.text = pSet.zielPfad
            jTextAreaSetBeschreibung.text = pSet.getBeschreibung()

            jCheckBoxSpeichern.isSelected = pSet.istSpeichern()
            jCheckBoxButton.isSelected = pSet.istButton()
            jCheckBoxAbo.isSelected = pSet.istAbo()
            when (pSet.aufloesung ?: FilmResolution.Enum.NORMAL) {
                FilmResolution.Enum.HIGH_QUALITY -> jRadioButtonAufloesungHD.isSelected = true
                FilmResolution.Enum.LOW -> jRadioButtonAufloesungKlein.isSelected = true
                else -> jRadioButtonAufloesungNormal.isSelected = true
            }
            bindProgramTableModel(pSet.listeProg)
            if (tabelleProgramme.rowCount > 0) {
                spaltenSetzenProgramme()
                tabelleProgramme.setRowSelectionInterval(0, 0)
                tabelleProgramme.scrollRectToVisible(tabelleProgramme.getCellRect(0, 0, true))
            }
        } else {
            jScrollPane1.border = BorderFactory.createTitledBorder(
                null,
                "",
                javax.swing.border.TitledBorder.LEFT,
                javax.swing.border.TitledBorder.TOP
            )
            jTabbedPane.setTitleAt(0, "Sets")
            jCheckBoxLaenge.isSelected = false
            jCheckBoxThema.isSelected = false
            jCheckBoxInfodatei.isSelected = false
            jCheckBoxSubtitle.isSelected = false
            jCheckBoxMp4Metadata.isSelected = false
            jCheckBoxSpotlight.isSelected = false
            jTextFieldSetName.text = ""
            tfGruppeDirektSuffix.text = ""
            tfGruppeDirektPraefix.text = ""
            tfGruppeZielName.text = ""
            tfGruppeZielPfad.text = ""
            jTextAreaSetBeschreibung.text = ""
            bindProgramTableModel(emptyProgramList)
        }
        stopBeob = false
        fillTextProgramme()
    }

    private fun enableComponents(container: Container, enable: Boolean) {
        for (component: Component in container.components) {
            component.isEnabled = enable
            if (component is Container) {
                enableComponents(component, enable)
            }
        }
    }

    private fun bindProgramTableModel(listeProg: ListeProg) {
        if (currentProgramList === listeProg) return
        val oldModel = tabelleProgramme.model as? AdvancedTableModel<*>
        val newModel = listeProg.eventTableModelWithThreadProxyList(PROGRAM_TABLE_FORMAT)
        tabelleProgramme.rowSorter = null
        tabelleProgramme.model = newModel
        currentProgramList = listeProg
        val sorter = programTableSorter
        if (sorter == null) {
            programTableSorter = TriStateTableRowSorter<TableModel>(newModel).also {
                it.addRowSorterListener { updateProgramMoveButtonsForSelection() }
            }
        } else {
            sorter.model = newModel
        }
        programTableSorter?.sortKeys = emptyList()
        tabelleProgramme.rowSorter = programTableSorter
        updateProgramMoveButtonsForSelection()
        oldModel?.dispose()
    }

    fun spaltenSetzenProgramme() {
        for (i in 0 until tabelleProgramme.columnCount) {
            val column = tabelleProgramme.columnModel.getColumn(tabelleProgramme.convertColumnIndexToView(i))
            if (i == DatenProg.PROGRAMM_PRAEFIX ||
                i == DatenProg.PROGRAMM_RESTART ||
                i == DatenProg.PROGRAMM_DOWNLOADMANAGER ||
                i == DatenProg.PROGRAMM_SUFFIX
            ) {
                column.minWidth = 10
                column.maxWidth = 3000
                column.preferredWidth = 75
            } else {
                column.minWidth = 10
                column.maxWidth = 3000
                column.preferredWidth = 150
            }
        }
    }

    private fun notifyProgramSetChanged() {
        programSets.notifyChanged()
    }

    private fun fillTextProgramme() {
        stopBeob = true
        val row = tabelleProgramme.selectedRow
        val validRowSelected = row != -1
        val modelRow = if (validRowSelected) tabelleProgramme.convertRowIndexToModel(row) else -1
        val modelRowCount = tabelleProgramme.model.rowCount
        val letzteZeile = modelRowCount <= 1 || modelRow == modelRowCount - 1

        setProgramFieldsEnabled(validRowSelected)
        jButtonProgPfad.isEnabled = validRowSelected
        jCheckBoxRestart.isEnabled = validRowSelected
        jCheckBoxRemoteDownload.isEnabled = validRowSelected
        if (validRowSelected) {
            val prog = getCurrentProgramList()[modelRow]
            updateProgramMoveButtons(prog)
            fillProgramFields(prog)
            jCheckBoxRestart.isSelected = prog.isRestart
            jCheckBoxRemoteDownload.isSelected = prog.isDownloadManager
        } else {
            clearProgramFields()
            updateProgramMoveButtons(null)
        }
        if (letzteZeile) {
            jTextFieldProgPraefix.isEnabled = false
            jTextFieldProgSuffix.isEnabled = false
        }
        stopBeob = false
    }

    private fun setProgramFieldsEnabled(enabled: Boolean) {
        jTextFieldProgPfad.isEnabled = enabled
        jTextFieldProgSchalter.isEnabled = enabled
        jTextFieldProgZielDateiName.isEnabled = enabled
        jTextFieldProgName.isEnabled = enabled
        jTextFieldProgPraefix.isEnabled = enabled
        jTextFieldProgSuffix.isEnabled = enabled
    }

    private fun fillProgramFields(prog: DatenProg) {
        jTextFieldProgPfad.text = prog.programPath
        jTextFieldProgSchalter.text = prog.switches
        jTextFieldProgZielDateiName.text = prog.targetFileName
        jTextFieldProgName.text = prog.name
        jTextFieldProgPraefix.text = prog.prefix
        jTextFieldProgSuffix.text = prog.suffix
    }

    private fun clearProgramFields() {
        jTextFieldProgPfad.text = ""
        jTextFieldProgSchalter.text = ""
        jTextFieldProgZielDateiName.text = ""
        jTextFieldProgName.text = ""
        jTextFieldProgPraefix.text = ""
        jTextFieldProgSuffix.text = ""
    }

    private fun getPset(): DatenPset? {
        val row = tabellePset.selectedRow
        return if (row == -1) null else listePset[tabellePset.convertRowIndexToModel(row)]
    }

    private fun getCurrentProgramList(): ListeProg = checkNotNull(currentProgramList) { "program table model" }

    private fun setAufAb(auf: Boolean) {
        val row = tabellePset.selectedRow
        if (row != -1) {
            var neu = programSets.move(tabellePset.convertRowIndexToModel(row), auf)
            neu = tabellePset.convertRowIndexToView(neu)
            tabellePset.setRowSelectionInterval(neu, neu)
            tabellePset.scrollRectToVisible(tabellePset.getCellRect(neu, 0, false))
        } else {
            NoSelectionErrorDialog.show(this)
        }
    }

    private fun setNeu() {
        programSets.addProgramSet(DatenPset("Neu-${++neuZaehler}"))
        tabellePset()
    }

    private fun setLoeschen() {
        val rows = tabellePset.selectedRows
        if (rows.isNotEmpty()) {
            val text = if (rows.size == 1) {
                listePset[tabellePset.convertRowIndexToModel(rows[0])].name
            } else {
                "${rows.size} Set löschen?"
            }
            val ret = JOptionPane.showConfirmDialog(parentComponent, text, "Löschen?", JOptionPane.YES_NO_OPTION)
            if (ret == JOptionPane.OK_OPTION) {
                val modelRows = rows.map { tabellePset.convertRowIndexToModel(it) }.toIntArray()
                programSets.removeAtIndexes(modelRows)
                tabellePset()
            }
        } else {
            NoSelectionErrorDialog.show(this)
        }
    }

    private fun setExport() {
        val rows = tabellePset.selectedRows
        if (rows.isNotEmpty()) {
            val liste = rows.map { tabellePset.convertRowIndexToModel(it) }.map { listePset[it] }
            val entryName = liste.first().name
            val name = if (entryName.isEmpty()) "Name.xml" else "$entryName.xml"
            val applicationConfiguration = ApplicationConfiguration.getInstance()
            val fileName = FilenameUtils.replaceEmptyFilename(
                name,
                false,
                replacementRules.takeIf { applicationConfiguration.useFilenameReplaceTable },
                applicationConfiguration.onlyAsciiFilenames
            )
            val resultFile = FileDialogs.chooseSaveFileLocation(parentFrame(), "PSet exportieren", fileName)
            if (resultFile != null) {
                val ziel = resultFile.absolutePath
                programSetExporter.accept(liste.toTypedArray(), ziel)
                JOptionPane.showMessageDialog(
                    this,
                    "Das Programmset wurde erfolgreich exportiert.",
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.INFORMATION_MESSAGE
                )
            }
        } else {
            NoSelectionErrorDialog.show(this)
        }
    }

    private fun isEmptyProgramEntry(prog: DatenProg): Boolean {
        for (i in 0 until DatenProg.PROGRAMM_RESTART) {
            val value = prog[i]
            if (value.isNotBlank()) {
                return false
            }
        }
        return !prog.isRestart && !prog.isDownloadManager
    }

    private fun updateProgramMoveButtons(prog: DatenProg?) {
        val enabled = prog != null && !isEmptyProgramEntry(prog) && !isProgramTableSorted()
        jButtonProgAuf.isEnabled = enabled
        jButtonProgAb.isEnabled = enabled
    }

    private fun updateProgramMoveButtonsForSelection() {
        updateProgramMoveButtons(getSelectedProgramEntry())
    }

    private fun getSelectedProgramEntry(): DatenProg? {
        val programList = currentProgramList ?: return null
        val viewRow = tabelleProgramme.selectedRow
        if (viewRow == -1) return null
        val modelRow = tabelleProgramme.convertRowIndexToModel(viewRow)
        if (modelRow < 0 || modelRow >= programList.size) return null
        return programList[modelRow]
    }

    private fun isProgramTableSorted(): Boolean = programTableSorter?.sortKeys?.isNotEmpty() == true

    private fun progNeueZeile(prog: DatenProg) {
        val gruppe = getPset()
        if (gruppe != null) {
            val newRow = gruppe.listeProg.size
            gruppe.addProg(prog)
            tabelleProgramme()
            selectProgramModelRow(newRow)
        }
    }

    private fun selectProgramModelRow(modelRow: Int) {
        val viewRow = tabelleProgramme.convertRowIndexToView(modelRow)
        if (viewRow != -1) {
            tabelleProgramme.setRowSelectionInterval(viewRow, viewRow)
            tabelleProgramme.scrollRectToVisible(tabelleProgramme.getCellRect(viewRow, 0, true))
        }
    }

    private fun progAufAb(auf: Boolean) {
        if (isProgramTableSorted()) return
        val rows = tabelleProgramme.selectedRow
        if (rows != -1) {
            val row = tabelleProgramme.convertRowIndexToModel(rows)
            val neu = getCurrentProgramList().moveEntryAtIndex(row, auf)
            tabelleProgramme()
            selectProgramModelRow(neu)
        } else {
            NoSelectionErrorDialog.show(this)
        }
    }

    private inner class BeobProgDoc : DocumentListener {
        override fun insertUpdate(e: DocumentEvent) = eingabe()
        override fun removeUpdate(e: DocumentEvent) = eingabe()
        override fun changedUpdate(e: DocumentEvent) = eingabe()

        private fun eingabe() {
            if (!stopBeob) {
                val rows = tabelleProgramme.selectedRow
                if (rows != -1) {
                    val row = tabelleProgramme.convertRowIndexToModel(rows)
                    val listeProg = getCurrentProgramList()
                    val prog = listeProg[row]
                    prog.programPath = jTextFieldProgPfad.text
                    prog.switches = jTextFieldProgSchalter.text
                    prog.name = jTextFieldProgName.text
                    prog.targetFileName = jTextFieldProgZielDateiName.text
                    prog.suffix = jTextFieldProgSuffix.text
                    prog.prefix = jTextFieldProgPraefix.text
                    listeProg.fireEntryChanged(row)
                    updateProgramMoveButtons(prog)
                }
            }
        }
    }

    private inner class BeobDoc(
        private val textComponent: JTextComponent,
        private val psetIndex: Int,
        private val fireUpdate: Boolean = true
    ) : DocumentListener {
        override fun insertUpdate(e: DocumentEvent) = eingabe()
        override fun removeUpdate(e: DocumentEvent) = eingabe()
        override fun changedUpdate(e: DocumentEvent) {
            // unused in plaintext components
        }

        private fun eingabe() {
            if (!stopBeob) {
                val row = tabellePset.selectedRow
                if (row != -1) {
                    stopBeob = true
                    val modelIndex = tabellePset.convertRowIndexToModel(row)
                    val datenPset = listePset[modelIndex]
                    datenPset[psetIndex] = textComponent.text
                    if (psetIndex == DatenPset.PROGRAMMSET_NAME) {
                        tabellePset.model.setValueAt(jTextFieldSetName.text, modelIndex, DatenPset.PROGRAMMSET_NAME)
                        jTabbedPane.setTitleAt(0, "Set Name: ${datenPset.name}")
                    }
                    if (fireUpdate) {
                        notifyProgramSetChanged()
                    }
                    stopBeob = false
                } else {
                    NoSelectionErrorDialog.show(null)
                }
            }
        }
    }

    private data class ProgramSetCheckInput(
        val name: String,
        val targetPath: String,
        val containsProgramPath: Boolean,
        val programs: List<ProgramCheckInput>
    )

    private data class ProgramCheckInput(
        val name: String,
        val path: String
    )

    private data class ProgramSetCheck(
        val isValid: Boolean,
        val details: String
    )

    private data class ProgramCheckResult(
        val allValid: Boolean,
        val details: String
    )

    private fun parentFrame() = parentComponent ?: JOptionPane.getFrameForComponent(this)

    companion object {
        private const val PIPE = "| "
        private const val INDENT = "      "
        private const val ARROW = " -> "
        private const val SEPARATOR = "++++++++++++++++++++++++++++++++++++++++++++"
        private val PROGRAM_TABLE_FORMAT = ProgramTableFormat()
    }
}
