/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.dialogEinstellungen

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.CommandLineOptions
import mediathek.daten.*
import mediathek.swing.IconUtils
import mediathek.tool.GuiFunktionenProgramme
import mediathek.tool.SVGIconUtilities
import mediathek.tool.TextCopyPasteHandler
import mediathek.tool.models.NonEditableTableModel
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import java.awt.Color
import java.awt.Cursor
import java.awt.FileDialog
import java.io.File
import java.util.function.BiConsumer
import javax.swing.DefaultComboBoxModel
import javax.swing.JFileChooser
import javax.swing.JFrame
import javax.swing.UIManager
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener
import javax.swing.event.ListSelectionEvent
import javax.swing.text.Document
import kotlin.time.Duration.Companion.milliseconds

class PanelPsetImport(
    private val programSets: ProgramSetRepository,
    private val programSetExporter: BiConsumer<Array<DatenPset>, String>,
    private val parentComponent: JFrame?,
) : PanelPsetImportBase() {
    private val listePsetVorlagen = ListePsetVorlagen()
    private val panelScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private var templateRefreshJob: Job? = null
    private var fileValidationJob: Job? = null
    private var textValidationJob: Job? = null

    init {
        initializePanel()
    }

    override fun removeNotify() {
        templateRefreshJob?.cancel()
        fileValidationJob?.cancel()
        textValidationJob?.cancel()
        panelScope.cancel()
        super.removeNotify()
    }

    private fun initializePanel() {
        jButtonAktualisieren.icon = IconUtils.of(FontAwesomeSolid.REDO_ALT)
        jButtonPfad.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg")
        jComboBoxBs.model = DefaultComboBoxModel(ListePsetVorlagen.BS)
        jComboBoxBs.addActionListener { loadTable() }
        jButtonImportDatei.isEnabled = false
        jButtonImportText.isEnabled = false
        jButtonPfad.addActionListener { chooseProgramSetFile() }

        jTextFieldDatei.document.onChange { validatePathInput() }
        TextCopyPasteHandler(jTextFieldDatei).also { handler ->
            jTextFieldDatei.componentPopupMenu = handler.getPopupMenu()
        }

        jTextAreaImport.document.onChange { validateTextInput() }
        TextCopyPasteHandler(jTextAreaImport).also { handler ->
            jTextAreaImport.componentPopupMenu = handler.getPopupMenu()
        }

        jButtonImportVorlage.addActionListener {
            jTextFieldUrl.text.takeIf(String::isNotEmpty)?.let(::importFile)
        }
        jButtonImportDatei.addActionListener { importFile(jTextFieldDatei.text) }
        jButtonImportText.addActionListener { importText() }
        jButtonAktualisieren.addActionListener { refreshTemplates() }
        jTableVorlagen.selectionModel.addListSelectionListener { event -> handleTableSelection(event) }
        jTableVorlagen.model = NonEditableTableModel(emptyArray(), arrayOf(*ListePsetVorlagen.PGR_COLUMN_NAMES))
        hideVersionColumnUnlessDebug()
        jButtonImportStandard.addActionListener { importStandardSet() }
    }

    private fun refreshTemplates() {
        if (templateRefreshJob?.isActive == true) {
            return
        }

        templateRefreshJob = panelScope.launch {
            withWaitCursor {
                withContext(Dispatchers.IO) {
                    listePsetVorlagen.loadListOfSets()
                }
                loadTable()
            }
        }
    }

    private fun importFile(file: String) {
        panelScope.launch {
            withWaitCursor {
                val listePset = withContext(Dispatchers.IO) {
                    ListePsetVorlagen.importPsetFile(file, true)
                }
                replaceTemplates(listePset)
                addTemplateSet(listePset, setVersion = false)
            }
        }
    }

    private fun importText() {
        val text = jTextAreaImport.text
        panelScope.launch {
            val listePset = withContext(Dispatchers.Default) {
                ListePsetVorlagen.importPsetText(text, true)
            }
            replaceTemplates(listePset)
            addTemplateSet(listePset, setVersion = false)
        }
    }

    private fun importStandardSet() {
        panelScope.launch {
            withWaitCursor {
                val listePset = withContext(Dispatchers.IO) {
                    ListePsetVorlagen.getStandarset(parentComponent, false)
                }
                replaceTemplates(listePset)
                addTemplateSet(listePset, setVersion = true)
            }
        }
    }

    private fun replaceTemplates(listePset: ListePset?) {
        if (listePset != null) {
            ProgramSetTemplateResolver.replaceTemplates(parentComponent, listePset)
        }
    }

    private fun addTemplateSet(listePset: ListePset?, setVersion: Boolean): Boolean =
        GuiFunktionenProgramme.addSetVorlagen(
            parentComponent,
            programSets,
            listePset,
            setVersion,
            programSetExporter,
        )

    private fun loadTable() {
        jTableVorlagen.model = listePsetVorlagen.createModel(jComboBoxBs.selectedItem?.toString().orEmpty())
        hideVersionColumnUnlessDebug()
    }

    private fun hideVersionColumnUnlessDebug() {
        if (CommandLineOptions.isDebugModeEnabled()) {
            return
        }

        val versionColumn = jTableVorlagen.columnModel.getColumn(
            jTableVorlagen.convertColumnIndexToView(ListePsetVorlagen.PGR_VERSION_NR),
        )
        versionColumn.minWidth = 0
        versionColumn.preferredWidth = 0
        versionColumn.maxWidth = 0
    }

    private fun selectTemplateRow() {
        val template = Array(ListePsetVorlagen.PGR_MAX_ELEM) { "" }
        val selectedTableRow = jTableVorlagen.selectedRow
        if (selectedTableRow >= 0) {
            val selectedModelRow = jTableVorlagen.convertRowIndexToModel(selectedTableRow)
            val model = jTableVorlagen.model
            for (index in template.indices) {
                template[index] = model.getValueAt(selectedModelRow, index).toString()
            }
        }
        jTextFieldName.text = template[ListePsetVorlagen.PGR_NAME_NR]
        jTextFieldBs.text = template[ListePsetVorlagen.PGR_BS_NR]
        jTextFieldUrl.text = template[ListePsetVorlagen.PGR_URL_NR]
        jTextAreaBeschreibung.text = template[ListePsetVorlagen.PGR_BESCHREIBUNG_NR]
    }

    private fun validatePathInput() {
        val path = jTextFieldDatei.text
        jButtonImportDatei.isEnabled = path.isNotEmpty()
        fileValidationJob?.cancel()
        if (path.isEmpty()) {
            jTextFieldDatei.background = UIManager.getDefaults().getColor("TextField.background")
            return
        }

        fileValidationJob = panelScope.launch {
            delay(INPUT_VALIDATION_DELAY)
            val valid = withContext(Dispatchers.IO) {
                ListePsetVorlagen.importPsetFile(path, false) != null
            }
            if (jTextFieldDatei.text == path) {
                jTextFieldDatei.background = if (valid) {
                    UIManager.getDefaults().getColor("TextField.background")
                } else {
                    INVALID_INPUT_BACKGROUND
                }
            }
        }
    }

    private fun validateTextInput() {
        val text = jTextAreaImport.text
        textValidationJob?.cancel()
        if (text.isEmpty()) {
            jButtonImportText.isEnabled = false
            jTextAreaImport.background = UIManager.getDefaults().getColor("TextArea.background")
            return
        }

        jButtonImportText.isEnabled = false
        textValidationJob = panelScope.launch {
            delay(INPUT_VALIDATION_DELAY)
            val valid = withContext(Dispatchers.Default) {
                ListePsetVorlagen.importPsetText(text, false) != null
            }
            if (jTextAreaImport.text == text) {
                jTextAreaImport.background = if (valid) {
                    UIManager.getDefaults().getColor("TextArea.background")
                } else {
                    INVALID_INPUT_BACKGROUND
                }
                jButtonImportText.isEnabled = valid
            }
        }
    }

    private fun chooseProgramSetFile() {
        val selectedFile = if (SystemUtils.IS_OS_MAC_OSX) {
            chooseNativeFile()
        } else {
            chooseSwingFile()
        }

        selectedFile?.let { file ->
            try {
                jTextFieldDatei.text = file.absolutePath
            } catch (ex: Exception) {
                logger.error(ex)
            }
        }
    }

    private fun chooseNativeFile(): File? = FileDialog(parentComponent, "Programmset auswählen").apply {
        mode = FileDialog.LOAD
        isVisible = true
    }.takeIf { chooser -> chooser.file != null }
        ?.let { chooser -> File(chooser.directory + chooser.file) }

    private fun chooseSwingFile(): File? = JFileChooser().apply {
        fileSelectionMode = JFileChooser.FILES_ONLY
        isFileHidingEnabled = false
        currentDirectory = File(initialDirectory())
    }.takeIf { chooser -> chooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION }?.selectedFile

    private fun initialDirectory(): String =
        jTextFieldDatei.text.ifEmpty { SystemUtils.USER_HOME }

    private fun handleTableSelection(event: ListSelectionEvent) {
        if (!event.valueIsAdjusting) {
            selectTemplateRow()
        }
    }

    private suspend fun withWaitCursor(block: suspend () -> Unit) {
        cursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR)
        try {
            block()
        } finally {
            cursor = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR)
        }
    }

    private fun Document.onChange(action: () -> Unit) {
        addDocumentListener(object : DocumentListener {
            override fun insertUpdate(e: DocumentEvent) = action()

            override fun removeUpdate(e: DocumentEvent) = action()

            override fun changedUpdate(e: DocumentEvent) = action()
        })
    }

    private companion object {
        private val logger = LogManager.getLogger()
        private val INVALID_INPUT_BACKGROUND = Color(255, 200, 200)
        private val INPUT_VALIDATION_DELAY = 150.milliseconds
    }
}
