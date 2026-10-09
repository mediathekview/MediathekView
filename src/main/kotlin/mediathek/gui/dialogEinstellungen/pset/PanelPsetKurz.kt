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

package mediathek.gui.dialogEinstellungen.pset

import mediathek.daten.DatenProg
import mediathek.daten.DatenPset
import mediathek.daten.ListePset
import mediathek.gui.messages.ProgramSetChangedEvent
import mediathek.tool.MessageBus
import mediathek.tool.SVGIconUtilities
import mediathek.tool.TextCopyPasteHandler
import net.engio.mbassy.listener.Handler
import org.apache.commons.lang3.SystemUtils
import java.awt.*
import java.io.File
import javax.swing.*
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener
import javax.swing.text.JTextComponent

class PanelPsetKurz(
    private val parentComponent: JFrame?,
    private val listePset: ListePset,
) : PanelPsetKurzBase() {
    private var currentProgramSet: DatenPset? = null
    private var isUpdatingUi = false

    init {
        installProgramSetList()
        installProgramSetEditors()
        MessageBus.messageBus.subscribe(this)
    }

    @Handler
    private fun handleProgramSetChanged(@Suppress("UNUSED_PARAMETER") event: ProgramSetChangedEvent) {
        SwingUtilities.invokeLater {
            if (!isUpdatingUi) {
                reloadProgramSetList()
            }
        }
    }

    override fun removeNotify() {
        MessageBus.messageBus.unsubscribe(this)
        super.removeNotify()
    }

    private fun installProgramSetList() {
        jListPset.addListSelectionListener {
            if (!isUpdatingUi) {
                displaySelectedProgramSet()
            }
        }
        reloadProgramSetList()
    }

    private fun installProgramSetEditors() {
        jTextFieldName.document.addDocumentListener(DuplicatePsetNameCheckListener(jTextFieldName, listePset))
        jTextFieldName.document.addDocumentListener(DocumentChangeListener(::updateProgramSetName))
        installTextPopupMenu(jTextFieldName)

        jTextFieldZiel.document.addDocumentListener(
            DocumentChangeListener {
                updateCurrentProgramSetTargetPath(jTextFieldZiel.text)
            },
        )
        installTextPopupMenu(jTextFieldZiel)

        jButtonZiel.addActionListener {
            choosePath(file = false, initialPath = jTextFieldZiel.text)?.let { selectedFile ->
                jTextFieldZiel.text = selectedFile.absolutePath
            }
        }
    }

    private fun reloadProgramSetList() {
        withoutUiUpdates {
            jListPset.model = DefaultComboBoxModel(listePset.objectDataCombo)
            if (!listePset.isEmpty()) {
                jListPset.selectedIndex = 0
            }
            displaySelectedProgramSet()
        }
    }

    private fun displaySelectedProgramSet() {
        withoutUiUpdates {
            currentProgramSet = selectedProgramSet()
            val programSet = currentProgramSet
            if (programSet == null) {
                clearProgramSetFields()
            } else {
                showProgramSet(programSet)
            }
        }
    }

    private fun selectedProgramSet(): DatenPset? =
        jListPset.selectedIndex.takeIf { it >= 0 }?.let(listePset::get)

    private fun showProgramSet(programSet: DatenPset) {
        jTextFieldName.text = programSet.name
        jTextArea1.text = programSet.getBeschreibung()
        configureTargetPathControls(programSet)
        jTextFieldZiel.text = programSet.zielPfad
        rebuildProgramPathFields(programSet)
    }

    private fun clearProgramSetFields() {
        jTextFieldName.text = ""
        jTextArea1.text = ""
        jTextFieldZiel.text = ""
        clearProgramPathFields()
    }

    private fun configureTargetPathControls(programSet: DatenPset) {
        val targetPathEditable = programSet.istSpeichern() || programSet.zielPfad.isNotEmpty()
        jTextFieldZiel.isEditable = targetPathEditable
        jButtonZiel.isEnabled = targetPathEditable
        if (targetPathEditable && programSet.zielPfad.isEmpty()) {
            programSet.zielPfad = SystemUtils.USER_HOME
        }
    }

    private fun rebuildProgramPathFields(programSet: DatenPset) {
        clearProgramPathFields()

        val layout = GridBagLayout()
        val constraints = programPathRowConstraints()
        jPanelExtra.layout = layout

        for (program in programSet.listeProg) {
            val panel = createProgramPathPanel(program)
            layout.setConstraints(panel, constraints)
            jPanelExtra.add(panel)
            constraints.gridy++
        }

        constraints.weighty = 10.0
        val filler = JLabel()
        layout.setConstraints(filler, constraints)
        jPanelExtra.add(filler)
        jPanelExtra.revalidate()
        jPanelExtra.repaint()
    }

    private fun clearProgramPathFields() {
        jPanelExtra.removeAll()
        jPanelExtra.revalidate()
        jPanelExtra.repaint()
    }

    private fun createProgramPathPanel(program: DatenProg): JPanel {
        val panel = JPanel(GridBagLayout()).apply {
            border = BorderFactory.createTitledBorder(
                BorderFactory.createLineBorder(Color(80, 80, 80), 1),
                program.name,
            )
        }
        val layout = panel.layout as GridBagLayout
        val constraints = programPathFieldConstraints()

        panel.addProgramPathLabel(layout, constraints)
        val textField = panel.addProgramPathTextField(layout, constraints, program)
        panel.addProgramPathChooserButton(layout, constraints, textField, program)
        return panel
    }

    private fun JPanel.addProgramPathLabel(layout: GridBagLayout, constraints: GridBagConstraints) {
        constraints.gridx = 0
        constraints.weightx = 0.0
        constraints.gridwidth = 1
        JLabel(PROGRAM_PATH_LABEL).also { label ->
            layout.setConstraints(label, constraints)
            add(label)
        }
    }

    private fun JPanel.addProgramPathTextField(
        layout: GridBagLayout,
        constraints: GridBagConstraints,
        program: DatenProg,
    ): JTextField {
        constraints.gridx = 1
        constraints.weightx = 10.0
        return JTextField(program.programPath).also { textField ->
            textField.document.addDocumentListener(
                DocumentChangeListener {
                    updateProgramPath(program, textField.text)
                },
            )
            installTextPopupMenu(textField)
            layout.setConstraints(textField, constraints)
            add(textField)
        }
    }

    private fun JPanel.addProgramPathChooserButton(
        layout: GridBagLayout,
        constraints: GridBagConstraints,
        textField: JTextField,
        program: DatenProg,
    ) {
        constraints.gridx = 2
        constraints.weightx = 0.0
        JButton().also { button ->
            button.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg")
            button.toolTipText = "Programm auswählen"
            button.addActionListener {
                choosePath(file = true, initialPath = textField.text)?.let { selectedFile ->
                    textField.text = selectedFile.absolutePath
                    updateProgramPath(program, textField.text)
                }
            }
            layout.setConstraints(button, constraints)
            add(button)
            textField.preferredSize = textField.preferredSize.apply { height = button.preferredSize.height }
        }
    }

    private fun updateProgramSetName() {
        val programSet = currentProgramSet ?: return
        if (isUpdatingUi) {
            return
        }

        programSet.name = jTextFieldName.text
        val selectedIndex = jListPset.selectedIndex
        withoutUiUpdates {
            jListPset.model = DefaultComboBoxModel(listePset.objectDataCombo)
            if (selectedIndex >= 0 && selectedIndex < listePset.size) {
                jListPset.selectedIndex = selectedIndex
            }
        }
    }

    private fun updateCurrentProgramSetTargetPath(value: String) {
        if (!isUpdatingUi) {
            currentProgramSet?.set(DatenPset.PROGRAMMSET_ZIEL_PFAD, value)
        }
    }

    private fun updateProgramPath(program: DatenProg, value: String) {
        if (!isUpdatingUi) {
            program[DatenProg.PROGRAMM_PROGRAMMPFAD] = value
        }
    }

    private fun choosePath(file: Boolean, initialPath: String): File? =
        if (SystemUtils.IS_OS_MAC_OSX) {
            chooseNativePath(file, initialPath)
        } else {
            chooseSwingPath(file, initialPath)
        }

    private fun chooseNativePath(file: Boolean, initialPath: String): File? =
        withMacDirectoryDialogProperty(file) {
            FileDialog(parentComponent, CHOOSER_TITLE).apply {
                mode = FileDialog.LOAD
                initialPath.takeIf(String::isNotEmpty)?.let { directory = it }
                isVisible = true
            }.selectedFile
        }

    private fun chooseSwingPath(file: Boolean, initialPath: String): File? =
        JFileChooser().apply {
            initialPath.takeIf(String::isNotEmpty)?.let { currentDirectory = File(it) }
            fileSelectionMode = if (file) JFileChooser.FILES_ONLY else JFileChooser.DIRECTORIES_ONLY
        }.takeIf { chooser -> chooser.showOpenDialog(parentComponent) == JFileChooser.APPROVE_OPTION }
            ?.selectedFile

    private inline fun <T> withMacDirectoryDialogProperty(file: Boolean, block: () -> T): T {
        if (file) {
            return block()
        }

        val propertyName = "apple.awt.fileDialogForDirectories"
        val previousValue = System.getProperty(propertyName)
        System.setProperty(propertyName, true.toString())
        return try {
            block()
        } finally {
            if (previousValue == null) {
                System.clearProperty(propertyName)
            } else {
                System.setProperty(propertyName, previousValue)
            }
        }
    }

    private val FileDialog.selectedFile: File?
        get() = file?.let { selectedFile -> File(directory, selectedFile) }

    private fun installTextPopupMenu(textComponent: JTextComponent) {
        val handler = TextCopyPasteHandler(textComponent)
        textComponent.componentPopupMenu = handler.getPopupMenu()
    }

    private fun programPathRowConstraints(): GridBagConstraints =
        GridBagConstraints().apply {
            fill = GridBagConstraints.HORIZONTAL
            insets = PROGRAM_PATH_INSETS
            weightx = 1.0
            weighty = 0.0
            gridx = 0
            gridy = 0
        }

    private fun programPathFieldConstraints(): GridBagConstraints =
        GridBagConstraints().apply {
            fill = GridBagConstraints.HORIZONTAL
            insets = PROGRAM_PATH_INSETS
            weightx = 1.0
            weighty = 0.0
            gridy = 0
        }

    private inline fun withoutUiUpdates(block: () -> Unit) {
        if (isUpdatingUi) {
            block()
            return
        }

        isUpdatingUi = true
        try {
            block()
        } finally {
            isUpdatingUi = false
        }
    }

    private class DocumentChangeListener(
        private val onChange: () -> Unit,
    ) : DocumentListener {
        override fun insertUpdate(e: DocumentEvent) {
            onChange()
        }

        override fun removeUpdate(e: DocumentEvent) {
            onChange()
        }

        override fun changedUpdate(e: DocumentEvent) {
            onChange()
        }
    }

    companion object {
        private const val CHOOSER_TITLE = "Film speichern"
        private const val PROGRAM_PATH_LABEL = "Programmpfad: "
        private val PROGRAM_PATH_INSETS = Insets(4, 10, 4, 10)
    }
}
