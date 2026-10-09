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

import mediathek.config.application.ApplicationConfiguration
import mediathek.tool.*
import mediathek.tool.models.NonEditableTableModel
import java.awt.Component
import javax.swing.JTextField
import javax.swing.SwingUtilities
import javax.swing.text.AbstractDocument
import javax.swing.text.AttributeSet
import javax.swing.text.DocumentFilter

class PanelDateinamen internal constructor(
    private val replacementRules: ReplacementRules,
    private val filenameSettings: FilenameSettings,
    private val addReplacementRuleDialog: (Component) -> ReplaceEntry?,
) : PanelDateinamenBase() {
    constructor(replacementRules: ReplacementRules) : this(
        replacementRules,
        ApplicationFilenameSettings(),
        ReplacementRuleDialog::show,
    )

    private var ruleEditorUpdatesSuppressed = false

    init {
        configureComponentMetadata()
        configureIcons()
        configureRuleActions()
        reloadTable()
        updateTextFields()
        configureRuleEditors()
        configurePopupMenus()
        configureApplicationSettings()
    }

    private fun configureComponentMetadata() {
        jButtonReset.name = PanelDateinamenComponentNames.RESET_RULES
        jButtonPlus.name = PanelDateinamenComponentNames.ADD_RULE
        jButtonMinus.name = PanelDateinamenComponentNames.REMOVE_RULE
        jButtonUp.name = PanelDateinamenComponentNames.MOVE_RULE_UP
        jButtonDown.name = PanelDateinamenComponentNames.MOVE_RULE_DOWN
        jTextFieldVon.name = PanelDateinamenComponentNames.REPLACEMENT_FROM
        jTextFieldNach.name = PanelDateinamenComponentNames.REPLACEMENT_TO
        jCheckBoxTable.name = PanelDateinamenComponentNames.USE_REPLACEMENT_TABLE
        jCheckBoxAscii.name = PanelDateinamenComponentNames.ONLY_ASCII
        jLabelVon.labelFor = jTextFieldVon
        jLabelNach.labelFor = jTextFieldNach
    }

    private fun configureIcons() {
        jLabelAlert.isVisible = false
        jLabelAlert.text = ""
        jLabelAlert.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/triangle-exclamation.svg", 32f)
        jButtonPlus.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/plus.svg")
        jButtonMinus.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/minus.svg")
        jButtonUp.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/arrow-up.svg")
        jButtonDown.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/arrow-down.svg")
    }

    private fun configureRuleActions() {
        jButtonReset.addActionListener {
            replacementRules.initDefaults()
            reloadTable()
            updateTextFields()
        }
        jButtonPlus.addActionListener {
            val entry = addReplacementRuleDialog(this)
            if (entry != null && replacementRules.add(entry.from, entry.to)) {
                reloadTable()
                val addedRow = tabelle.rowCount - 1
                tabelle.setRowSelectionInterval(addedRow, addedRow)
                updateTextFields()
            }
        }
        jButtonMinus.addActionListener {
            val selectedTableRow = tabelle.selectedRow
            if (selectedTableRow != -1) {
                replacementRules.removeAt(tabelle.convertRowIndexToModel(selectedTableRow))
                reloadTable()
                updateTextFields()
            }
        }
        jButtonUp.addActionListener { moveSelectedRule(RuleMoveDirection.UP) }
        jButtonDown.addActionListener { moveSelectedRule(RuleMoveDirection.DOWN) }
    }

    private fun configureRuleEditors() {
        tabelle.selectionModel.addListSelectionListener { event ->
            if (!ruleEditorUpdatesSuppressed && !event.valueIsAdjusting) {
                updateTextFields()
            }
        }
        jTextFieldVon.addTextChangeFilter(::updateFromText)
        jTextFieldNach.addTextChangeFilter(::updateToText)
    }

    private fun configurePopupMenus() {
        jTextFieldNach.componentPopupMenu = TextCopyPasteHandler(jTextFieldNach).getPopupMenu()
        jTextFieldVon.componentPopupMenu = TextCopyPasteHandler(jTextFieldVon).getPopupMenu()
    }

    private fun configureApplicationSettings() {
        jCheckBoxTable.addActionListener {
            filenameSettings.useFilenameReplaceTable = jCheckBoxTable.isSelected
        }
        jCheckBoxTable.isSelected = filenameSettings.useFilenameReplaceTable

        jCheckBoxAscii.addActionListener {
            filenameSettings.onlyAsciiFilenames = jCheckBoxAscii.isSelected
        }
        jCheckBoxAscii.isSelected = filenameSettings.onlyAsciiFilenames
    }

    private fun updateFromText(text: String) {
        if (ruleEditorUpdatesSuppressed) {
            return
        }
        val selectedTableRow = tabelle.selectedRow
        if (selectedTableRow != -1) {
            replacementRules.setFrom(
                tabelle.convertRowIndexToModel(selectedTableRow),
                text,
            )
            reloadTable()
            if (text.isEmpty()) {
                SwingUtilities.invokeLater(::updateTextFields)
            }
        }
    }

    private fun updateToText(text: String) {
        if (ruleEditorUpdatesSuppressed) {
            return
        }
        val selectedTableRow = tabelle.selectedRow
        if (selectedTableRow != -1) {
            replacementRules.setTo(
                tabelle.convertRowIndexToModel(selectedTableRow),
                text,
            )
            reloadTable()
        }
    }

    private fun moveSelectedRule(direction: RuleMoveDirection) {
        val selectedTableRow = tabelle.selectedRow
        if (selectedTableRow == -1) {
            NoSelectionErrorDialog.show(this)
            return
        }

        val modelRow = tabelle.convertRowIndexToModel(selectedTableRow)
        val newIndex = when (direction) {
            RuleMoveDirection.UP -> replacementRules.moveUp(modelRow)
            RuleMoveDirection.DOWN -> replacementRules.moveDown(modelRow)
        }
        reloadTable()
        tabelle.setRowSelectionInterval(newIndex, newIndex)
        tabelle.scrollRectToVisible(tabelle.getCellRect(newIndex, 0, true))
    }

    private fun reloadTable() {
        withRuleEditorUpdatesSuppressed {
            var selectedTableRow = tabelle.selectedRow
            if (selectedTableRow != -1) {
                selectedTableRow = tabelle.convertRowIndexToModel(selectedTableRow)
            }

            val model = NonEditableTableModel().apply {
                setColumnIdentifiers(replacementRules.columnNames())
                replacementRules.entries().forEach { entry -> addRow(entry.toArray()) }
            }
            tabelle.model = model

            if (selectedTableRow != -1) {
                when {
                    tabelle.rowCount > 0 && selectedTableRow < tabelle.rowCount ->
                        tabelle.setRowSelectionInterval(selectedTableRow, selectedTableRow)

                    tabelle.rowCount > 0 && selectedTableRow > 0 ->
                        tabelle.setRowSelectionInterval(tabelle.rowCount - 1, tabelle.rowCount - 1)

                    tabelle.rowCount > 0 -> tabelle.setRowSelectionInterval(0, 0)
                }
            } else if (tabelle.rowCount > 0) {
                tabelle.setRowSelectionInterval(0, 0)
            }
            jLabelAlert.isVisible = replacementRules.check()
        }
    }

    private fun updateTextFields() {
        val selectedTableRow = tabelle.selectedRow
        withRuleEditorUpdatesSuppressed {
            if (selectedTableRow != -1) {
                val modelRow = tabelle.convertRowIndexToModel(selectedTableRow)
                jTextFieldVon.text = tabelle.model.getValueAt(modelRow, ReplacementRules.VON_NR).toString()
                jTextFieldNach.text = tabelle.model.getValueAt(modelRow, ReplacementRules.NACH_NR).toString()
            } else {
                jTextFieldVon.text = ""
                jTextFieldNach.text = ""
            }
        }

        val hasSelection = selectedTableRow >= 0
        jTextFieldNach.isEnabled = hasSelection
        jTextFieldVon.isEnabled = hasSelection
        jButtonUp.isEnabled = hasSelection
        jButtonDown.isEnabled = hasSelection
        jLabelNach.isEnabled = hasSelection
        jLabelVon.isEnabled = hasSelection
    }

    private inline fun withRuleEditorUpdatesSuppressed(action: () -> Unit) {
        val wereUpdatesSuppressed = ruleEditorUpdatesSuppressed
        ruleEditorUpdatesSuppressed = true
        try {
            action()
        } finally {
            ruleEditorUpdatesSuppressed = wereUpdatesSuppressed
        }
    }

}

private enum class RuleMoveDirection { UP, DOWN }

internal object PanelDateinamenComponentNames {
    const val RESET_RULES = "PanelDateinamen.resetRules"
    const val ADD_RULE = "PanelDateinamen.addRule"
    const val REMOVE_RULE = "PanelDateinamen.removeRule"
    const val MOVE_RULE_UP = "PanelDateinamen.moveRuleUp"
    const val MOVE_RULE_DOWN = "PanelDateinamen.moveRuleDown"
    const val REPLACEMENT_FROM = "PanelDateinamen.replacementFrom"
    const val REPLACEMENT_TO = "PanelDateinamen.replacementTo"
    const val USE_REPLACEMENT_TABLE = "PanelDateinamen.useReplacementTable"
    const val ONLY_ASCII = "PanelDateinamen.onlyAscii"
}

internal interface FilenameSettings {
    var useFilenameReplaceTable: Boolean
    var onlyAsciiFilenames: Boolean
}

private class ApplicationFilenameSettings(
    private val configuration: ApplicationConfiguration = ApplicationConfiguration.getInstance(),
) : FilenameSettings {
    override var useFilenameReplaceTable: Boolean
        get() = configuration.useFilenameReplaceTable
        set(value) {
            configuration.useFilenameReplaceTable = value
        }

    override var onlyAsciiFilenames: Boolean
        get() = configuration.onlyAsciiFilenames
        set(value) {
            configuration.onlyAsciiFilenames = value
        }
}

private fun JTextField.addTextChangeFilter(action: (String) -> Unit) {
    (document as AbstractDocument).documentFilter = object : DocumentFilter() {
        override fun insertString(filterBypass: FilterBypass, offset: Int, text: String?, attributes: AttributeSet?) {
            filterBypass.insertString(offset, text, attributes)
            action(this@addTextChangeFilter.text)
        }

        override fun remove(filterBypass: FilterBypass, offset: Int, length: Int) {
            filterBypass.remove(offset, length)
            action(this@addTextChangeFilter.text)
        }

        override fun replace(
            filterBypass: FilterBypass,
            offset: Int,
            length: Int,
            text: String?,
            attributes: AttributeSet?,
        ) {
            filterBypass.replace(offset, length, text, attributes)
            action(this@addTextChangeFilter.text)
        }
    }
}
