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

package mediathek.gui.dialogEinstellungen.blacklist

import mediathek.audiothek.ui.table.TriStateTableRowSorter
import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.config.MVColor
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.blacklist.BlacklistRule
import mediathek.filmeSuchen.ListenerFilmeLaden
import mediathek.filmeSuchen.ListenerFilmeLadenEvent
import mediathek.gui.dialog.DialogHilfe
import mediathek.gui.messages.BlacklistAboSettingChangedEvent
import mediathek.gui.messages.BlacklistChangedEvent
import mediathek.tool.*
import net.engio.mbassy.listener.Handler
import org.apache.logging.log4j.LogManager
import java.awt.Color
import java.awt.Component
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.util.regex.PatternSyntaxException
import javax.swing.*
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener
import javax.swing.table.DefaultTableCellRenderer
import javax.swing.table.TableModel
import javax.swing.table.TableStringConverter

class PanelBlacklist(
    private val daten: Daten,
    private val parentComponent: JFrame?,
    private val name: String,
) : PanelBlacklistBase() {
    var ok: Boolean = false

    private val tableModel = BlacklistRuleTableModel(daten.listeBlacklist) {
        synchronized(daten.listeFilme) {
            daten.listeFilme.toList()
        }
    }
    private val filmLoadListener = object : ListenerFilmeLaden() {
        override fun fertig(event: ListenerFilmeLadenEvent) {
            comboThemaLaden()
            tableModel.refreshFilteredCounts()
        }
    }
    private var listenersRegistered = false

    init {
        jButtonHilfe.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg")

        jButtonAendern.isEnabled = jTableBlacklist.selectionModel.selectedItemsCount == 1

        jTableBlacklist.model = tableModel
        setupTableRenderer()

        tableModel.addTableModelListener { jButtonTabelleLoeschen.isEnabled = tableModel.rowCount != 0 }
        jTableBlacklist.selectionModel.addListSelectionListener { event ->
            if (!event.valueIsAdjusting) {
                jButtonAendern.isEnabled = jTableBlacklist.selectionModel.selectedItemsCount == 1

                if (jTableBlacklist.selectionModel.selectedItemsCount == 0) {
                    resetRuleEntryFields()
                }
            }
        }

        jCheckBoxGeo.addActionListener {
            ApplicationConfiguration.getInstance().blacklistDoNotShowGeoblockedFilms = jCheckBoxGeo.isSelected
            notifyBlacklistChanged()
        }

        initPanelState()
        initBehavior()

        setupTableFilter()

        lblNumEntries.text = jTableBlacklist.rowCount.toString()
        jTableBlacklist.model.addTableModelListener { lblNumEntries.text = jTableBlacklist.rowCount.toString() }
    }

    override fun addNotify() {
        super.addNotify()
        registerListeners()
    }

    override fun removeNotify() {
        unregisterListeners()
        super.removeNotify()
    }

    private fun registerListeners() {
        if (listenersRegistered) {
            return
        }
        MessageBus.messageBus.subscribe(this)
        daten.filmeLaden.addAdListener(filmLoadListener)
        listenersRegistered = true
    }

    private fun unregisterListeners() {
        if (!listenersRegistered) {
            return
        }
        MessageBus.messageBus.unsubscribe(this)
        daten.filmeLaden.removeAdListener(filmLoadListener)
        listenersRegistered = false
    }

    private fun setupTableRenderer() {
        val renderer = object : DefaultTableCellRenderer() {
            override fun getTableCellRendererComponent(
                table: JTable,
                value: Any?,
                isSelected: Boolean,
                hasFocus: Boolean,
                row: Int,
                column: Int,
            ): Component {
                val component = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
                horizontalAlignment = if (value is Number) RIGHT else LEADING

                val modelRow = table.convertRowIndexToModel(row)
                component.foreground = if (tableModel.hasNoFilteredFilms(modelRow)) {
                    Color.RED
                } else if (isSelected) {
                    table.selectionForeground
                } else {
                    table.foreground
                }
                return component
            }
        }

        jTableBlacklist.setDefaultRenderer(String::class.java, renderer)
        jTableBlacklist.setDefaultRenderer(Int::class.javaObjectType, renderer)
    }

    private fun setupTableFilter() {
        val sorter = TriStateTableRowSorter(tableModel)
        sorter.stringConverter = object : TableStringConverter() {
            override fun toString(model: TableModel, row: Int, column: Int): String =
                model.getValueAt(row, column).toString().lowercase()
        }
        jTableBlacklist.rowSorter = sorter
        btnFilterTable.addActionListener {
            val text = tfFilter.text
            if (text.isEmpty()) {
                sorter.rowFilter = null
                GuiFunktionen.showErrorIndication(tfFilter, false)
            } else {
                try {
                    sorter.rowFilter = RowFilter.regexFilter(text.lowercase())
                    GuiFunktionen.showErrorIndication(tfFilter, false)
                } catch (exception: PatternSyntaxException) {
                    GuiFunktionen.showErrorIndication(tfFilter, true)
                    logger.error("Bad regex pattern", exception)
                }
            }
        }
    }

    private fun resetRuleEntryFields() {
        jTextFieldTitel.text = ""
        jTextFieldThemaTitel.text = ""
        jComboBoxThema.selectedItem = ""
        jComboBoxSender.selectedItem = ""
    }

    @Handler
    private fun handleBlacklistChangedEvent(@Suppress("UNUSED_PARAMETER") event: BlacklistChangedEvent) {
        SwingUtilities.invokeLater(::initPanelState)
    }

    @Handler
    private fun handleBlacklistAboSettingChangedEvent(event: BlacklistAboSettingChangedEvent) {
        if (event.sourceName != name) {
            SwingUtilities.invokeLater(::initPanelState)
        }
    }

    private fun initPanelState() {
        val applicationConfiguration = ApplicationConfiguration.getInstance()
        jCheckBoxAbo.isSelected = applicationConfiguration.blacklistApplyToAbo

        jCheckBoxBlacklistEingeschaltet.isSelected =
            applicationConfiguration.isBlacklistEnabled

        jCheckBoxZukunftNichtAnzeigen.isSelected =
            applicationConfiguration.blacklistDoNotShowFutureFilms

        jCheckBoxGeo.isSelected = applicationConfiguration.blacklistDoNotShowGeoblockedFilms

        jSliderMinuten.value = applicationConfiguration.blacklistMinimumFilmLengthMinutes

        tableModel.refreshFilteredCounts()
    }

    private fun initBehavior() {
        jTableBlacklist.addMouseListener(BlacklistTableMouseHandler())
        jTableBlacklist.selectionModel.addListSelectionListener { event ->
            if (!event.valueIsAdjusting) {
                fillControlsWithRuleData()
            }
        }

        jRadioButtonWhitelist.isSelected = ApplicationConfiguration.getInstance().blacklistWhitelistMode
        jRadioButtonWhitelist.addActionListener {
            ApplicationConfiguration.getInstance().blacklistWhitelistMode = jRadioButtonWhitelist.isSelected
            notifyBlacklistChanged()
        }
        jRadioButtonBlacklist.addActionListener {
            ApplicationConfiguration.getInstance().blacklistWhitelistMode = jRadioButtonWhitelist.isSelected
            notifyBlacklistChanged()
        }
        jCheckBoxZukunftNichtAnzeigen.addActionListener {
            ApplicationConfiguration.getInstance().blacklistDoNotShowFutureFilms = jCheckBoxZukunftNichtAnzeigen.isSelected
            notifyBlacklistChanged()
        }
        jCheckBoxAbo.addActionListener {
            ApplicationConfiguration.getInstance().blacklistApplyToAbo = jCheckBoxAbo.isSelected
            MessageBus.messageBus.publishAsync(BlacklistAboSettingChangedEvent(name))
        }
        jCheckBoxBlacklistEingeschaltet.addActionListener {
            ApplicationConfiguration.getInstance().isBlacklistEnabled = jCheckBoxBlacklistEingeschaltet.isSelected
            notifyBlacklistChanged()
        }
        jButtonHinzufuegen.addActionListener { onAddBlacklistRule() }
        jButtonAendern.addActionListener { onChangeBlacklistRule() }
        jButtonHilfe.addActionListener {
            DialogHilfe(parentComponent, true, GetFile.getHilfeSuchen(Konstanten.PFAD_HILFETEXT_BLACKLIST)).isVisible = true
        }
        jButtonTabelleLoeschen.addActionListener {
            val result = JOptionPane.showConfirmDialog(
                parentComponent,
                "<html>Möchten Sie wirklich <b>alle Regeln</b> dauerhaft löschen?</html>",
                "Blacklist Regeln",
                JOptionPane.YES_NO_OPTION,
            )
            if (result == JOptionPane.OK_OPTION) {
                tableModel.removeAll()
            }
        }
        jComboBoxSender.addActionListener { comboThemaLaden() }

        val documentListener = object : DocumentListener {
            override fun insertUpdate(event: DocumentEvent) = validatePatternInput()
            override fun removeUpdate(event: DocumentEvent) = validatePatternInput()
            override fun changedUpdate(event: DocumentEvent) = validatePatternInput()

            private fun validatePatternInput() {
                validatePatternInput(jTextFieldThemaTitel)
                validatePatternInput(jTextFieldTitel)
            }

            private fun validatePatternInput(textField: JTextField) {
                val text = textField.text
                if (Filter.isPattern(text)) {
                    textField.foreground = MVColor.getRegExPatternColor()
                    GuiFunktionen.showErrorIndication(textField, Filter.makePatternNoCache(text) == null)
                } else {
                    GuiFunktionen.showErrorIndication(textField, false)
                    textField.foreground = UIManager.getColor("TextField.foreground")
                }
            }
        }
        jTextFieldTitel.document.addDocumentListener(documentListener)
        jTextFieldThemaTitel.document.addDocumentListener(documentListener)

        jSliderMinuten.value = ApplicationConfiguration.getInstance().blacklistMinimumFilmLengthMinutes
        updateMinimumLengthText()
        jSliderMinuten.addChangeListener {
            updateMinimumLengthText()
            if (!jSliderMinuten.valueIsAdjusting) {
                ApplicationConfiguration.getInstance().blacklistMinimumFilmLengthMinutes = jSliderMinuten.value
                notifyBlacklistChanged()
            }
        }

        jComboBoxSender.model = SenderListComboBoxModel()
        comboThemaLaden()

        var handler = TextCopyPasteHandler(jTextFieldThemaTitel)
        jTextFieldThemaTitel.componentPopupMenu = handler.getPopupMenu()

        handler = TextCopyPasteHandler(jTextFieldTitel)
        jTextFieldTitel.componentPopupMenu = handler.getPopupMenu()
    }

    private fun updateMinimumLengthText() {
        jTextFieldMinuten.text = if (jSliderMinuten.value == 0) {
            "alles"
        } else {
            jSliderMinuten.value.toString()
        }
    }

    private fun onChangeBlacklistRule() {
        val sender = requireNotNull(jComboBoxSender.selectedItem).toString()
        val topic = requireNotNull(jComboBoxThema.selectedItem).toString()
        val title = jTextFieldTitel.text.trim()
        val topicTitle = jTextFieldThemaTitel.text.trim()
        if (sender.isNotEmpty() || topic.isNotEmpty() || title.isNotEmpty() || topicTitle.isNotEmpty()) {
            val selectedTableRow = jTableBlacklist.selectedRow
            if (selectedTableRow != -1) {
                val modelIndex = jTableBlacklist.convertRowIndexToModel(selectedTableRow)
                tableModel.updateRule(modelIndex, BlacklistRule(sender, topic, title, topicTitle))
            }
        }
    }

    private fun notifyBlacklistChanged() {
        daten.listeBlacklist.filterListe()
        MessageBus.messageBus.publishAsync(BlacklistChangedEvent())
    }

    private fun comboThemaLaden() {
        val filterSender = requireNotNull(jComboBoxSender.selectedItem).toString()

        val topics = daten.listeFilme.getThemen(filterSender)
        val model = DefaultComboBoxModel<String>()
        model.addElement("")
        for (topic in topics) {
            model.addElement(topic)
        }
        jComboBoxThema.model = model
    }

    private fun fillControlsWithRuleData() {
        val selectedTableRow = jTableBlacklist.selectedRow
        if (selectedTableRow != -1) {
            val modelIndex = jTableBlacklist.convertRowIndexToModel(selectedTableRow)
            val rule = tableModel.getRule(modelIndex)
            jComboBoxSender.selectedItem = rule.sender
            jComboBoxThema.selectedItem = rule.thema
            jTextFieldTitel.text = rule.titel
            jTextFieldThemaTitel.text = rule.thema_titel
        }
    }

    private fun onAddBlacklistRule() {
        val sender = requireNotNull(jComboBoxSender.selectedItem).toString()
        val topic = requireNotNull(jComboBoxThema.selectedItem).toString()
        val title = jTextFieldTitel.text.trim()
        val topicTitle = jTextFieldThemaTitel.text.trim()

        if (sender.isNotEmpty() || topic.isNotEmpty() || title.isNotEmpty() || topicTitle.isNotEmpty()) {
            val rule = BlacklistRule(sender, topic, title, topicTitle)
            if (!tableModel.contains(rule)) {
                tableModel.addRule(rule)
                resetRuleEntryFields()
            } else {
                val message = """
                    Es existiert bereits eine gleichlautende Regel.
                    Es dürfen keine Duplikate in der Liste vorkommen.
                """.trimIndent()
                JOptionPane.showMessageDialog(this, message, Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE)
            }
        }
    }

    private inner class BlacklistTableMouseHandler : MouseAdapter() {
        override fun mousePressed(event: MouseEvent) {
            if (event.isPopupTrigger) {
                showMenu(event)
            }
        }

        override fun mouseReleased(event: MouseEvent) {
            if (event.isPopupTrigger) {
                showMenu(event)
            }
        }

        private fun onRemoveBlacklistRules() {
            val selectedIndices = jTableBlacklist.selectionModel.selectedIndices
            if (selectedIndices.size == 1) {
                val modelIndex = jTableBlacklist.convertRowIndexToModel(selectedIndices[0])
                tableModel.removeRow(modelIndex)
            } else {
                val rules = selectedIndices.map { selectedRow ->
                    val modelIndex = jTableBlacklist.convertRowIndexToModel(selectedRow)
                    tableModel.getRule(modelIndex)
                }
                tableModel.removeRules(rules)
            }
        }

        private fun showMenu(event: MouseEvent) {
            val row = jTableBlacklist.rowAtPoint(event.point)
            if (row == -1) {
                return
            }
            if (!jTableBlacklist.isRowSelected(row)) {
                jTableBlacklist.selectionModel.setSelectionInterval(row, row)
            }

            val menu = JPopupMenu()
            val menuText = if (jTableBlacklist.selectedRowCount > 1) "Zeilen löschen" else "Zeile löschen"
            val item = JMenuItem(menuText)
            item.addActionListener { onRemoveBlacklistRules() }
            menu.add(item)
            menu.show(event.component, event.x, event.y)
        }
    }

    private companion object {
        private val logger = LogManager.getLogger()
    }
}
