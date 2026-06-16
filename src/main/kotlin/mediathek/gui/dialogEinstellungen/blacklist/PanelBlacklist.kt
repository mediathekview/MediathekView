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

import kotlinx.coroutines.CancellationException
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.cancel
import kotlinx.coroutines.launch
import kotlinx.coroutines.swing.Swing
import kotlinx.coroutines.withContext
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
import mediathek.swing.IconUtils
import mediathek.tool.*
import net.engio.mbassy.listener.Handler
import org.apache.logging.log4j.LogManager
import org.kordamp.ikonli.materialdesign2.MaterialDesignF
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
) : PanelBlacklistBase() {
    private val aboSettingEventSource = Any()
    private val tableModel = BlacklistRuleTableModel(daten.listeBlacklist)
    private val filmLoadListener = object : ListenerFilmeLaden() {
        override fun fertig(event: ListenerFilmeLadenEvent) {
            comboThemaLaden()
            scheduleFilteredCountRefresh()
        }
    }
    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private var filteredCountRefreshJob: Job? = null
    private var filteredCountRefreshSequence = 0
    private var blacklistRefreshJob: Job? = null
    private var blacklistRefreshSequence = 0
    private var listenersRegistered = false
    private lateinit var tableColumnSettings: BlacklistRuleTableColumnSettings

    init {
        jButtonHilfe.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg")
        jButtonDeactivateZeroFilterRules.icon = IconUtils.of(MaterialDesignF.FILTER_OFF_OUTLINE)

        jButtonAendern.isEnabled = jTableBlacklist.selectionModel.selectedItemsCount == 1

        jTableBlacklist.model = tableModel
        tableColumnSettings = BlacklistRuleTableColumnSettings(jTableBlacklist)
        tableColumnSettings.restore()
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
            scheduleBlacklistSettingsChanged()
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
        tableColumnSettings.save()
        cancelScheduledRefreshes()
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

    private fun cancelScheduledRefreshes() {
        filteredCountRefreshJob?.cancel()
        filteredCountRefreshJob = null
        blacklistRefreshJob?.cancel()
        blacklistRefreshJob = null
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
        jCheckBoxRuleActive.isSelected = true
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
        if (event.source !== aboSettingEventSource) {
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

        scheduleFilteredCountRefresh()
    }

    private fun scheduleFilteredCountRefresh() {
        uiScope.launch {
            val refreshSequence = ++filteredCountRefreshSequence
            filteredCountRefreshJob?.cancel()
            filteredCountRefreshJob = launch {
                try {
                    val counts = withContext(Dispatchers.Default) {
                        tableModel.calculateFilteredCounts(daten.listeFilme.snapshot())
                    }
                    if (refreshSequence == filteredCountRefreshSequence) {
                        tableModel.applyFilteredCounts(counts)
                    }
                } catch (exception: CancellationException) {
                    throw exception
                } catch (exception: Exception) {
                    logger.error("Failed to refresh blacklist filtered counts", exception)
                }
            }
        }
    }

    private fun scheduleBlacklistFilterRefresh() {
        uiScope.launch {
            val refreshSequence = ++blacklistRefreshSequence
            blacklistRefreshJob?.cancel()
            blacklistRefreshJob = launch {
                try {
                    withContext(Dispatchers.Default) {
                        daten.listeBlacklist.filterListe()
                    }
                    if (refreshSequence == blacklistRefreshSequence) {
                        MessageBus.messageBus.publishAsync(BlacklistChangedEvent())
                    }
                } catch (exception: CancellationException) {
                    throw exception
                } catch (exception: Exception) {
                    logger.error("Failed to refresh blacklist filter", exception)
                }
            }
        }
    }

    private fun scheduleBlacklistRulesChanged() {
        scheduleFilteredCountRefresh()
        scheduleBlacklistFilterRefresh()
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
            scheduleBlacklistSettingsChanged()
        }
        jRadioButtonBlacklist.addActionListener {
            ApplicationConfiguration.getInstance().blacklistWhitelistMode = jRadioButtonWhitelist.isSelected
            scheduleBlacklistSettingsChanged()
        }
        jCheckBoxZukunftNichtAnzeigen.addActionListener {
            ApplicationConfiguration.getInstance().blacklistDoNotShowFutureFilms = jCheckBoxZukunftNichtAnzeigen.isSelected
            scheduleBlacklistSettingsChanged()
        }
        jCheckBoxAbo.addActionListener {
            ApplicationConfiguration.getInstance().blacklistApplyToAbo = jCheckBoxAbo.isSelected
            MessageBus.messageBus.publishAsync(BlacklistAboSettingChangedEvent(aboSettingEventSource))
        }
        jCheckBoxBlacklistEingeschaltet.addActionListener {
            ApplicationConfiguration.getInstance().isBlacklistEnabled = jCheckBoxBlacklistEingeschaltet.isSelected
            scheduleBlacklistSettingsChanged()
        }
        jButtonHinzufuegen.addActionListener { onAddBlacklistRule() }
        jButtonAendern.addActionListener { onChangeBlacklistRule() }
        jButtonDeactivateZeroFilterRules.addActionListener { onDeactivateZeroFilterRules() }
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
                if (daten.listeBlacklist.isNotEmpty()) {
                    daten.listeBlacklist.clearWithoutNotification()
                    tableModel.rulesChanged()
                    scheduleBlacklistRulesChanged()
                }
            }
        }
        jComboBoxSender.addActionListener { comboThemaLaden("") }

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
                scheduleBlacklistSettingsChanged()
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
        val active = jCheckBoxRuleActive.isSelected
        if (sender.isNotEmpty() || topic.isNotEmpty() || title.isNotEmpty() || topicTitle.isNotEmpty()) {
            val selectedTableRow = jTableBlacklist.selectedRow
            if (selectedTableRow != -1) {
                val modelIndex = jTableBlacklist.convertRowIndexToModel(selectedTableRow)
                if (!daten.listeBlacklist.replaceAtIfUniqueWithoutNotification(
                        modelIndex,
                        BlacklistRule(sender, topic, title, topicTitle, active),
                    )
                ) {
                    showDuplicateRuleMessage()
                } else {
                    tableModel.ruleUpdated(modelIndex)
                    scheduleBlacklistRulesChanged()
                }
            }
        }
    }

    private fun onDeactivateZeroFilterRules() {
        val changedRows = BlacklistRuleBulkActions.deactivateActiveRulesWithZeroFilteredCount(
            daten.listeBlacklist,
            tableModel,
        )
        for (modelIndex in changedRows) {
            tableModel.ruleUpdated(modelIndex)
        }
        if (changedRows.isNotEmpty()) {
            fillControlsWithRuleData()
            scheduleBlacklistRulesChanged()
        }
    }

    private fun scheduleBlacklistSettingsChanged() {
        scheduleBlacklistFilterRefresh()
    }

    private fun comboThemaLaden() {
        comboThemaLaden(jComboBoxThema.selectedItem?.toString().orEmpty())
    }

    private fun comboThemaLaden(selectedTopic: String) {
        val filterSender = requireNotNull(jComboBoxSender.selectedItem).toString()

        val topics = daten.listeFilme.getThemen(filterSender)
        val model = DefaultComboBoxModel<String>()
        model.addElement("")
        for (topic in topics) {
            model.addElement(topic)
        }
        if (selectedTopic.isNotEmpty() && !topics.contains(selectedTopic)) {
            model.addElement(selectedTopic)
        }
        jComboBoxThema.model = model
        jComboBoxThema.selectedItem = selectedTopic
    }

    private fun fillControlsWithRuleData() {
        val selectedTableRow = jTableBlacklist.selectedRow
        if (selectedTableRow != -1) {
            val modelIndex = jTableBlacklist.convertRowIndexToModel(selectedTableRow)
            val rule = tableModel.getRule(modelIndex)
            jCheckBoxRuleActive.isSelected = rule.active
            jComboBoxSender.selectedItem = rule.sender
            comboThemaLaden(rule.thema)
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
        val active = jCheckBoxRuleActive.isSelected

        if (sender.isNotEmpty() || topic.isNotEmpty() || title.isNotEmpty() || topicTitle.isNotEmpty()) {
            val rule = BlacklistRule(sender, topic, title, topicTitle, active)
            val rowIndex = daten.listeBlacklist.size
            if (daten.listeBlacklist.addWithoutNotification(rule)) {
                tableModel.ruleInserted(rowIndex)
                resetRuleEntryFields()
                scheduleBlacklistRulesChanged()
            } else {
                showDuplicateRuleMessage()
            }
        }
    }

    private fun showDuplicateRuleMessage() {
        val message = """
            Es existiert bereits eine gleichlautende Regel.
            Es dürfen keine Duplikate in der Liste vorkommen.
        """.trimIndent()
        JOptionPane.showMessageDialog(this, message, Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE)
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
                daten.listeBlacklist.removeAtWithoutNotification(modelIndex)
                tableModel.ruleRemoved(modelIndex)
                scheduleBlacklistRulesChanged()
            } else {
                val rules = selectedIndices.map { selectedRow ->
                    val modelIndex = jTableBlacklist.convertRowIndexToModel(selectedRow)
                    tableModel.getRule(modelIndex)
                }
                if (daten.listeBlacklist.removeAllWithoutNotification(rules)) {
                    tableModel.rulesChanged()
                    scheduleBlacklistRulesChanged()
                }
            }
        }

        private fun onToggleBlacklistRulesActive() {
            val selectedIndices = jTableBlacklist.selectionModel.selectedIndices
                .map(jTableBlacklist::convertRowIndexToModel)
                .distinct()
            selectedIndices.forEach { modelIndex ->
                val rule = tableModel.getRule(modelIndex)
                if (daten.listeBlacklist.replaceAtIfUniqueWithoutNotification(
                        modelIndex,
                        rule.copy(active = !rule.active),
                    )
                ) {
                    tableModel.ruleUpdated(modelIndex)
                }
            }
            if (selectedIndices.isNotEmpty()) {
                fillControlsWithRuleData()
                scheduleBlacklistRulesChanged()
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
            val toggleActiveItem = JMenuItem("Aktiv umschalten")
            toggleActiveItem.addActionListener { onToggleBlacklistRulesActive() }
            menu.add(toggleActiveItem)

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
