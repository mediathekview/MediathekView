/*
 * Copyright (c) 2025-2026 derreisende77.
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

package mediathek.gui.tabs.tab_film.filter

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.swing.GlazedListsSwing
import com.jidesoft.swing.CheckBoxList
import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.Konstanten
import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.messages.TableModelChangeEvent
import mediathek.gui.tabs.tab_film.filter_selection.FilterSelectionComboBoxModel
import mediathek.swing.IconUtils
import mediathek.tool.EventListWithEmptyFirstEntry
import mediathek.tool.FilterDTO
import mediathek.tool.SVGIconUtilities
import mediathek.tool.withWriteLock
import org.apache.logging.log4j.LogManager
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import org.kordamp.ikonli.materialdesign2.MaterialDesignD
import java.awt.Component
import java.awt.Dimension
import java.awt.Window
import java.awt.event.*
import javax.swing.*
import javax.swing.event.ListDataEvent
import javax.swing.event.ListDataListener
import kotlin.time.Duration.Companion.milliseconds

class SwingFilterDialog internal constructor(
    private val owner: Window,
    private val filterSelectionComboBoxModel: FilterSelectionComboBoxModel,
    private val filterToggleButton: JToggleButton,
    private val filterController: FilmFilterController,
    private val prompts: DialogPrompts = JOptionPaneDialogPrompts(owner),
) : SwingFilterDialogBase(owner, filterSelectionComboBoxModel) {

    companion object {
        private const val CHECKBOX_RELOAD_DEBOUNCE_MS = 450L
        private const val ZEITRAUM_RELOAD_DEBOUNCE_MS = 450L
        private const val STR_NEW_FILTER = "Neuen Filter anlegen"
        private const val STR_CLONE_CURRENT_FILTER = "Aktuellen Filter kopieren"
        private const val STR_DELETE_CURRENT_FILTER = "Aktuellen Filter löschen"
        private const val STR_RESET_THEMA = "Thema zurücksetzen"
        private const val STR_RENAME_FILTER = "Filter umbenennen"
        private val logger = LogManager.getLogger()

        internal fun applyFilterStateChangeReload(
            previousState: FilmFilterState,
            currentState: FilmFilterState,
            reloadRequester: FilmFilterController.ReloadRequester
        ): FilterSwitchReloadType {
            if (previousState == currentState) {
                return FilterSwitchReloadType.NONE
            }

            return if (previousState.zeitraum != currentState.zeitraum) {
                reloadRequester.requestZeitraumReload()
                FilterSwitchReloadType.ZEITRAUM
            } else {
                reloadRequester.requestTableReload()
                FilterSwitchReloadType.TABLE
            }
        }
    }

    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private val sourceThemaList: EventList<String> = BasicEventList()
    private val renameFilterAction = RenameFilterAction()
    private val deleteCurrentFilterAction = DeleteCurrentFilterAction()
    private val addNewFilterAction = AddNewFilterAction()
    private val cloneCurrentFilterAction = CloneCurrentFilterAction()
    private val resetCurrentFilterAction = ResetCurrentFilterAction()
    private val checkBoxBindings = createCheckBoxBindings()
    private val senderCheckBoxList = SenderCheckBoxList()
    private val filmLengthRangeSlider = filmLengthSlider as FilmLengthSlider
    private val filterView: FilmFilterView by lazy {
        DialogFilterView(
            checkBoxBindings = checkBoxBindings.map { it.checkBox to it.selectedStateReader },
            senderSelectionRenderer = senderCheckBoxList::restoreFilterConfig,
            sourceThemaList = sourceThemaList,
            themaSelectionRenderer = ::setThemaSelectionSilently,
            filmLengthRangeSlider = filmLengthRangeSlider,
            minFilmLengthValueLabel = lblMinFilmLengthValue,
            maxFilmLengthValueLabel = lblMaxFilmLengthValue,
            zeitraumSpinner = spZeitraum,
            zeitraumFallbackWriter = filterController::onZeitraumFallbackChanged,
            deleteCurrentFilterAction = deleteCurrentFilterAction,
        )
    }
    private val filterSelectionDataListener = FilterSelectionDataListener()
    private val filterSelectionActionListener = ActionListener { syncCurrentFilterAndRestore(requestReload = true) }
    private val filterSwitchReloadRequester = object : FilmFilterController.ReloadRequester {
        override fun requestTableReload() = filterController.requestTableReload()
        override fun requestZeitraumReload() = filterController.requestZeitraumReload()
    }
    private var checkboxReloadJob: Job? = null
    private var zeitraumReloadJob: Job? = null
    private val suppressedEventTypes = mutableSetOf<SuppressedEventType>()
    private val managedActions by lazy {
        listOf<Action>(
            renameFilterAction,
            addNewFilterAction,
            resetCurrentFilterAction
        )
    }
    private val managedComponents by lazy {
        listOf<JComponent>(
            cboxFilterSelection,
            cbLockCurrentFilter,
            btnSplit,
            btnResetThema,
            label3,
            senderCheckBoxList,
            label4,
            jcbThema,
            label5,
            label7,
            lblMinFilmLengthValue,
            lblMaxFilmLengthValue,
            filmLengthRangeSlider,
            spZeitraum,
            label1,
            label2
        ) + checkBoxBindings.map(CheckBoxBinding::checkBox)
    }

    internal interface DialogPrompts {
        fun confirmDeleteCurrentFilter(): Boolean
        fun confirmResetCurrentFilter(): Boolean
        fun requestNewFilterName(suggestedName: String): String?
        fun requestRenameFilterName(currentFilterName: String): String?
    }

    private class JOptionPaneDialogPrompts(
        private val owner: Window,
    ) : DialogPrompts {
        override fun confirmDeleteCurrentFilter(): Boolean {
            return JOptionPane.showConfirmDialog(
                owner,
                "Möchten Sie wirklich den aktuellen Filter löschen?",
                Konstanten.PROGRAMMNAME,
                JOptionPane.YES_NO_OPTION
            ) == JOptionPane.YES_OPTION
        }

        override fun confirmResetCurrentFilter(): Boolean {
            return JOptionPane.showConfirmDialog(
                owner,
                "Sind Sie sicher, dass Sie den Filter zurücksetzen möchten?",
                "Filter zurücksetzen",
                JOptionPane.YES_NO_OPTION
            ) == JOptionPane.YES_OPTION
        }

        override fun requestNewFilterName(suggestedName: String): String? {
            return JOptionPane.showInputDialog(
                owner,
                "Filtername:",
                STR_NEW_FILTER,
                JOptionPane.PLAIN_MESSAGE,
                null,
                null,
                suggestedName
            ) as? String
        }

        override fun requestRenameFilterName(currentFilterName: String): String? {
            return JOptionPane.showInputDialog(
                owner,
                "Neuer Name des Filters:",
                "Filter umbenennen",
                JOptionPane.PLAIN_MESSAGE,
                null,
                null,
                currentFilterName
            ) as? String
        }
    }

    init {
        scpSenderList.setViewportView(senderCheckBoxList)
        filterController.initializeFilmData()

        configureComponents()
        setupInteraction()
        restoreState()
        registerLocalListeners()
    }

    override fun dispose() {
        checkboxReloadJob?.cancel()
        zeitraumReloadJob?.cancel()
        uiScope.cancel()
        filterSelectionComboBoxModel.removeListDataListener(filterSelectionDataListener)
        cboxFilterSelection.removeActionListener(filterSelectionActionListener)
        super.dispose()
    }

    private fun configureComponents() {
        setupRoundControls()
        btnSplit.icon = IconUtils.of(MaterialDesignD.DOTS_VERTICAL)
        populateSplitButton()
        ToggleVisibilityKeyHandler(this).installHandler(filterToggleButton.action)
        setupButtons()
    }

    private fun setupInteraction() {
        setupLockCheckBox()
        setupCheckBoxes()
        setupThemaComboBox()
        setupFilmLengthSlider()
        setupZeitraumSpinner()
    }

    private fun setupLockCheckBox() {
        val unlockedIcon = IconUtils.of(FontAwesomeSolid.LOCK_OPEN)
        val lockedIcon = IconUtils.of(FontAwesomeSolid.LOCK)
        cbLockCurrentFilter.text = null
        cbLockCurrentFilter.icon = unlockedIcon
        cbLockCurrentFilter.selectedIcon = lockedIcon
        cbLockCurrentFilter.disabledIcon = IconUtils.generateDisabledIcon(unlockedIcon)
        cbLockCurrentFilter.disabledSelectedIcon = IconUtils.generateDisabledIcon(lockedIcon)
        cbLockCurrentFilter.isSelected = filterController.areCurrentFilterChangesLocked()
        updateLockTooltip()
        cbLockCurrentFilter.addActionListener {
            filterController.setCurrentFilterChangesLocked(cbLockCurrentFilter.isSelected)
            updateLockTooltip()
        }
    }

    private fun updateLockTooltip() {
        cbLockCurrentFilter.toolTipText = if (cbLockCurrentFilter.isSelected) {
            "Filter ist gesperrt: Änderungen werden angewendet, aber nicht gespeichert"
        } else {
            "Filter ist entsperrt: Änderungen werden angewendet und gespeichert"
        }
    }

    private fun restoreState() {
        restoreConfigSettings()
        restoreWindowSizeFromConfig()
        restoreDialogVisibility()
    }

    private fun registerLocalListeners() {
        filterSelectionComboBoxModel.addListDataListener(filterSelectionDataListener)
        cboxFilterSelection.addActionListener(filterSelectionActionListener)
        addComponentListener(FilterDialogComponentListener())
    }

    private fun setupRoundControls() {
        cboxFilterSelection.putClientProperty("JComponent.roundRect", true)
        cboxFilterSelection.maximumSize = Dimension(500, 100)
        jcbThema.putClientProperty("JComponent.roundRect", true)
        spZeitraum.putClientProperty("JComponent.roundRect", true)
    }

    private fun populateSplitButton() {
        btnSplit.add(renameFilterAction)
        btnSplit.add(addNewFilterAction)
        btnSplit.add(cloneCurrentFilterAction)
        btnSplit.add(deleteCurrentFilterAction)
        btnSplit.addSeparator()
        btnSplit.add(resetCurrentFilterAction)
    }

    private fun setupButtons() {
        updateDeleteCurrentFilterButtonState()
        btnResetThema.action = ResetThemaAction()
    }

    private fun setupCheckBoxes() {
        checkBoxBindings.forEach { binding ->
            binding.checkBox.addActionListener {
                if (isSuppressed(SuppressedEventType.CHECKBOX)) {
                    return@addActionListener
                }
                binding.changeHandler(binding.checkBox.isSelected)
                requestDebouncedCheckboxReload()
            }
        }
    }

    private fun requestDebouncedCheckboxReload() {
        checkboxReloadJob = restartDebouncedJob(checkboxReloadJob, CHECKBOX_RELOAD_DEBOUNCE_MS) {
            filterController.requestTableReload()
        }
    }

    private fun createCheckBoxBindings(): List<CheckBoxBinding> {
        return listOf(
            CheckBoxBinding(cbShowNewOnly, FilmFilterState::showNewOnly, filterController::onShowNewOnlyChanged),
            CheckBoxBinding(cbShowBookMarkedOnly, FilmFilterState::showBookMarkedOnly, filterController::onShowBookMarkedOnlyChanged),
            CheckBoxBinding(cbShowOnlyHq, FilmFilterState::showHighQualityOnly, filterController::onShowHighQualityOnlyChanged),
            CheckBoxBinding(cbShowSubtitlesOnly, FilmFilterState::showSubtitlesOnly, filterController::onShowSubtitlesOnlyChanged),
            CheckBoxBinding(cbShowOnlyLivestreams, FilmFilterState::showLivestreamsOnly, filterController::onShowLivestreamsOnlyChanged),
            CheckBoxBinding(cbShowUnseenOnly, FilmFilterState::showUnseenOnly, filterController::onShowUnseenOnlyChanged),
            CheckBoxBinding(cbDontShowAbos, FilmFilterState::dontShowAbos, filterController::onDontShowAbosChanged),
            CheckBoxBinding(cbDontShowSignLanguage, FilmFilterState::dontShowSignLanguage, filterController::onDontShowSignLanguageChanged),
            CheckBoxBinding(cbDontShowGeoblocked, FilmFilterState::dontShowGeoblocked, filterController::onDontShowGeoblockedChanged),
            CheckBoxBinding(cbDontShowTrailers, FilmFilterState::dontShowTrailers, filterController::onDontShowTrailersChanged),
            CheckBoxBinding(cbDontShowAudioVersions, FilmFilterState::dontShowAudioVersions, filterController::onDontShowAudioVersionsChanged),
            CheckBoxBinding(cbDontShowDuplicates, FilmFilterState::dontShowDuplicates, filterController::onDontShowDuplicatesChanged)
        )
    }

    private fun setupFilmLengthSlider() {
        val slider = filmLengthRangeSlider

        lblMinFilmLengthValue.text = slider.lowValueText
        lblMaxFilmLengthValue.text = slider.highValueText

        slider.addChangeListener {
            lblMinFilmLengthValue.text = slider.lowValueText
            lblMaxFilmLengthValue.text = slider.highValueText

            if (!slider.valueIsAdjusting) {
                filterController.onFilmLengthChanged(slider.lowValue, slider.highValue)
            }
        }
    }

    private fun renderFilterState() {
        val renderModel = filterController.renderModel()
        withSuppressedEvents(SuppressedEventType.ZEITRAUM) {
            filterView.render(renderModel.state, renderModel.availableThemen, renderModel.canDeleteCurrentFilter)
        }
    }

    private fun createPopupMenu(): JPopupMenu {
        return JPopupMenu().apply {
            add(ResetThemaButtonAction())
        }
    }

    private fun setupThemaComboBox() {
        jcbThema.componentPopupMenu = createPopupMenu()

        val model = GlazedListsSwing.eventComboBoxModel(EventListWithEmptyFirstEntry(sourceThemaList))
        jcbThema.model = model

        val thema = filterController.state().thema
        sourceThemaList.withWriteLock {
            if (!sourceThemaList.contains(thema)) {
                sourceThemaList.add(thema)
            }
        }

        setThemaSelectionSilently(thema)
        jcbThema.addActionListener {
            if (isSuppressed(SuppressedEventType.THEMA)) {
                return@addActionListener
            }
            filterController.onThemaChanged(currentThemaSelection())
        }
    }

    private fun setupZeitraumSpinner() {
        runCatching {
            filterView.render(
                filterController.state(),
                availableThemen = sourceThemaList.toList(),
                canDeleteCurrentFilter = filterController.canDeleteCurrentFilter()
            )
            spZeitraum.installValueChangeListener {
                if (!isSuppressed(SuppressedEventType.ZEITRAUM)) {
                    filterController.onZeitraumChanged(it)
                    requestDebouncedZeitraumReload()
                }
            }
        }.onFailure {
            logger.error("Failed to setup zeitraum spinner", it)
        }
    }

    private fun requestDebouncedZeitraumReload() {
        zeitraumReloadJob = restartDebouncedJob(zeitraumReloadJob, ZEITRAUM_RELOAD_DEBOUNCE_MS) {
            //senderCheckBoxList.selectNone()
            filterController.requestZeitraumReload()
        }
    }

    private fun restartDebouncedJob(job: Job?, delayMs: Long, action: suspend () -> Unit): Job {
        job?.cancel()
        return uiScope.launch {
            delay(delayMs.milliseconds)
            action()
        }
    }

    private fun updateDeleteCurrentFilterButtonState() {
        deleteCurrentFilterAction.isEnabled = filterController.canDeleteCurrentFilter()
    }

    private fun reloadForStateChange(previousState: FilmFilterState) {
        applyFilterStateChangeReload(previousState, filterController.state(), filterSwitchReloadRequester)
    }

    private fun restoreConfigSettings() {
        checkboxReloadJob?.cancel()
        zeitraumReloadJob?.cancel()
        cbLockCurrentFilter.isSelected = filterController.areCurrentFilterChangesLocked()
        updateLockTooltip()
        withSuppressedEvents(SuppressedEventType.FILTER_SELECTION, SuppressedEventType.CHECKBOX) {
            renderFilterState()
        }
    }

    private fun currentThemaSelection(): String {
        return (jcbThema.selectedItem as? String)
            ?: (jcbThema.editor.item as? String)
            ?: ""
    }

    private fun setThemaSelectionSilently(thema: String?) {
        withSuppressedEvents(SuppressedEventType.THEMA) {
            jcbThema.selectedItem = thema
        }
    }

    private fun isSuppressed(eventType: SuppressedEventType): Boolean = eventType in suppressedEventTypes

    private inline fun withSuppressedEvents(vararg eventTypes: SuppressedEventType, action: () -> Unit) {
        suppressedEventTypes.addAll(eventTypes)
        try {
            action()
        } finally {
            eventTypes.forEach(suppressedEventTypes::remove)
        }
    }

    private fun applyEnabledState(enabled: Boolean) {
        managedActions.forEach { it.isEnabled = enabled }
        managedComponents.forEach { it.isEnabled = enabled }
    }

    override fun setEnabled(enabled: Boolean) {
        super.setEnabled(enabled)
        applyEnabledState(enabled)
    }

    fun onTableModelChangeEvent(event: TableModelChangeEvent) {
        uiScope.launch {
            val enabled = !event.active
            isEnabled = enabled

            if (event.active) {
                deleteCurrentFilterAction.isEnabled = false
            } else {
                updateDeleteCurrentFilterButtonState()
            }
        }
    }

    fun onFilmDataLoadingStarted() {
        isEnabled = false
    }

    fun onFilmDataLoaded() {
        filterController.onFilmDataLoaded()
        renderFilterState()
        isEnabled = true
    }

    internal fun triggerResetCurrentFilter() {
        resetCurrentFilterAction.actionPerformed(null)
    }

    internal fun triggerDeleteCurrentFilter() {
        deleteCurrentFilterAction.actionPerformed(null)
    }

    internal fun triggerRenameCurrentFilter() {
        renameFilterAction.actionPerformed(null)
    }

    internal fun triggerAddNewFilter() {
        addNewFilterAction.actionPerformed(null)
    }

    internal fun triggerCloneCurrentFilter() {
        cloneCurrentFilterAction.actionPerformed(null)
    }

    private fun restoreDialogVisibility() {
        isVisible = ApplicationConfiguration.getInstance().filterDialogVisible
    }

    private fun restoreWindowSizeFromConfig() {
        val state = ApplicationConfiguration.getInstance().filterDialogState
        if (state.hasStoredBounds()) {
            setBounds(state.x, state.y, state.width, state.height)
        }
    }

    class ToggleVisibilityKeyHandler(dlg: JDialog) {
        companion object {
            private const val TOGGLE_FILTER_VISIBILITY = "toggle_dialog_visibility"
        }

        private val rootPane: JRootPane = dlg.rootPane

        fun installHandler(action: Action?) {
            rootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
                .put(KeyStroke.getKeyStroke(KeyEvent.VK_F12, 0), TOGGLE_FILTER_VISIBILITY)
            rootPane.actionMap.put(TOGGLE_FILTER_VISIBILITY, action)
        }
    }

    private inner class ResetCurrentFilterAction : AbstractAction() {
        init {
            putValue(SMALL_ICON, IconUtils.of(FontAwesomeSolid.RECYCLE))
            putValue(SHORT_DESCRIPTION, "Aktuellen Filter zurücksetzen")
            putValue(NAME, "Aktuellen Filter zurücksetzen...")
        }

        override fun actionPerformed(e: ActionEvent?) {
            if (prompts.confirmResetCurrentFilter()) {
                val previousState = filterController.state()
                filterController.resetCurrentFilter()
                restoreConfigSettings()
                reloadForStateChange(previousState)
            }
        }
    }

    private inner class SenderCheckBoxList : CheckBoxList() {
        private val miVerticalWrap = JCheckBoxMenuItem("Senderliste vertikal umbrechen", false)

        init {
            visibleRowCount = -1
            setupSenderList()
            restoreVerticalWrapState()
        }

        private fun restoreVerticalWrapState() {
            if (ApplicationConfiguration.getInstance().senderListVerticalWrap) {
                miVerticalWrap.doClick()
            }
        }

        private fun setupSenderList() {
            model = GlazedListsSwing.eventListModel(filterController.senderList())
            checkBoxListSelectionModel.addListSelectionListener { event ->
                if (isSuppressed(SuppressedEventType.SENDER_SELECTION)) {
                    return@addListSelectionListener
                }
                if (!event.valueIsAdjusting) {
                    filterController.onSenderSelectionChanged(selectedSenders.toSet())
                    renderFilterState()
                }
            }

            setupContextMenu()
        }

        private fun setupContextMenu() {
            componentPopupMenu = JPopupMenu().apply {
                add(JMenuItem("Alle Senderfilter zurücksetzen").apply {
                    addActionListener { selectNone() }
                })
                addSeparator()
                add(miVerticalWrap.apply {
                    addActionListener {
                        val selected = isSelected
                        ApplicationConfiguration.getInstance().senderListVerticalWrap = selected
                        layoutOrientation = if (selected) VERTICAL_WRAP else VERTICAL
                        repaint()
                    }
                })
            }
        }

        private val selectedSenders: List<String>
            get() {
                val newSelectedSenderList = mutableListOf<String>()
                val senderListModel = model
                val selectionModel = checkBoxListSelectionModel
                for (index in 0 until senderListModel.size) {
                    if (selectionModel.isSelectedIndex(index)) {
                        newSelectedSenderList += senderListModel.getElementAt(index).toString()
                    }
                }
                return newSelectedSenderList
            }

        fun restoreFilterConfig(state: FilmFilterState) {
            val checkedSenders = state.checkedChannels
            val selectionModel = checkBoxListSelectionModel
            val senderListModel = model

            withSuppressedEvents(SuppressedEventType.SENDER_SELECTION) {
                selectionModel.valueIsAdjusting = true
                try {
                    selectNone()
                    for (index in 0 until senderListModel.size) {
                        val item = senderListModel.getElementAt(index) as String
                        if (checkedSenders.contains(item)) {
                            selectionModel.addSelectionInterval(index, index)
                        }
                    }
                } finally {
                    selectionModel.valueIsAdjusting = false
                }
            }
        }

    }

    private inner class AddNewFilterAction : AbstractAction() {
        init {
            putValue(SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/plus.svg"))
            putValue(SHORT_DESCRIPTION, STR_NEW_FILTER)
            putValue(NAME, "$STR_NEW_FILTER...")
        }

        override fun actionPerformed(e: ActionEvent?) {
            val newFilterName = prompts.requestNewFilterName(filterController.nextFilterNameSuggestion())

            if (newFilterName != null) {
                val previousState = filterController.state()
                when (val result = filterController.addFilter(newFilterName)) {
                    FilmFilterController.AddFilterResult.NameAlreadyExists -> {
                        JOptionPane.showMessageDialog(
                            owner,
                            "Ein Filter mit dem gewählten Namen existiert bereits!",
                            STR_NEW_FILTER,
                            JOptionPane.ERROR_MESSAGE
                        )
                    }
                    is FilmFilterController.AddFilterResult.Added -> {
                        restoreConfigSettings()
                        updateDeleteCurrentFilterButtonState()
                        reloadForStateChange(previousState)
                        filterSelectionComboBoxModel.selectedItem = result.filter
                    }
                }
            }
        }

    }

    private inner class CloneCurrentFilterAction : AbstractAction() {
        init {
            putValue(SMALL_ICON, IconUtils.of(FontAwesomeSolid.COPY))
            putValue(SHORT_DESCRIPTION, STR_CLONE_CURRENT_FILTER)
            putValue(NAME, "$STR_CLONE_CURRENT_FILTER...")
        }

        override fun actionPerformed(e: ActionEvent?) {
            val previousState = filterController.state()
            val result = filterController.cloneCurrentFilter()
            restoreConfigSettings()
            updateDeleteCurrentFilterButtonState()
            reloadForStateChange(previousState)
            filterSelectionComboBoxModel.selectedItem = result.filter
        }
    }

    private inner class DeleteCurrentFilterAction : AbstractAction() {
        init {
            putValue(SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/trash-can.svg"))
            putValue(SHORT_DESCRIPTION, STR_DELETE_CURRENT_FILTER)
            putValue(NAME, "$STR_DELETE_CURRENT_FILTER...")
        }

        override fun actionPerformed(e: ActionEvent?) {
            if (prompts.confirmDeleteCurrentFilter()) {
                val previousState = filterController.state()
                filterController.deleteCurrentFilter()
                restoreConfigSettings()
                updateDeleteCurrentFilterButtonState()
                reloadForStateChange(previousState)
            }
        }

    }

    private open inner class ResetThemaAction : AbstractAction() {
        init {
            putValue(SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/trash-can.svg"))
            putValue(SHORT_DESCRIPTION, STR_RESET_THEMA)
        }

        override fun actionPerformed(e: ActionEvent?) {
            filterController.onThemaReset()
            jcbThema.selectedItem = ""
        }

    }

    private inner class ResetThemaButtonAction : ResetThemaAction() {
        init {
            putValue(NAME, STR_RESET_THEMA)
        }
    }

    private inner class RenameFilterAction : AbstractAction() {
        init {
            putValue(SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/pen-to-square.svg"))
            putValue(SHORT_DESCRIPTION, STR_RENAME_FILTER)
            putValue(NAME, "$STR_RENAME_FILTER...")
        }

        override fun actionPerformed(e: ActionEvent?) {
            val currentFilterName = filterController.state().currentFilter.name
            val input = prompts.requestRenameFilterName(currentFilterName) ?: return

            if (input.isEmpty()) {
                JOptionPane.showMessageDialog(
                    owner,
                    "Filtername darf nicht leer sein!",
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.ERROR_MESSAGE
                )
                logger.warn("Rename filter text was empty...doing nothing")
                return
            }

            val trimmedName = input.trim()
            if (trimmedName == currentFilterName) {
                logger.warn("New and old filter name are identical...doing nothing")
                return
            }

            when (filterController.renameCurrentFilter(trimmedName)) {
                FilmFilterController.RenameFilterResult.NameAlreadyExists -> {
                    JOptionPane.showMessageDialog(
                        owner,
                        "Filter $trimmedName existiert bereits.\nAktion wird abgebrochen",
                        Konstanten.PROGRAMMNAME,
                        JOptionPane.ERROR_MESSAGE
                    )
                }
                FilmFilterController.RenameFilterResult.Renamed -> {
                    logger.trace("Renamed filter \"{}\" to \"{}\"", currentFilterName, trimmedName)
                }
            }
        }

    }

    inner class FilterDialogComponentListener : ComponentAdapter() {
        override fun componentResized(event: ComponentEvent) {
            storeWindowPosition(event)
        }

        override fun componentMoved(event: ComponentEvent) {
            storeWindowPosition(event)
        }

        override fun componentShown(event: ComponentEvent) {
            storeDialogVisibility()
            filterToggleButton.isSelected = true
        }

        override fun componentHidden(event: ComponentEvent) {
            storeWindowPosition(event)
            storeDialogVisibility()
            filterToggleButton.isSelected = false
        }

        private fun storeDialogVisibility() {
            ApplicationConfiguration.getInstance().filterDialogVisible = isVisible
        }

        private fun storeWindowPosition(event: ComponentEvent) {
            val component: Component = event.component
            val dims = component.size
            val loc = component.location
            ApplicationConfiguration.getInstance()
                .setFilterDialogBounds(loc.x, loc.y, dims.width, dims.height)
        }
    }

    private inner class FilterSelectionDataListener : ListDataListener {
        override fun intervalAdded(event: ListDataEvent) = restoreConfigSettings()
        override fun intervalRemoved(event: ListDataEvent) = restoreConfigSettings()
        override fun contentsChanged(event: ListDataEvent) {
            if (event.index0 == -1 && event.index1 == -1) {
                return
            }
            restoreConfigSettings()
        }
    }

    private fun syncCurrentFilterAndRestore(requestReload: Boolean = false) {
        if (isSuppressed(SuppressedEventType.FILTER_SELECTION)) {
            return
        }
        val selectedFilter = cboxFilterSelection.selectedItem as? FilterDTO ?: return
        val previousState = filterController.state()
        val changed = filterController.restoreCurrentFilterSelection(selectedFilter)
        restoreConfigSettings()
        if (!changed) {
            return
        }
        FilterSwitchReload.apply(previousState, filterController.state(), requestReload, filterSwitchReloadRequester)
    }

    private data class CheckBoxBinding(
        val checkBox: JCheckBox,
        val selectedStateReader: (FilmFilterState) -> Boolean,
        val changeHandler: (Boolean) -> Unit
    )

}

private enum class SuppressedEventType {
    CHECKBOX,
    THEMA,
    SENDER_SELECTION,
    ZEITRAUM,
    FILTER_SELECTION
}
