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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
@file:Suppress(
    "UNNECESSARY_LATEINIT",
)

package ca.odell.glazedlists.swing

import ca.odell.glazedlists.*
import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.gui.TableFormat
import ca.odell.glazedlists.impl.filter.SearchTerm
import ca.odell.glazedlists.impl.filter.TextMatcher
import ca.odell.glazedlists.impl.filter.TextSearchStrategy
import ca.odell.glazedlists.matchers.Matcher
import ca.odell.glazedlists.matchers.Matchers
import ca.odell.glazedlists.matchers.TextMatcherEditor
import java.awt.Component
import java.awt.Dimension
import java.awt.EventQueue
import java.awt.FocusTraversalPolicy
import java.awt.event.*
import java.beans.PropertyChangeEvent
import java.beans.PropertyChangeListener
import java.text.Format
import java.text.ParsePosition
import javax.swing.*
import javax.swing.border.Border
import javax.swing.event.ListDataEvent
import javax.swing.event.ListDataListener
import javax.swing.event.PopupMenuEvent
import javax.swing.event.PopupMenuListener
import javax.swing.plaf.UIResource
import javax.swing.plaf.basic.BasicComboBoxEditor
import javax.swing.plaf.basic.ComboPopup
import javax.swing.text.*

/**
 * Installs filtering and autocompletion support into a standard [JComboBox].
 *
 * This is a faithful Kotlin port of the legacy Glazed Lists implementation.
 * All mutation must occur on the Swing Event Dispatch Thread.
 */
class AutoCompleteSupport<E> private constructor(
    comboBox: JComboBox<E>,
    items: EventList<E>,
    filterator: TextFilterator<in E>?,
    format: Format?,
) {
    private val state = AutoCompleteSupportState(comboBox, items, filterator, format)

    val comboBox: JComboBox<E>?
        get() = state.comboBox

    val textFilterator: TextFilterator<in E>
        get() = state.textFilterator

    val itemList: EventList<E>
        get() = state.itemList

    var correctsCase: Boolean
        get() = state.correctsCase
        set(value) {
            state.correctsCase = value
        }

    var isStrict: Boolean
        get() = state.isStrict
        set(value) {
            state.isStrict = value
        }

    var beepOnStrictViolation: Boolean
        get() = state.beepOnStrictViolation
        set(value) {
            state.beepOnStrictViolation = value
        }

    var selectsTextOnFocusGain: Boolean
        get() = state.selectsTextOnFocusGain
        set(value) {
            state.selectsTextOnFocusGain = value
        }

    var hidesPopupOnFocusLost: Boolean
        get() = state.hidesPopupOnFocusLost
        set(value) {
            state.hidesPopupOnFocusLost = value
        }

    var filterMode: Int
        get() = state.filterMode
        set(value) {
            state.filterMode = value
        }

    var textMatchingStrategy: Any?
        get() = state.textMatchingStrategy
        set(value) {
            state.textMatchingStrategy = value
        }

    var firstItem: E
        get() = state.firstItem
        set(value) {
            state.firstItem = value
        }

    fun removeFirstItem(): E? = state.removeFirstItem()

    val isInstalled: Boolean
        get() = state.isInstalled

    fun uninstall() = state.uninstall()

    class AutoCompleteCellEditor<E>(
        private val autoCompleteSupport: AutoCompleteSupport<E>,
    ) : DefaultCellEditor(autoCompleteSupport.comboBox!!) {
        fun getAutoCompleteSupport(): AutoCompleteSupport<E> = autoCompleteSupport
    }

    companion object {
        fun <E> install(
            comboBox: JComboBox<E>,
            items: EventList<E>,
            filterator: TextFilterator<in E>? = null,
            format: Format? = null,
        ): AutoCompleteSupport<E> = installImpl(comboBox, items, filterator, format)

        fun <E> createTableCellEditor(
            tableFormat: TableFormat<E>,
            tableData: EventList<E>,
            columnIndex: Int,
            uniqueComparator: Comparator<*>? = GlazedLists.comparableComparator<Comparable<Any?>>(),
        ): AutoCompleteCellEditor<E> =
            createTableCellEditorImpl(uniqueComparator, tableFormat, tableData, columnIndex)

        fun <E> createTableCellEditor(source: EventList<E>): AutoCompleteCellEditor<E> =
            createTableCellEditorImpl(source)

        private fun <E> installImpl(
            comboBox: JComboBox<E>,
            items: EventList<E>,
            filterator: TextFilterator<in E>?,
            format: Format?,
        ): AutoCompleteSupport<E> {
            checkAutoCompleteAccessThread()
            val editorComponent = comboBox.editor.editorComponent
            require(editorComponent is JTextField) {
                "comboBox must use a JTextField as its editor component"
            }
            require(editorComponent.document is AbstractDocument) {
                "comboBox must use a JTextField backed by an AbstractDocument as its editor component"
            }
            require(comboBox.model !is AutoCompleteModelMarker) {
                "comboBox is already configured for autocompletion"
            }
            return AutoCompleteSupport(comboBox, items, filterator, format)
        }

        private fun <E> createTableCellEditorImpl(
            uniqueComparator: Comparator<*>?,
            tableFormat: TableFormat<E>,
            tableData: EventList<E>,
            columnIndex: Int,
        ): AutoCompleteCellEditor<E> {
            val columnValueFunction: (E) -> Any? =
                AutoCompleteTableColumnValueFunction(tableFormat, columnIndex)
            val allColumnValues: FunctionList<E, Any?> =
                FunctionList(tableData, columnValueFunction)

            @Suppress("UNCHECKED_CAST")
            val uniqueColumnValues: EventList<Any?> =
                UniqueList(allColumnValues, uniqueComparator as Comparator<Any?>?)
            @Suppress("UNCHECKED_CAST")
            return createTableCellEditorImpl(uniqueColumnValues) as AutoCompleteCellEditor<E>
        }

        private fun <E> createTableCellEditorImpl(source: EventList<E>): AutoCompleteCellEditor<E> {
            val comboBox: JComboBox<E> = AutoCompleteTableCellComboBox()
            comboBox.putClientProperty("JComboBox.isTableCellEditor", java.lang.Boolean.TRUE)
            val autoCompleteSupport = install(comboBox, source)
            autoCompleteSupport.selectsTextOnFocusGain = false
            val cellEditor = AutoCompleteCellEditor(autoCompleteSupport)
            cellEditor.clickCountToStart = 2
            return cellEditor
        }

        private fun checkAutoCompleteAccessThread() {
            if (!SwingUtilities.isEventDispatchThread()) {
                throw IllegalStateException(
                    "AutoCompleteSupport must be accessed from the Swing Event Dispatch Thread, but was called on Thread \"${Thread.currentThread().name}\"",
                )
            }
        }
    }
}

private interface AutoCompleteModelMarker

private class AutoCompleteSupportState<E>(
    comboBoxState: JComboBox<E>,
    private val items: EventList<E>,
    filterator: TextFilterator<in E>?,
    private val format: Format?,
) {
    private var correctsCaseState = true
    private var strictState = false
    private var beepOnStrictViolationState = true
    private var selectsTextOnFocusGainState = true
    private var hidesPopupOnFocusLostState = true

    private var comboBoxState: JComboBox<E>? = comboBoxState
    private var popupMenu: JPopupMenu? = null
    private var popup: ComboPopup? = null
    private var arrowButton: JButton? = null

    private val comboBoxModel: AutoCompleteComboBoxModel
    private val renderer: ListCellRenderer<Any?>?
    private val filteredItems: FilterList<E>
    private val firstItemList: EventList<E>
    private val allItemsFiltered: CompositeList<E>
    private val allItemsUnfiltered: CompositeList<E>
    private val filterMatcherEditor: TextMatcherEditor<E>

    private var comboBoxEditor: FormatComboBoxEditor? = null
    private var comboBoxEditorComponent: JTextField? = null
    private var document: AbstractDocument? = null
    private val documentFilter = AutoCompleteFilter()
    private lateinit var filterMatcher: UserInputFilter
    private var isFiltering = false
    private val isTableCellEditor: Boolean

    private var arrowButtonMouseListener: ArrowButtonMouseListener? = null
    private val listDataHandler: ListDataListener = ListDataHandler()
    private val popupSizerHandler: PopupMenuListener = PopupSizer()
    private val popupMouseHandler: MouseListener = PopupMouseHandler()
    private val strictModeBackspaceHandler: KeyListener = AutoCompleteKeyHandler()
    private val selectTextOnFocusGainHandler: FocusListener = ComboBoxEditorFocusHandler()
    private val documentWatcher = DocumentWatcher()
    private val modelWatcher = ModelWatcher()
    private val uiWatcher = UIWatcher()

    private var doNotPostProcessDocumentChanges = false
    private var doNotFilter = false
    private var doNotChangeDocument = false
    private var doNotAutoComplete = false
    private var doNotTogglePopup = false
    private var doNotClearFilterOnPopupHide = false

    private val originalComboBoxEditable = comboBoxState.isEditable
    private var originalModel: ComboBoxModel<E>? = comboBoxState.model
    private var originalRenderer: ListCellRenderer<in E>? = null

    private var originalSelectNextAction: Action? = null
    private var originalSelectPreviousAction: Action? = null
    private var originalSelectNext2Action: Action? = null
    private var originalSelectPrevious2Action: Action? = null
    private var originalAquaSelectNextAction: Action? = null
    private var originalAquaSelectPreviousAction: Action? = null

    init {
        val defaultRendererInstalled = comboBoxState.renderer is UIResource
        renderer = if (format != null && defaultRendererInstalled) StringFunctionRenderer() else null

        isTableCellEditor = java.lang.Boolean.TRUE == comboBoxState.getClientProperty("JComboBox.isTableCellEditor")
        doNotTogglePopup = !isTableCellEditor

        items.readWriteLock.writeLock().lock()
        try {
            @Suppress("UNCHECKED_CAST")
            val actualFilterator: TextFilterator<E> =
                filterator as TextFilterator<E>?
                    ?: DefaultTextFilterator(::convertToString) as TextFilterator<E>
            filterMatcherEditor = TextMatcherEditor(actualFilterator)
            filterMatcherEditor.mode = TextMatcherEditor.STARTS_WITH
            filterMatcher = PrefixFilter()
            filteredItems = FilterList(items, filterMatcherEditor)
            firstItemList = BasicEventList(items.publisher, items.readWriteLock)

            allItemsFiltered = CompositeList(items.publisher, items.readWriteLock)
            allItemsFiltered.addMemberList(firstItemList)
            allItemsFiltered.addMemberList(filteredItems)
            comboBoxModel = AutoCompleteComboBoxModel(allItemsFiltered)

            allItemsUnfiltered = CompositeList(items.publisher, items.readWriteLock)
            allItemsUnfiltered.addMemberList(firstItemList)
            allItemsUnfiltered.addMemberList(items)
        } finally {
            items.readWriteLock.writeLock().unlock()
        }

        comboBoxState.model = comboBoxModel
        comboBoxState.isEditable = true
        decorateCurrentUI()

        comboBoxState.addPropertyChangeListener("UI", uiWatcher)
        comboBoxState.addPropertyChangeListener("model", modelWatcher)
        comboBoxEditorComponent!!.addPropertyChangeListener("document", documentWatcher)
    }

    private abstract inner class UserInputFilter {
        var filterMatcher: Matcher<String> = Matchers.trueMatcher()
        var input = ""

        abstract fun updateFilter(userInput: String)
        fun matches(itemString: String): Boolean = filterMatcher.matches(itemString)
        abstract fun determineInput(): String
        abstract fun visualizeUserInputText()
        abstract fun findInputInString(matchString: String)
        abstract fun getInputOffset(): Int

        protected fun updateFilterCommon(userInput: String, mode: Int) {
            input = userInput
            filterMatcher =
                if (input.isEmpty()) {
                    Matchers.trueMatcher()
                } else {
                    TextMatcher(
                        singleSearchTerm(input),
                        GlazedLists.toStringTextFilterator(),
                        mode,
                        textMatchingStrategy,
                    )
                }
        }

        protected fun indexOf(matchString: String): Int {
            val strategyFactory = textMatchingStrategy
            if (strategyFactory is TextSearchStrategy.Factory) {
                val finder = strategyFactory.create(filterMode, input)
                finder.setSubtext(input)
                return finder.indexOf(matchString)
            }
            return -1
        }
    }

    private inner class PrefixFilter : UserInputFilter() {
        override fun updateFilter(userInput: String) =
            updateFilterCommon(userInput, TextMatcherEditor.STARTS_WITH)

        override fun determineInput(): String = comboBoxEditorComponent!!.text

        override fun visualizeUserInputText() {
            comboBoxEditorComponent!!.select(input.length, document!!.length)
        }

        override fun findInputInString(matchString: String) = Unit
        override fun getInputOffset(): Int = 0
    }

    private inner class ContainsFilter : UserInputFilter() {
        private var inputOffset = 0

        override fun updateFilter(userInput: String) =
            updateFilterCommon(userInput, TextMatcherEditor.CONTAINS)

        override fun determineInput(): String {
            var valueAfterEdit = comboBoxEditorComponent!!.text
            if (valueAfterEdit.length <= inputOffset) {
                inputOffset = 0
            } else {
                valueAfterEdit = valueAfterEdit.substring(inputOffset)
            }
            return valueAfterEdit
        }

        override fun visualizeUserInputText() {
            comboBoxEditorComponent!!.select(inputOffset + input.length, document!!.length)
        }

        override fun findInputInString(matchString: String) {
            inputOffset = indexOf(matchString).coerceAtLeast(0)
        }

        override fun getInputOffset(): Int = inputOffset
    }

    private fun decorateCurrentUI() {
        val currentComboBox = comboBoxState!!
        originalRenderer = currentComboBox.renderer
        popupMenu = currentComboBox.ui.getAccessibleChild(currentComboBox, 0) as JPopupMenu
        popup = popupMenu as ComboPopup
        arrowButton = findArrowButton(currentComboBox)

        arrowButton?.let {
            it.removeMouseListener(popup!!.mouseListener)
            arrowButtonMouseListener = ArrowButtonMouseListener(popup!!.mouseListener)
            it.addMouseListener(arrowButtonMouseListener)
        }

        currentComboBox.model.addListDataListener(listDataHandler)
        popupMenu!!.addPopupMenuListener(popupSizerHandler)
        popup!!.list.addMouseListener(popupMouseHandler)

        val actionMap = currentComboBox.actionMap
        originalSelectNextAction = actionMap["selectNext"]
        originalSelectPreviousAction = actionMap["selectPrevious"]
        originalSelectNext2Action = actionMap["selectNext2"]
        originalSelectPrevious2Action = actionMap["selectPrevious2"]
        originalAquaSelectNextAction = actionMap["aquaSelectNext"]
        originalAquaSelectPreviousAction = actionMap["aquaSelectPrevious"]

        val upAction: Action = MoveAction(-1)
        val downAction: Action = MoveAction(1)
        actionMap.put("selectPrevious", upAction)
        actionMap.put("selectNext", downAction)
        actionMap.put("selectPrevious2", upAction)
        actionMap.put("selectNext2", downAction)
        actionMap.put("aquaSelectPrevious", upAction)
        actionMap.put("aquaSelectNext", downAction)

        comboBoxEditor = FormatComboBoxEditor(currentComboBox.editor)
        currentComboBox.editor = comboBoxEditor
        comboBoxEditorComponent = currentComboBox.editor.editorComponent as JTextField
        document = comboBoxEditorComponent!!.document as AbstractDocument
        document!!.documentFilter = documentFilter

        renderer?.let { currentComboBox.renderer = it }
        comboBoxEditorComponent!!.addKeyListener(strictModeBackspaceHandler)
        comboBoxEditorComponent!!.addFocusListener(selectTextOnFocusGainHandler)
    }

    private fun undecorateOriginalUI() {
        val currentComboBox = comboBoxState!!
        arrowButton?.let {
            it.removeMouseListener(arrowButtonMouseListener)
            it.addMouseListener(arrowButtonMouseListener!!.decorated)
        }

        currentComboBox.model.removeListDataListener(listDataHandler)
        document!!.documentFilter = null
        if (currentComboBox.editor === comboBoxEditor) {
            currentComboBox.editor = comboBoxEditor!!.delegate
        }
        popupMenu!!.removePopupMenuListener(popupSizerHandler)
        popup!!.list.removeMouseListener(popupMouseHandler)

        val actionMap = currentComboBox.actionMap
        actionMap.put("selectPrevious", originalSelectPreviousAction)
        actionMap.put("selectNext", originalSelectNextAction)
        actionMap.put("selectPrevious2", originalSelectPrevious2Action)
        actionMap.put("selectNext2", originalSelectNext2Action)
        actionMap.put("aquaSelectPrevious", originalAquaSelectPreviousAction)
        actionMap.put("aquaSelectNext", originalAquaSelectNextAction)

        comboBoxEditorComponent!!.removeKeyListener(strictModeBackspaceHandler)
        comboBoxEditorComponent!!.removeFocusListener(selectTextOnFocusGainHandler)
        if (currentComboBox.renderer === renderer) {
            currentComboBox.renderer = originalRenderer
        }

        originalRenderer = null
        comboBoxEditor = null
        comboBoxEditorComponent = null
        document = null
        popupMenu = null
        popup = null
        arrowButton = null
    }

    private fun throwIllegalStateException(message: String): Nothing {
        val exceptionMessage =
            "$message\n" +
                    "In order for AutoCompleteSupport to continue to " +
                    "work, the following invariants must be maintained after " +
                    "AutoCompleteSupport.install() has been called:\n" +
                    "* the ComboBoxModel may not be removed\n" +
                    "* the AbstractDocument behind the JTextField can be changed but must be changed to some subclass of AbstractDocument\n" +
                    "* the DocumentFilter on the AbstractDocument behind the JTextField may not be removed\n"
        uninstall()
        throw IllegalStateException(exceptionMessage)
    }

    private fun convertToString(comboBoxElement: Any?): String {
        if (comboBoxElement === NOT_FOUND) return "NOT_FOUND"
        if (format != null) return format.format(comboBoxElement)
        return comboBoxElement?.toString() ?: ""
    }

    val comboBox: JComboBox<E>?
        get() = comboBoxState

    val textFilterator: TextFilterator<in E>
        get() = filterMatcherEditor.filterator!!

    val itemList: EventList<E>
        get() = filteredItems

    var correctsCase: Boolean
        get() = correctsCaseState
        set(value) {
            checkAccessThread()
            correctsCaseState = value
        }

    var isStrict: Boolean
        get() = strictState
        set(value) {
            checkAccessThread()
            if (strictState == value) return
            strictState = value

            if (value) {
                val currentComboBox = comboBoxState!!
                val currentText = comboBoxEditorComponent!!.text
                var currentItem = findAutoCompleteTerm(currentText)
                var currentItemText = convertToString(currentItem)
                var itemMatches = currentItem === currentComboBox.selectedItem
                var textMatches = currentItemText == currentText

                if (currentItem === NOT_FOUND && allItemsUnfiltered.isNotEmpty()) {
                    currentItem = allItemsUnfiltered[0]
                    currentItemText = convertToString(currentItem)
                    itemMatches = currentItem === currentComboBox.selectedItem
                    textMatches = currentItemText == currentText
                }

                applyFilter("")
                doNotPostProcessDocumentChanges = true
                try {
                    if (!textMatches) comboBoxEditorComponent!!.text = currentItemText
                    if (!itemMatches || currentComboBox.selectedIndex == -1) {
                        currentComboBox.selectedItem = currentItem
                    }
                } finally {
                    doNotPostProcessDocumentChanges = false
                }
            }
        }

    var beepOnStrictViolation: Boolean
        get() = beepOnStrictViolationState
        set(value) {
            checkAccessThread()
            beepOnStrictViolationState = value
        }

    var selectsTextOnFocusGain: Boolean
        get() = selectsTextOnFocusGainState
        set(value) {
            checkAccessThread()
            selectsTextOnFocusGainState = value
        }

    var hidesPopupOnFocusLost: Boolean
        get() = hidesPopupOnFocusLostState
        set(value) {
            checkAccessThread()
            hidesPopupOnFocusLostState = value
        }

    var filterMode: Int
        get() = filterMatcherEditor.mode
        set(value) {
            checkAccessThread()
            doNotChangeDocument = true
            try {
                filterMatcherEditor.mode = value
                filterMatcher =
                    if (value == TextMatcherEditor.CONTAINS) ContainsFilter() else PrefixFilter()
            } finally {
                doNotChangeDocument = false
            }
        }

    var textMatchingStrategy: Any?
        get() = filterMatcherEditor.strategy
        set(value) {
            checkAccessThread()
            doNotChangeDocument = true
            try {
                filterMatcherEditor.strategy = value
            } finally {
                doNotChangeDocument = false
            }
        }

    var firstItem: E
        get() {
            firstItemList.readWriteLock.readLock().lock()
            try {
                @Suppress("UNCHECKED_CAST")
                return if (firstItemList.isEmpty()) null as E else firstItemList[0]
            } finally {
                firstItemList.readWriteLock.readLock().unlock()
            }
        }
        set(value) {
            checkAccessThread()
            doNotChangeDocument = true
            firstItemList.readWriteLock.writeLock().lock()
            try {
                if (firstItemList.isEmpty()) firstItemList.add(value) else firstItemList[0] = value
            } finally {
                firstItemList.readWriteLock.writeLock().unlock()
                doNotChangeDocument = false
            }
        }

    fun removeFirstItem(): E? {
        checkAccessThread()
        doNotChangeDocument = true
        firstItemList.readWriteLock.writeLock().lock()
        try {
            return if (firstItemList.isEmpty()) null else firstItemList.removeAt(0)
        } finally {
            firstItemList.readWriteLock.writeLock().unlock()
            doNotChangeDocument = false
        }
    }

    val isInstalled: Boolean
        get() {
            checkAccessThread()
            return comboBoxState != null
        }

    fun uninstall() {
        checkAccessThread()
        val currentComboBox =
            comboBoxState ?: throw IllegalStateException("This AutoCompleteSupport has already been uninstalled")

        items.readWriteLock.writeLock().lock()
        try {
            currentComboBox.removePropertyChangeListener("UI", uiWatcher)
            currentComboBox.removePropertyChangeListener("model", modelWatcher)
            comboBoxEditorComponent!!.removePropertyChangeListener("document", documentWatcher)
            undecorateOriginalUI()
            currentComboBox.model = originalModel
            originalModel = null
            currentComboBox.isEditable = originalComboBoxEditable
            comboBoxModel.dispose()
            allItemsFiltered.dispose()
            allItemsUnfiltered.dispose()
            filteredItems.dispose()
            comboBoxState = null
        } finally {
            items.readWriteLock.writeLock().unlock()
        }
    }

    private fun applyFilter(newFilter: String) {
        if (doNotFilter) return

        doNotChangeDocument = true
        val listeners = unregisterAllActionListeners(comboBoxState!!)
        isFiltering = true
        try {
            filterMatcherEditor.setFilterText(arrayOf(newFilter))
        } finally {
            isFiltering = false
            registerAllActionListeners(comboBoxState!!, listeners)
            doNotChangeDocument = false
        }
    }

    private fun togglePopup() {
        if (doNotTogglePopup) return
        val currentComboBox = comboBoxState!!
        if (comboBoxModel.size == 0) {
            currentComboBox.hidePopup()
        } else if (
            currentComboBox.isShowing &&
            !currentComboBox.isPopupVisible &&
            comboBoxEditorComponent!!.hasFocus()
        ) {
            currentComboBox.showPopup()
        }
    }

    private fun findAutoCompleteTerm(value: String): Any? {
        val prefixIsEmpty = value.isEmpty()
        val valueMatcher =
            TextMatcher(
                singleSearchTerm<String>(value),
                GlazedLists.toStringTextFilterator(),
                filterMode,
                textMatchingStrategy,
            )
        var partialMatchItem: Any? = NOT_FOUND

        for (item in allItemsUnfiltered) {
            val itemString = convertToString(item)
            if (value == itemString) return item
            if (
                partialMatchItem === NOT_FOUND &&
                (if (prefixIsEmpty) itemString.isEmpty() else valueMatcher.matches(itemString))
            ) {
                partialMatchItem = item
            }
        }
        return partialMatchItem
    }

    private inner class AutoCompleteComboBoxModel(
        source: EventList<E>,
    ) : DefaultEventComboBoxModel<E>(source.swingThreadProxyList(), true),
        AutoCompleteModelMarker {
        override fun setSelectedItem(selected: Any?) {
            doNotFilter = true
            doNotAutoComplete = true
            val listeners = unregisterAllActionListeners(comboBoxState!!)
            try {
                super.setSelectedItem(selected)
                comboBoxEditorComponent?.let {
                    val caretPosition = it.caretPosition
                    it.select(caretPosition, caretPosition)
                }
            } finally {
                registerAllActionListeners(comboBoxState!!, listeners)
                doNotFilter = false
                doNotAutoComplete = false
            }
        }

        override fun listChanged(listChanges: ListEvent<E>) {
            doNotChangeDocument = true
            try {
                super.listChanged(listChanges)
            } finally {
                doNotChangeDocument = false
            }
        }
    }

    private fun isSelectNSContains(): Boolean =
        java.lang.Boolean.TRUE == comboBoxState!!.getClientProperty(GL_ENABLE_NON_STRICT_CONTAINS_SELECTION)

    private inner class AutoCompleteFilter : DocumentFilter() {
        @Throws(BadLocationException::class)
        override fun replace(
            filterBypass: FilterBypass,
            offset: Int,
            length: Int,
            string: String?,
            attributeSet: AttributeSet?,
        ) {
            if (doNotChangeDocument) return

            val valueBeforeEdit = comboBoxEditorComponent!!.text
            val selectionStart = comboBoxEditorComponent!!.selectionStart
            val selectionEnd = comboBoxEditorComponent!!.selectionEnd
            val isReplacingAllText = offset == 0 && document!!.length == length
            if (isReplacingAllText && valueBeforeEdit == string) return

            super.replace(filterBypass, offset, length, string, attributeSet)
            postProcessDocumentChange(
                filterBypass,
                attributeSet,
                valueBeforeEdit,
                selectionStart,
                selectionEnd,
                true,
            )
        }

        @Throws(BadLocationException::class)
        override fun insertString(
            filterBypass: FilterBypass,
            offset: Int,
            string: String?,
            attributeSet: AttributeSet?,
        ) {
            if (doNotChangeDocument) return

            val valueBeforeEdit = comboBoxEditorComponent!!.text
            val selectionStart = comboBoxEditorComponent!!.selectionStart
            val selectionEnd = comboBoxEditorComponent!!.selectionEnd
            super.insertString(filterBypass, offset, string, attributeSet)
            postProcessDocumentChange(
                filterBypass,
                attributeSet,
                valueBeforeEdit,
                selectionStart,
                selectionEnd,
                true,
            )
        }

        @Throws(BadLocationException::class)
        override fun remove(filterBypass: FilterBypass, offset: Int, length: Int) {
            if (doNotChangeDocument) return

            val valueBeforeEdit = comboBoxEditorComponent!!.text
            val selectionStart = comboBoxEditorComponent!!.selectionStart
            val selectionEnd = comboBoxEditorComponent!!.selectionEnd
            super.remove(filterBypass, offset, length)
            postProcessDocumentChange(
                filterBypass,
                null,
                valueBeforeEdit,
                selectionStart,
                selectionEnd,
                isStrict,
            )
        }

        @Throws(BadLocationException::class)
        private fun postProcessDocumentChange(
            filterBypass: FilterBypass,
            attributeSet: AttributeSet?,
            valueBeforeEdit: String,
            selectionStart: Int,
            selectionEnd: Int,
            allowPartialAutoCompletionTerm: Boolean,
        ) {
            if (doNotPostProcessDocumentChanges) return

            val valueAfterEdit = filterMatcher.determineInput()
            if (
                isStrict &&
                findAutoCompleteTerm(valueAfterEdit) === NOT_FOUND &&
                allItemsUnfiltered.isNotEmpty()
            ) {
                if (beepOnStrictViolation) {
                    UIManager.getLookAndFeel().provideErrorFeedback(comboBoxEditorComponent)
                }
                doNotPostProcessDocumentChanges = true
                try {
                    comboBoxEditorComponent!!.text = valueBeforeEdit
                } finally {
                    doNotPostProcessDocumentChanges = false
                }
                comboBoxEditorComponent!!.select(selectionStart, selectionEnd)
                return
            }

            val selectedItemBeforeEdit = comboBoxState!!.selectedItem
            filterMatcher.updateFilter(valueAfterEdit)
            applyFilter(filterMatcher.input)
            selectAutoCompleteTerm(
                filterBypass,
                attributeSet,
                selectedItemBeforeEdit,
                allowPartialAutoCompletionTerm,
            )
            togglePopup()
        }

        @Throws(BadLocationException::class)
        private fun selectAutoCompleteTerm(
            filterBypass: FilterBypass,
            attributeSet: AttributeSet?,
            selectedItemBeforeEdit: Any?,
            allowPartialAutoCompletionTerm: Boolean,
        ) {
            if (doNotAutoComplete) return

            val input = filterMatcher.input
            val inputIsEmpty = input.isEmpty()
            val originalCaretPosition = comboBoxEditorComponent!!.caretPosition
            val originalText = comboBoxEditorComponent!!.text
            var autoCompleteTermIsExactMatch = false

            for (i in 0 until comboBoxModel.size) {
                var itemString = convertToString(comboBoxModel.getElementAt(i))
                if (if (inputIsEmpty) itemString.isNotEmpty() else !filterMatcher.matches(itemString)) continue

                var matchIndex = i
                var matchString = itemString
                var matchIndexStartsWith = 0
                var matchStringStartsWith: String? = null
                val matchStartsWith: TextMatcher<String>? =
                    if (filterMode == TextMatcherEditor.CONTAINS) {
                        TextMatcher(
                            singleSearchTerm(input),
                            GlazedLists.toStringTextFilterator(),
                            TextMatcherEditor.STARTS_WITH,
                            textMatchingStrategy,
                        )
                    } else {
                        null
                    }

                for (j in i until comboBoxModel.size) {
                    itemString = convertToString(comboBoxModel.getElementAt(j))
                    if (input == itemString) {
                        matchIndex = j
                        matchString = itemString
                        autoCompleteTermIsExactMatch = true
                        break
                    }
                    if (matchStartsWith != null && matchStringStartsWith == null && matchStartsWith.matches(itemString)) {
                        matchIndexStartsWith = j
                        matchStringStartsWith = itemString
                    }
                }

                if (!allowPartialAutoCompletionTerm && input != itemString) return
                if (
                    !isSelectNSContains() &&
                    !isStrict &&
                    filterMode == TextMatcherEditor.CONTAINS &&
                    matchStringStartsWith == null &&
                    !autoCompleteTermIsExactMatch
                ) {
                    break
                }

                if (!autoCompleteTermIsExactMatch && matchStringStartsWith != null) {
                    matchIndex = matchIndexStartsWith
                    matchString = matchStringStartsWith
                }

                filterMatcher.findInputInString(matchString)
                if (correctsCase || isStrict) {
                    filterBypass.replace(0, document!!.length, matchString, attributeSet)
                } else {
                    val textOffset = filterMatcher.getInputOffset()
                    val inputLength = input.length
                    val replacement =
                        if (textOffset + inputLength <= matchString.length) {
                            matchString.substring(0, textOffset) +
                                    input +
                                    matchString.substring(textOffset + inputLength)
                        } else {
                            matchString
                        }
                    filterBypass.replace(0, document!!.length, replacement, attributeSet)
                }

                val silently = isTableCellEditor || selectedItemBeforeEdit == matchString
                selectItem(matchIndex, silently)
                if (autoCompleteTermIsExactMatch && originalText == input) {
                    comboBoxEditorComponent!!.caretPosition = originalCaretPosition
                } else {
                    filterMatcher.visualizeUserInputText()
                }
                return
            }

            if (originalText != input) {
                filterBypass.replace(0, document!!.length, input, attributeSet)
            }
            val silently = isTableCellEditor || selectedItemBeforeEdit == null
            selectItem(-1, silently)
        }

        private fun selectItem(index: Int, silently: Boolean) {
            val valueToSelect = if (index == -1) null else comboBoxModel.getElementAt(index)
            if (comboBoxModel.selectedItem == valueToSelect) return

            doNotChangeDocument = true
            try {
                if (silently) {
                    comboBoxModel.selectedItem = valueToSelect
                } else {
                    comboBoxState!!.selectedItem = valueToSelect
                }
            } finally {
                doNotChangeDocument = false
            }
        }
    }

    private fun selectPossibleValue(requestedIndex: Int) {
        val currentComboBox = comboBoxState!!
        var index = requestedIndex
        if (isStrict) {
            if (index < 0) index = currentComboBox.model.size - 1
            if (index > currentComboBox.model.size - 1) index = 0
        } else if (index == -2) {
            index = currentComboBox.model.size - 1
        }

        val validIndex = index >= 0 && index < currentComboBox.model.size
        if (!validIndex) index = -1

        doNotPostProcessDocumentChanges = true
        try {
            if (isTableCellEditor) {
                val listeners = unregisterAllActionListeners(currentComboBox)
                try {
                    currentComboBox.selectedIndex = index
                } finally {
                    registerAllActionListeners(currentComboBox, listeners)
                }
            } else {
                currentComboBox.selectedIndex = index
            }

            if (!validIndex) {
                comboBoxEditorComponent!!.text = filterMatcher.input
                doNotClearFilterOnPopupHide = true
                try {
                    currentComboBox.hidePopup()
                } finally {
                    doNotClearFilterOnPopupHide = false
                }
                currentComboBox.showPopup()
            }
        } finally {
            doNotPostProcessDocumentChanges = false
        }

        val newSelection = comboBoxEditorComponent!!.text
        filterMatcher.findInputInString("")
        if (
            !isSelectNSContains() &&
            !isStrict &&
            filterMode == TextMatcherEditor.CONTAINS
        ) {
            val matcher =
                TextMatcher(
                    singleSearchTerm<String>(filterMatcher.input),
                    GlazedLists.toStringTextFilterator(),
                    TextMatcherEditor.STARTS_WITH,
                    textMatchingStrategy,
                )
            if (matcher.matches(newSelection)) {
                comboBoxEditorComponent!!.select(filterMatcher.input.length, document!!.length)
            }
            return
        }
        if (filterMatcher.matches(newSelection)) {
            filterMatcher.findInputInString(newSelection)
            filterMatcher.visualizeUserInputText()
        }
    }

    private inner class MoveAction(
        private val offset: Int,
    ) : AbstractAction() {
        override fun actionPerformed(event: ActionEvent?) {
            val currentComboBox = comboBoxState!!
            if (currentComboBox.isShowing) {
                if (currentComboBox.isPopupVisible) {
                    selectPossibleValue(currentComboBox.selectedIndex + offset)
                } else {
                    applyFilter(filterMatcher.input)
                    currentComboBox.showPopup()
                }
            }
        }
    }

    private inner class ListDataHandler : ListDataListener {
        private var previousItemCount = -1
        private val checkStrictModeInvariantRunnable = CheckStrictModeInvariantRunnable()

        override fun contentsChanged(event: ListDataEvent) {
            val currentComboBox = comboBoxState!!
            val newItemCount = currentComboBox.itemCount
            if (previousItemCount != newItemCount) {
                val maxPopupItemCount = currentComboBox.maximumRowCount
                if (popupMenu!!.isShowing) {
                    if (currentComboBox.isShowing) {
                        if (newItemCount < maxPopupItemCount || previousItemCount < maxPopupItemCount) {
                            doNotClearFilterOnPopupHide = true
                            try {
                                currentComboBox.hidePopup()
                            } finally {
                                doNotClearFilterOnPopupHide = false
                            }
                            currentComboBox.showPopup()
                        }
                    } else {
                        currentComboBox.hidePopup()
                    }
                }
                previousItemCount = newItemCount
            }

            val userSelectedNewItem = event.index0 == -1 || event.index1 == -1
            if (isStrict && !userSelectedNewItem && !isFiltering) {
                SwingUtilities.invokeLater(checkStrictModeInvariantRunnable)
            }
        }

        override fun intervalAdded(event: ListDataEvent) = contentsChanged(event)
        override fun intervalRemoved(event: ListDataEvent) = contentsChanged(event)

        private inner class CheckStrictModeInvariantRunnable : Runnable {
            override fun run() {
                val editor = comboBoxEditorComponent
                if (editor != null) {
                    val currentText = editor.text
                    val item = findAutoCompleteTerm(currentText)
                    var itemText = convertToString(item)
                    if (currentText != itemText) {
                        if (item === NOT_FOUND && allItemsUnfiltered.isNotEmpty()) {
                            itemText = convertToString(allItemsUnfiltered[0])
                        }
                        editor.text = itemText
                    }
                }
            }
        }
    }

    private inner class PopupSizer : PopupMenuListener {
        override fun popupMenuWillBecomeVisible(event: PopupMenuEvent?) {
            val prototypeValue = comboBoxState!!.prototypeDisplayValue ?: return
            val popupComponent = event!!.source as JComponent
            val component = popupComponent.getComponent(0)
            if (component is JScrollPane) {
                val scrollerSize = component.preferredSize
                val prototypeSize = getPrototypeSize(prototypeValue)
                prototypeSize.width += component.verticalScrollBar.preferredSize.width
                if (prototypeSize.width > scrollerSize.width) {
                    scrollerSize.width = prototypeSize.width
                    component.maximumSize = scrollerSize
                    component.preferredSize = scrollerSize
                    component.minimumSize = scrollerSize
                }
            }
        }

        @Suppress("UNCHECKED_CAST")
        private fun getPrototypeSize(prototypeValue: E): Dimension {
            val currentComboBox = comboBoxState!!
            val actualRenderer: ListCellRenderer<in E> =
                currentComboBox.renderer ?: DefaultListCellRenderer()
            val component =
                actualRenderer.getListCellRendererComponent(
                    popup!!.list as JList<out E>,
                    prototypeValue,
                    -1,
                    false,
                    false,
                )
            component.font = currentComboBox.font
            return component.preferredSize
        }

        override fun popupMenuWillBecomeInvisible(event: PopupMenuEvent?) {
            if (!doNotClearFilterOnPopupHide) applyFilter("")
        }

        override fun popupMenuCanceled(event: PopupMenuEvent?) = Unit
    }

    private inner class PopupMouseHandler : MouseAdapter() {
        override fun mousePressed(event: MouseEvent?) {
            doNotAutoComplete = true
        }

        override fun mouseReleased(event: MouseEvent?) {
            doNotAutoComplete = false
        }
    }

    private inner class ArrowButtonMouseListener(
        val decorated: MouseListener,
    ) : MouseListener {
        override fun mousePressed(event: MouseEvent?) {
            applyFilter("")
            decorated.mousePressed(event)
        }

        override fun mouseClicked(event: MouseEvent?) = decorated.mouseClicked(event)
        override fun mouseReleased(event: MouseEvent?) = decorated.mouseReleased(event)
        override fun mouseEntered(event: MouseEvent?) = decorated.mouseEntered(event)
        override fun mouseExited(event: MouseEvent?) = decorated.mouseExited(event)
    }

    private inner class AutoCompleteKeyHandler : KeyAdapter() {
        private var actionListeners: Array<ActionListener>? = null

        override fun keyPressed(event: KeyEvent) {
            if (!isTableCellEditor) doNotTogglePopup = false
            if (event.keyChar.code == KeyEvent.VK_ENTER) {
                doNotChangeDocument = true
                actionListeners = unregisterAllActionListeners(comboBoxState!!)
            }
            if (isTrigger(event)) doNotChangeDocument = true
        }

        override fun keyTyped(event: KeyEvent) {
            if (isTrigger(event)) {
                if (comboBoxEditorComponent!!.text.isEmpty()) return
                var selectionStart =
                    minOf(
                        comboBoxEditorComponent!!.selectionStart,
                        comboBoxEditorComponent!!.selectionEnd,
                    )
                if (selectionStart == 0) {
                    if (beepOnStrictViolation) {
                        UIManager.getLookAndFeel().provideErrorFeedback(comboBoxEditorComponent)
                    }
                    return
                }
                selectionStart--
                comboBoxEditorComponent!!.caretPosition = comboBoxEditorComponent!!.text.length
                comboBoxEditorComponent!!.moveCaretPosition(selectionStart)
            }
        }

        override fun keyReleased(event: KeyEvent) {
            if (isTrigger(event)) doNotChangeDocument = false
            if (event.keyChar.code == KeyEvent.VK_ENTER) {
                filterMatcher.updateFilter(comboBoxEditorComponent!!.text)
                filterMatcher.findInputInString("")
                actionListeners?.let {
                    registerAllActionListeners(comboBoxState!!, it)
                    comboBoxState!!.actionPerformed(ActionEvent(event.source, event.id, null))
                }
                actionListeners = null
                doNotChangeDocument = false
            }
            if (!isTableCellEditor) doNotTogglePopup = true
        }

        private fun isTrigger(event: KeyEvent): Boolean =
            isStrict && event.keyChar.code == KeyEvent.VK_BACK_SPACE
    }

    private inner class ComboBoxEditorFocusHandler : FocusAdapter() {
        override fun focusGained(event: FocusEvent?) {
            if (selectsTextOnFocusGain) {
                comboBoxEditorComponent!!.select(0, comboBoxEditorComponent!!.text.length)
            }
        }

        override fun focusLost(event: FocusEvent?) {
            if (comboBoxState!!.isPopupVisible && hidesPopupOnFocusLost) {
                comboBoxState!!.isPopupVisible = false
            }
        }
    }

    private inner class UIWatcher : PropertyChangeListener {
        override fun propertyChange(event: PropertyChangeEvent?) {
            undecorateOriginalUI()
            decorateCurrentUI()
        }
    }

    private inner class ModelWatcher : PropertyChangeListener {
        override fun propertyChange(event: PropertyChangeEvent) {
            throwIllegalStateException("The ComboBoxModel cannot be changed. It was changed to: ${event.newValue}")
        }
    }

    private inner class DocumentWatcher : PropertyChangeListener {
        override fun propertyChange(event: PropertyChangeEvent) {
            val newDocument = event.newValue as Document?
            if (newDocument !is AbstractDocument) {
                throwIllegalStateException(
                    "The Document behind the JTextField was changed to no longer be an AbstractDocument. It was changed to: $newDocument",
                )
            }
            document!!.documentFilter = null
            document = newDocument
            document!!.documentFilter = documentFilter
        }
    }

    private inner class StringFunctionRenderer : DefaultListCellRenderer() {
        override fun getListCellRendererComponent(
            list: JList<*>?,
            value: Any?,
            index: Int,
            isSelected: Boolean,
            cellHasFocus: Boolean,
        ): Component {
            var string = convertToString(value)
            if (string.isEmpty()) string = " "
            return super.getListCellRendererComponent(list, string, index, isSelected, cellHasFocus)
        }
    }

    private inner class FormatComboBoxEditor(
        val delegate: ComboBoxEditor,
    ) : ComboBoxEditor, UIResource {
        private var oldValue: Any? = null

        override fun setItem(anObject: Any?) {
            oldValue = anObject
            (editorComponent as JTextField).text = convertToString(anObject)
        }

        override fun getItem(): Any? {
            val oldValueString = convertToString(oldValue)
            val currentString = (editorComponent as JTextField).text
            if (oldValueString == currentString) return oldValue
            if (format != null) return format.parseObject(currentString, ParsePosition(0))

            val previousValue = oldValue
            if (previousValue != null && previousValue !is String) {
                try {
                    val method = previousValue.javaClass.getMethod("valueOf", *VALUE_OF_SIGNATURE)
                    return method.invoke(previousValue, currentString)
                } catch (_: ReflectiveOperationException) {
                    // Preserve BasicComboBoxEditor's silent fallback.
                } catch (_: RuntimeException) {
                    // Preserve BasicComboBoxEditor's silent fallback.
                }
            }
            return currentString
        }

        override fun getEditorComponent(): Component = delegate.editorComponent
        override fun selectAll() = delegate.selectAll()
        override fun addActionListener(listener: ActionListener?) = delegate.addActionListener(listener)
        override fun removeActionListener(listener: ActionListener?) = delegate.removeActionListener(listener)
    }

    private class DefaultTextFilterator(
        private val stringFunction: (Any?) -> String,
    ) : TextFilterator<Any?> {
        override fun getFilterStrings(
            baseList: MutableList<String>,
            element: Any?,
        ) {
            baseList.add(stringFunction(element))
        }
    }

    private companion object {
        private val VALUE_OF_SIGNATURE = arrayOf<Class<*>>(String::class.java)
        private val NOT_FOUND = Any()
        private const val GL_ENABLE_NON_STRICT_CONTAINS_SELECTION = "GL:SelectContains"

        private fun <T> singleSearchTerm(text: String): Array<SearchTerm<T>> =
            arrayOf(SearchTerm(text))

        private fun checkAccessThread() {
            if (!SwingUtilities.isEventDispatchThread()) {
                throw IllegalStateException(
                    "AutoCompleteSupport must be accessed from the Swing Event Dispatch Thread, but was called on Thread \"${Thread.currentThread().name}\"",
                )
            }
        }

        private fun unregisterAllActionListeners(comboBoxState: JComboBox<*>): Array<ActionListener> {
            val listeners = comboBoxState.actionListeners
            for (listener in listeners) comboBoxState.removeActionListener(listener)
            return listeners
        }

        private fun registerAllActionListeners(
            comboBoxState: JComboBox<*>,
            listeners: Array<ActionListener>,
        ) {
            for (listener in listeners) comboBoxState.addActionListener(listener)
        }

        private fun findArrowButton(comboBoxState: JComboBox<*>): JButton? {
            for (index in 0 until comboBoxState.componentCount) {
                val component = comboBoxState.getComponent(index)
                if (component is JButton) return component
            }
            return null
        }

    }
}

private class AutoCompleteTableCellComboBox<E> :
    JComboBox<E>(),
    FocusListener {
    init {
        editor = TableCellComboBoxEditor()
        replaceUIDelegateFocusListener(editor.editorComponent, this)
        replaceUIDelegateFocusListener(this, this)
    }

    override fun focusGained(event: FocusEvent) {
        val currentEditor = editor
        if (currentEditor != null && currentEditor.editorComponent !== event.source) {
            repaint()
            if (isEditable) currentEditor.editorComponent.requestFocus()
        }
    }

    override fun focusLost(event: FocusEvent) {
        val currentEditor = editor
        if (!event.isTemporary && currentEditor != null && currentEditor.editorComponent === event.source) {
            val currentItem = currentEditor.item
            if (currentItem != null && currentItem != selectedItem) {
                fireActionPerformed(currentEditor)
            }
        }
        repaint()
    }

    private fun fireActionPerformed(source: ComboBoxEditor) {
        actionPerformed(ActionEvent(source, 0, "", EventQueue.getMostRecentEventTime(), 0))
    }

    override fun processKeyBinding(
        keyStroke: KeyStroke,
        event: KeyEvent,
        condition: Int,
        pressed: Boolean,
    ): Boolean {
        val tableCellTextField = editor.editorComponent as TableCellTextField
        tableCellTextField.processKeyBinding(keyStroke, event, condition, pressed)
        if (!tableCellTextField.hasFocus()) tableCellTextField.requestFocus()
        return super.processKeyBinding(keyStroke, event, condition, pressed)
    }

    override fun setFocusTraversalPolicy(policy: FocusTraversalPolicy?) {
        super.focusTraversalPolicy = policy
        val currentEditor = editor ?: return
        val editorComponent = currentEditor.editorComponent
        if (editorComponent is JComponent) {
            editorComponent.focusTraversalPolicy = policy
            editorComponent.isFocusTraversalPolicyProvider = policy != null
        }
    }

    private class TableCellComboBoxEditor : BasicComboBoxEditor() {
        init {
            editor = TableCellTextField()
        }
    }

    private class TableCellTextField : JTextField("", 9) {
        override fun setText(newText: String?) {
            if (!equalsText(newText)) super.text = newText
        }

        private fun equalsText(newText: String?): Boolean = text == newText

        override fun setBorder(border: Border?) = Unit

        public override fun processKeyBinding(
            keyStroke: KeyStroke,
            event: KeyEvent,
            condition: Int,
            pressed: Boolean,
        ): Boolean = super.processKeyBinding(keyStroke, event, condition, pressed)
    }

    private companion object {
        fun replaceUIDelegateFocusListener(
            component: Component,
            replacement: FocusListener,
        ) {
            for (focusListener in component.focusListeners) {
                if (focusListener.javaClass.name.contains("ComboBoxUI")) {
                    component.removeFocusListener(focusListener)
                }
            }
            component.addFocusListener(replacement)
        }
    }
}

private class AutoCompleteTableColumnValueFunction<E>(
    private val tableFormat: TableFormat<E>,
    private val columnIndex: Int,
) : (E) -> Any? {
    override fun invoke(sourceValue: E): Any? = tableFormat.getColumnValue(sourceValue, columnIndex)
}
