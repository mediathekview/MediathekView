/*
 * MediathekView
 * Copyright (c) 2026 derreisende77.
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
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 *
 * Project: https://github.com/mediathekview/MediathekView
 */
package ca.odell.glazedlists.swing

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.GlazedLists
import ca.odell.glazedlists.matchers.TextMatcherEditor
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.awt.Component
import java.awt.event.ActionListener
import java.text.FieldPosition
import java.text.Format
import java.text.ParsePosition
import javax.swing.*
import javax.swing.text.AbstractDocument
import javax.swing.text.Document
import javax.swing.text.PlainDocument

class AutoCompleteSupportBehaviorTest {
    @Test
    fun defaultsAndMutableOptionsRoundTrip() = onEdt {
        val combo = JComboBox<String>()
        val items = BasicEventList<String>().apply { addAll(listOf("alpha", "beta")) }
        val support = AutoCompleteSupport.install(combo, items)
        try {
            assertTrue(support.isInstalled)
            assertSame(combo, support.comboBox)
            assertTrue(support.correctsCase)
            assertFalse(support.isStrict)
            assertTrue(support.beepOnStrictViolation)
            assertTrue(support.selectsTextOnFocusGain)
            assertTrue(support.hidesPopupOnFocusLost)
            assertEquals(TextMatcherEditor.STARTS_WITH, support.filterMode)
            assertSame(TextMatcherEditor.IDENTICAL_STRATEGY, support.textMatchingStrategy)
            assertNull(support.firstItem)
            support.correctsCase = false
            support.isStrict = true
            support.beepOnStrictViolation = false
            support.selectsTextOnFocusGain = false
            support.hidesPopupOnFocusLost = false
            support.filterMode = TextMatcherEditor.CONTAINS
            support.textMatchingStrategy = TextMatcherEditor.NORMALIZED_STRATEGY

            assertFalse(support.correctsCase)
            assertTrue(support.isStrict)
            assertFalse(support.beepOnStrictViolation)
            assertFalse(support.selectsTextOnFocusGain)
            assertFalse(support.hidesPopupOnFocusLost)
            assertEquals(TextMatcherEditor.CONTAINS, support.filterMode)
            assertSame(TextMatcherEditor.NORMALIZED_STRATEGY, support.textMatchingStrategy)
            assertThrows(IllegalArgumentException::class.java) { support.filterMode = Int.MIN_VALUE }
        } finally {
            support.uninstall()
        }
    }

    @Test
    fun installAndUninstallRestoreSwingStateAndRejectDoubleUninstall() = onEdt {
        val combo = JComboBox(arrayOf("original"))
        val items = BasicEventList<String>().apply { addAll(listOf("alpha", "beta")) }
        val originalUi = combo.ui
        val originalModel = combo.model
        val originalEditable = combo.isEditable
        val originalRenderer = combo.renderer
        val originalEditor = combo.editor
        val originalDocument = (originalEditor.editorComponent as JTextField).document as AbstractDocument
        val originalActions = ACTION_KEYS.associateWith { combo.actionMap[it] }
        val originalPropertyListenerCount = combo.propertyChangeListeners.size
        val originalKeyListenerCount = originalEditor.editorComponent.keyListeners.size

        val support = AutoCompleteSupport.install(combo, items)
        val installedEditor = combo.editor.editorComponent as JTextField
        assertSame(originalUi, combo.ui)
        assertSame(originalDocument, installedEditor.document)
        assertSame(originalRenderer, combo.renderer)
        assertNotSame(originalModel, combo.model)
        assertNotSame(originalEditor, combo.editor)
        assertTrue(combo.isEditable)
        assertNotNull((installedEditor.document as AbstractDocument).documentFilter)
        assertEquals(originalPropertyListenerCount + 2, combo.propertyChangeListeners.size)
        assertEquals(originalKeyListenerCount + 1, installedEditor.keyListeners.size)
        ACTION_KEYS.forEach { assertNotSame(originalActions[it], combo.actionMap[it]) }

        support.uninstall()

        assertFalse(support.isInstalled)
        assertSame(originalUi, combo.ui)
        assertSame(originalModel, combo.model)
        assertEquals(originalEditable, combo.isEditable)
        assertSame(originalEditor, combo.editor)
        assertSame(originalRenderer, combo.renderer)
        assertSame(originalDocument, (combo.editor.editorComponent as JTextField).document)
        assertNull(originalDocument.documentFilter)
        assertEquals(originalPropertyListenerCount, combo.propertyChangeListeners.size)
        assertEquals(originalKeyListenerCount, combo.editor.editorComponent.keyListeners.size)
        ACTION_KEYS.forEach { assertSame(originalActions[it], combo.actionMap[it]) }
        assertThrows(IllegalStateException::class.java, support::uninstall)
    }

    @Test
    fun installAndEnvironmentInvariantFailuresMatchBaseline() = onEdt {
        val nonTextEditorCombo = JComboBox<String>()
        nonTextEditorCombo.editor = object : ComboBoxEditor {
            private var value: Any? = null

            override fun getEditorComponent(): Component = JPanel()

            override fun setItem(anObject: Any?) {
                value = anObject
            }

            override fun getItem(): Any? = value

            override fun selectAll() = Unit

            override fun addActionListener(listener: ActionListener?) = Unit

            override fun removeActionListener(listener: ActionListener?) = Unit
        }
        assertThrows(IllegalArgumentException::class.java) {
            AutoCompleteSupport.install(nonTextEditorCombo, BasicEventList())
        }

        val invalidEditorCombo = JComboBox<String>()
        (invalidEditorCombo.editor.editorComponent as JTextField).document = NonAbstractDocument()
        assertThrows(IllegalArgumentException::class.java) {
            AutoCompleteSupport.install(invalidEditorCombo, BasicEventList())
        }

        val combo = JComboBox<String>()
        val support = AutoCompleteSupport.install(combo, BasicEventList())
        try {
            assertThrows(IllegalArgumentException::class.java) {
                AutoCompleteSupport.install(combo, BasicEventList())
            }
            val replacementModel = DefaultComboBoxModel<String>()
            assertThrows(IllegalStateException::class.java) { combo.model = replacementModel }
            assertNotSame(replacementModel, combo.model)
        } finally {
            if (support.isInstalled) support.uninstall()
        }

        val documentCombo = JComboBox<String>()
        val documentSupport = AutoCompleteSupport.install(documentCombo, BasicEventList())
        try {
            val replacement = NonAbstractDocument()
            assertThrows(IllegalStateException::class.java) {
                (documentCombo.editor.editorComponent as JTextField).document = replacement
            }
            assertSame(replacement, (documentCombo.editor.editorComponent as JTextField).document)
        } finally {
            if (documentSupport.isInstalled) documentSupport.uninstall()
        }
    }

    @Test
    fun nullElementsFilterWithoutBeingRenderedAsMatches() = onEdt {
        val combo = JComboBox<String?>()
        val items = BasicEventList<String?>().apply {
            addAll(listOf(null, "New Brunswick", "Nova Scotia", "Newfoundland", "Prince Edward Island", null))
        }
        val support = AutoCompleteSupport.install(combo, items)
        try {
            assertEquals(6, combo.itemCount)
            (combo.editor.editorComponent as JTextField).text = "New"
            assertEquals(listOf("New Brunswick", "Newfoundland"), combo.items())
        } finally {
            support.uninstall()
        }
    }

    @Test
    fun strictCaseCorrectionAndFilterModesPreserveDocumentAndSelectionSemantics() = onEdt {
        val combo = JComboBox<String>()
        val items = BasicEventList<String>().apply {
            addAll(listOf("New Brunswick", "Nova Scotia", "Newfoundland", "Prince Edward Island"))
        }
        val support = AutoCompleteSupport.install(combo, items)
        try {
            val text = combo.editor.editorComponent as JTextField
            val document = text.document as AbstractDocument

            support.correctsCase = false
            support.isStrict = false
            document.replace(0, document.length, "NEW", null)
            assertEquals("NEW Brunswick", text.text)
            assertEquals(2, combo.itemCount)

            support.correctsCase = true
            document.replace(0, document.length, "NEW", null)
            assertEquals("New Brunswick", text.text)

            support.isStrict = true
            support.correctsCase = false
            document.replace(0, document.length, "garbage", null)
            assertEquals("New Brunswick", text.text)
            assertEquals(4, combo.itemCount)

            support.isStrict = false
            support.correctsCase = true
            document.replace(0, document.length, "u", null)
            assertEquals(0, combo.itemCount)
            support.filterMode = TextMatcherEditor.CONTAINS
            assertEquals("u", text.text)
            assertEquals(listOf("New Brunswick", "Newfoundland"), combo.items())
            document.replace(0, document.length, "n", null)
            assertEquals(4, combo.itemCount)
            support.filterMode = TextMatcherEditor.STARTS_WITH
            assertEquals("New Brunswick", text.text)
            assertEquals("ew Brunswick", text.selectedText)
        } finally {
            support.uninstall()
        }
    }

    @Test
    fun firstItemRemainsAtIndexZeroAcrossFilteringAndStrictMode() = onEdt {
        val combo = JComboBox<String>()
        val items = BasicEventList<String>()
        val support = AutoCompleteSupport.install(combo, items)
        try {
            support.firstItem = "Special"
            items.addAll(listOf("one", "two", "three"))
            assertEquals(listOf("Special", "one", "two", "three"), combo.items())

            val document = (combo.editor.editorComponent as JTextField).document as AbstractDocument
            document.insertString(0, "t", null)
            assertEquals(listOf("Special", "two", "three"), combo.items())
            assertSame("Special", support.removeFirstItem())
            assertEquals(listOf("two", "three"), combo.items())
            assertNull(support.firstItem)

            support.isStrict = false
            support.firstItem = "Fallback"
            combo.selectedItem = null
            support.isStrict = true
            assertEquals("Fallback", combo.selectedItem)
            assertEquals(0, combo.selectedIndex)
        } finally {
            support.uninstall()
        }
    }

    @Test
    fun exactMatchEditingPreservesCaretAndSelectionIdentity() = onEdt {
        val combo = JComboBox<String>()
        val items = BasicEventList<String>().apply { add("foobar") }
        val support = AutoCompleteSupport.install(combo, items)
        try {
            val text = combo.editor.editorComponent as JTextField
            val document = text.document as AbstractDocument
            document.replace(0, document.length, "fobar", null)
            text.caretPosition = 2
            document.insertString(2, "o", null)
            assertEquals("foobar", text.text)
            assertEquals(3, text.caretPosition)
            assertSame(items[0], combo.selectedItem)

            document.insertString(3, "t", null)
            assertEquals("footbar", text.text)
            assertEquals(4, text.caretPosition)
            assertNull(combo.selectedItem)

            document.remove(3, 1)
            assertEquals("foobar", text.text)
            assertEquals(3, text.caretPosition)
            assertSame(items[0], combo.selectedItem)
        } finally {
            support.uninstall()
        }
    }

    @Test
    fun explicitlySelectedValueSurvivesSourceChangesUntilUserEdits() = onEdt {
        val combo = JComboBox<String>()
        val items = BasicEventList<String>()
        val support = AutoCompleteSupport.install(combo, items)
        try {
            combo.selectedItem = "Foo"
            items.addAll(listOf("Foobar", "Blarg"))
            assertEquals("Foo", combo.selectedItem)
            assertEquals("Foo", (combo.editor.editorComponent as JTextField).text)

            val document = (combo.editor.editorComponent as JTextField).document as AbstractDocument
            document.insertString(3, "b", null)
            assertEquals("Foobar", combo.selectedItem)
            combo.selectedItem = "Foo"
            assertEquals("Foo", combo.selectedItem)
        } finally {
            support.uninstall()
        }
    }

    @Test
    fun sourceMutationsDoNotRewriteNonMatchingEditorText() = onEdt {
        val combo = JComboBox<String>()
        val items = BasicEventList<String>().apply { add("foobar") }
        val support = AutoCompleteSupport.install(combo, items)
        try {
            val text = combo.editor.editorComponent as JTextField
            (text.document as AbstractDocument).replace(0, text.document.length, "fobar", null)
            assertNull(combo.selectedItem)

            items.add("fobar")
            assertEquals("fobar", text.text)
            assertNull(combo.selectedItem)
            items[1] = "wheeble"
            assertEquals("fobar", text.text)
            items.removeAt(1)
            assertEquals("fobar", text.text)
        } finally {
            support.uninstall()
        }
    }

    @Test
    fun containsSelectionClientPropertyControlsMiddleMatchCompletion() = onEdt {
        fun exercise(selectContains: Boolean): Triple<String, Any?, String?> {
            val combo = JComboBox<String>()
            combo.putClientProperty("GL:SelectContains", selectContains)
            val items = BasicEventList<String>().apply { addAll(listOf("xabcdex", "ab", "abcd")) }
            val support = AutoCompleteSupport.install(combo, items)
            return try {
                support.filterMode = TextMatcherEditor.CONTAINS
                val text = combo.editor.editorComponent as JTextField
                val document = text.document as AbstractDocument
                document.replace(0, 0, "abcde", null)
                Triple(text.text, combo.selectedItem, text.selectedText)
            } finally {
                support.uninstall()
            }
        }

        assertEquals(Triple("xabcdex", "xabcdex", "x"), exercise(true))
        assertEquals(Triple("abcde", null, null), exercise(false))
    }

    @Test
    fun customFormatControlsEditorValueButPreservesCustomRenderer() = onEdt {
        val combo = JComboBox<Number>()
        val renderer = DefaultListCellRenderer()
        combo.renderer = renderer
        val items = BasicEventList<Number>().apply { addAll(listOf(12L, 34L)) }
        val format = object : Format() {
            override fun format(obj: Any?, toAppendTo: StringBuffer, pos: FieldPosition): StringBuffer =
                toAppendTo.append("#").append(obj)

            override fun parseObject(source: String, pos: ParsePosition): Any? {
                if (!source.startsWith("#")) {
                    pos.errorIndex = pos.index
                    return null
                }
                pos.index = source.length
                return source.substring(1).toLong()
            }
        }
        val support = AutoCompleteSupport.install(combo, items, GlazedLists.toStringTextFilterator(), format)
        try {
            assertSame(renderer, combo.renderer)
            combo.editor.item = 12L
            assertEquals("#12", (combo.editor.editorComponent as JTextField).text)
            (combo.editor.editorComponent as JTextField).text = "#34"
            assertEquals(34L, combo.editor.item)
        } finally {
            support.uninstall()
        }
        assertSame(renderer, combo.renderer)
    }

    @Test
    fun publicMutationsAndLifecycleRejectNonEdtAccess() {
        assertThrows(IllegalStateException::class.java) {
            AutoCompleteSupport.install(JComboBox<String>(), BasicEventList())
        }

        lateinit var support: AutoCompleteSupport<String>
        SwingUtilities.invokeAndWait {
            support = AutoCompleteSupport.install(JComboBox(), BasicEventList())
        }
        assertThrows(IllegalStateException::class.java) { support.correctsCase = false }
        assertThrows(IllegalStateException::class.java) { support.isStrict = true }
        assertThrows(IllegalStateException::class.java) { support.beepOnStrictViolation = false }
        assertThrows(IllegalStateException::class.java) { support.selectsTextOnFocusGain = false }
        assertThrows(IllegalStateException::class.java) { support.hidesPopupOnFocusLost = false }
        assertThrows(IllegalStateException::class.java) { support.filterMode = TextMatcherEditor.CONTAINS }
        assertThrows(IllegalStateException::class.java) {
            support.textMatchingStrategy = TextMatcherEditor.NORMALIZED_STRATEGY
        }
        assertThrows(IllegalStateException::class.java) { support.firstItem = "first" }
        assertThrows(IllegalStateException::class.java, support::removeFirstItem)
        assertThrows(IllegalStateException::class.java) { support.isInstalled }
        assertThrows(IllegalStateException::class.java, support::uninstall)
        assertNotNull(support.comboBox)
        assertNull(support.firstItem)
        SwingUtilities.invokeAndWait { support.uninstall() }
    }

    private class NonAbstractDocument : Document by PlainDocument()

    private fun <E> JComboBox<E>.items(): List<E?> = (0 until itemCount).map(::getItemAt)

    private fun onEdt(block: () -> Unit) {
        SwingUtilities.invokeAndWait(block)
    }

    private companion object {
        val ACTION_KEYS = listOf(
            "selectNext",
            "selectPrevious",
            "selectNext2",
            "selectPrevious2",
            "aquaSelectNext",
            "aquaSelectPrevious",
        )
    }
}
