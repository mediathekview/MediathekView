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
package ca.odell.glazedlists.swing

import ca.odell.glazedlists.TextFilterator
import ca.odell.glazedlists.matchers.TextMatcherEditor
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import javax.swing.JTextArea
import javax.swing.JTextField
import javax.swing.SwingUtilities
import javax.swing.text.AbstractDocument
import javax.swing.text.PlainDocument

internal class TextComponentMatcherEditorBehaviorTest {
    @Test
    fun constructorsInstallTheExpectedListenersAndFilterPreloadedText() = onEdt {
        val field = JTextField("alpha beta")
        val document = field.document as AbstractDocument
        val initialDocumentListeners = document.documentListeners.size
        val initialActionListeners = field.actionListeners.size
        val initialPropertyListeners = field.propertyChangeListeners.size

        val liveEditor = TextComponentMatcherEditor(field, valueFilterator)
        assertTrue(liveEditor.isLive)
        assertSame(valueFilterator, liveEditor.filterator)
        assertEquals(initialDocumentListeners + 1, document.documentListeners.size)
        assertEquals(initialActionListeners, field.actionListeners.size)
        assertEquals(initialPropertyListeners + 1, field.propertyChangeListeners.size)
        assertTrue(liveEditor.matcher.matches("alpha middle beta"))
        assertFalse(liveEditor.matcher.matches("alpha only"))
        liveEditor.dispose()

        val actionEditor = TextComponentMatcherEditor(field, valueFilterator, false)
        assertFalse(actionEditor.isLive)
        assertEquals(initialDocumentListeners, document.documentListeners.size)
        assertEquals(initialActionListeners + 1, field.actionListeners.size)
        assertEquals(initialPropertyListeners + 1, field.propertyChangeListeners.size)
        actionEditor.dispose()

        val documentEditor = TextComponentMatcherEditor<String>(document, valueFilterator)
        assertTrue(documentEditor.isLive)
        assertEquals(initialDocumentListeners + 1, document.documentListeners.size)
        assertEquals(initialPropertyListeners, field.propertyChangeListeners.size)
        assertTrue(documentEditor.matcher.matches("alpha middle beta"))
        documentEditor.dispose()

        assertEquals(initialDocumentListeners, document.documentListeners.size)
        assertEquals(initialActionListeners, field.actionListeners.size)
        assertEquals(initialPropertyListeners, field.propertyChangeListeners.size)
    }

    @Test
    fun liveAndActionModesRefilterOnlyOnTheirConfiguredTrigger() = onEdt {
        val field = JTextField()
        val document = field.document as AbstractDocument
        val editor = TextComponentMatcherEditor(field, valueFilterator, false)
        try {
            field.text = "beta"
            assertTrue(editor.matcher.matches("anything"))
            field.postActionEvent()
            assertTrue(editor.matcher.matches("contains beta"))
            assertFalse(editor.matcher.matches("contains gamma"))

            editor.isLive = true
            assertEquals(0, field.actionListeners.count { it.javaClass.name.contains("FilterHandler") })
            assertTrue(document.documentListeners.any { it.javaClass.name.contains("FilterHandler") })
            field.text = "gamma"
            assertFalse(editor.matcher.matches("contains beta"))
            assertTrue(editor.matcher.matches("contains gamma"))

            editor.isLive = false
            field.text = "delta"
            assertTrue(editor.matcher.matches("contains gamma"))
            assertFalse(editor.matcher.matches("contains delta"))
            field.postActionEvent()
            assertTrue(editor.matcher.matches("contains delta"))
        } finally {
            editor.dispose()
        }
    }

    @Test
    fun documentSwapMovesListenersAndImmediatelyRefilters() = onEdt {
        val field = JTextField()
        val first = field.document as AbstractDocument
        first.insertString(0, "first", null)
        val second = PlainDocument().apply { insertString(0, "second", null) }
        val firstFilterHandlerCount = first.filterHandlerCount()
        val secondFilterHandlerCount = second.filterHandlerCount()
        val editor = TextComponentMatcherEditor(field, valueFilterator, true)
        try {
            assertTrue(editor.matcher.matches("first value"))
            field.document = second
            assertEquals(firstFilterHandlerCount, first.filterHandlerCount())
            assertEquals(secondFilterHandlerCount + 1, second.filterHandlerCount())
            assertFalse(editor.matcher.matches("first value"))
            assertTrue(editor.matcher.matches("second value"))

            second.replace(0, second.length, "third", null)
            assertTrue(editor.matcher.matches("third value"))
        } finally {
            editor.dispose()
        }
        assertEquals(secondFilterHandlerCount, second.filterHandlerCount())
    }

    @Test
    fun matchingModeControlsWhitespaceTokenization() = onEdt {
        val field = JTextField("alpha beta")
        val editor = TextComponentMatcherEditor(field, valueFilterator)
        try {
            assertTrue(editor.matcher.matches("alpha middle beta"))
            editor.mode = TextMatcherEditor.STARTS_WITH
            field.text = "alpha beta"
            assertTrue(editor.matcher.matches("alpha beta suffix"))
            assertFalse(editor.matcher.matches("prefix alpha beta"))

            editor.mode = TextMatcherEditor.EXACT
            field.text = "alpha beta"
            assertTrue(editor.matcher.matches("alpha beta"))
            assertFalse(editor.matcher.matches("alpha beta suffix"))
        } finally {
            editor.dispose()
        }
    }

    @Test
    fun containsModeRetainsJavaSplitLeadingInteriorAndTrailingEmptySemantics() = onEdt {
        val field = JTextField()
        val editor = RecordingEditor(field)
        try {
            field.text = " alpha  beta\t"
            assertArrayEquals(arrayOf("", "alpha", "", "beta"), editor.lastFilters)
        } finally {
            editor.dispose()
        }
    }

    @Test
    fun refilterReadsTheOverridableModeBeforeReadingDocumentText() = onEdt {
        dispatchTrace.clear()
        val editor = DispatchOrderEditor(DispatchOrderDocument())
        try {
            assertEquals(listOf("mode", "text", "setFilterText", "mode"), dispatchTrace)
        } finally {
            editor.dispose()
        }
    }

    @Test
    fun invalidNonLiveTransitionsRetainTheirJavaPartialFailureState() = onEdt {
        val areaFailure = assertThrows(IllegalArgumentException::class.java) {
            TextComponentMatcherEditor<String>(JTextArea(), valueFilterator, false)
        }
        assertTrue(areaFailure.message!!.contains(JTextArea::class.java.name))

        val document = PlainDocument()
        val initialListeners = document.documentListeners.size
        val editor = TextComponentMatcherEditor<String>(document, valueFilterator)
        assertThrows(IllegalArgumentException::class.java) { editor.isLive = false }
        assertFalse(editor.isLive)
        assertEquals(initialListeners, document.documentListeners.size)
        assertThrows(NullPointerException::class.java) { editor.dispose() }
    }

    @Test
    fun disposeDetachesBothLiveAndActionEditorsAndIsRepeatable() = onEdt {
        for (live in listOf(true, false)) {
            val field = JTextField()
            val document = field.document as AbstractDocument
            val documentCount = document.documentListeners.size
            val actionCount = field.actionListeners.size
            val propertyCount = field.propertyChangeListeners.size
            val editor = TextComponentMatcherEditor(field, valueFilterator, live)

            field.text = "before"
            field.postActionEvent()
            assertTrue(editor.matcher.matches("before value"))
            editor.dispose()
            editor.dispose()

            assertEquals(documentCount, document.documentListeners.size)
            assertEquals(actionCount, field.actionListeners.size)
            assertEquals(propertyCount, field.propertyChangeListeners.size)
            field.text = "after"
            field.postActionEvent()
            assertTrue(editor.matcher.matches("before value"))
            assertFalse(editor.matcher.matches("after value"))
        }
    }

    @Test
    fun changingModeAfterDisposalReinstallsTheConfiguredListeners() = onEdt {
        val field = JTextField()
        val actionCount = field.actionListeners.size
        val propertyCount = field.propertyChangeListeners.size
        val editor = TextComponentMatcherEditor(field, valueFilterator, true)

        editor.dispose()
        editor.isLive = false
        assertEquals(actionCount + 1, field.actionListeners.size)
        assertEquals(propertyCount + 1, field.propertyChangeListeners.size)

        field.text = "resurrected"
        field.postActionEvent()
        assertTrue(editor.matcher.matches("resurrected value"))
        editor.dispose()
    }

    private fun onEdt(action: () -> Unit) {
        if (SwingUtilities.isEventDispatchThread()) action() else SwingUtilities.invokeAndWait(action)
    }

    private fun AbstractDocument.filterHandlerCount(): Int =
        documentListeners.count { it.javaClass.name.contains("TextComponentMatcherEditor\$FilterHandler") }

    private class RecordingEditor(field: JTextField) :
        TextComponentMatcherEditor<String>(field, valueFilterator) {
        var lastFilters: Array<String>? = null

        override fun setFilterText(newFilters: Array<String>) {
            lastFilters = newFilters.copyOf()
            super.setFilterText(newFilters)
        }
    }

    private class DispatchOrderDocument : PlainDocument() {
        override fun getText(offset: Int, length: Int): String {
            dispatchTrace += "text"
            return super.getText(offset, length)
        }
    }

    private class DispatchOrderEditor(document: PlainDocument) :
        TextComponentMatcherEditor<String>(document, valueFilterator) {
        override var mode: Int
            get() {
                dispatchTrace += "mode"
                return super.mode
            }
            set(value) {
                super.mode = value
            }

        override fun setFilterText(newFilters: Array<String>) {
            dispatchTrace += "setFilterText"
            super.setFilterText(newFilters)
        }
    }

    private companion object {
        val dispatchTrace = mutableListOf<String>()
        val valueFilterator = TextFilterator<String> { values, element -> values += element }
    }
}
