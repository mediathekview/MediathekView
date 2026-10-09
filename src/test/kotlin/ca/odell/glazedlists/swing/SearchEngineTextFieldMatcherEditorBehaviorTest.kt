package ca.odell.glazedlists.swing

import ca.odell.glazedlists.TextFilterator
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import javax.swing.JTextField
import javax.swing.SwingUtilities

internal class SearchEngineTextFieldMatcherEditorBehaviorTest {
    @Test
    fun initialTextActionsAndDisposalRetainFilteringBehavior() {
        onEdt {
            val textField = JTextField("alpha")
            val initialListenerCount = textField.actionListeners.size
            val editor = SearchEngineTextFieldMatcherEditor(textField, valueFilterator)

            assertEquals(initialListenerCount + 1, textField.actionListeners.size)
            assertTrue(editor.matcher.matches("alpha news"))
            assertFalse(editor.matcher.matches("beta news"))

            textField.text = "beta"
            assertTrue(editor.matcher.matches("alpha news"))
            assertFalse(editor.matcher.matches("beta news"))

            textField.postActionEvent()
            assertFalse(editor.matcher.matches("alpha news"))
            assertTrue(editor.matcher.matches("beta news"))

            editor.dispose()
            assertEquals(initialListenerCount, textField.actionListeners.size)

            textField.text = "gamma"
            textField.postActionEvent()
            assertTrue(editor.matcher.matches("beta news"))
            assertFalse(editor.matcher.matches("gamma news"))
        }
    }

    private fun onEdt(action: () -> Unit) {
        if (SwingUtilities.isEventDispatchThread()) {
            action()
        } else {
            SwingUtilities.invokeAndWait(action)
        }
    }

    private companion object {
        val valueFilterator = TextFilterator<String> { values, element -> values += element }
    }
}
