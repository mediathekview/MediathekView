package mediathek.javafx.filterpanel

import javafx.beans.value.ObservableValue
import javafx.event.EventHandler
import javafx.scene.Cursor
import javafx.scene.control.Tooltip
import javafx.scene.input.KeyCode
import javafx.scene.input.KeyEvent
import mediathek.tool.Filter
import org.controlsfx.control.textfield.CustomTextField
import org.controlsfx.glyphfont.FontAwesome
import org.controlsfx.glyphfont.GlyphFontRegistry

class FXSearchControl : CustomTextField() {

    companion object {
        private const val PROMPT_THEMA_TITEL = "Thema/Titel"
        private const val PROMPT_IRGENDWO = "Thema/Titel/Beschreibung"
    }

    private val themaTitelTooltip = Tooltip("Thema/Titel durchsuchen")
    private val irgendwoTooltip = Tooltip("Thema/Titel/Beschreibung durchsuchen")

    fun setMode(mode: FXSearchControlFieldMode) {
        when (mode) {
            FXSearchControlFieldMode.THEMA_TITEL -> {
                tooltip = themaTitelTooltip
                promptText = PROMPT_THEMA_TITEL
            }
            FXSearchControlFieldMode.IRGENDWO -> {
                tooltip = irgendwoTooltip
                promptText = PROMPT_IRGENDWO
            }
        }
    }

    init {
        val fontAwesome = GlyphFontRegistry.font("FontAwesome")
        left = fontAwesome.create(FontAwesome.Glyph.SEARCH)
        right = fontAwesome.create(FontAwesome.Glyph.REMOVE)
        onKeyPressed = EventHandler { event: KeyEvent ->
            if (event.code == KeyCode.ESCAPE) {
                if (text.isNotEmpty())
                    text = ""
                event.consume()
            }
        }

        val rightNode = right
        rightNode.onMouseClicked = EventHandler { text = "" }
        rightNode.cursor = Cursor.DEFAULT
        rightNode.isVisible = false

        val textProperty = textProperty()
        textProperty.addListener { _: ObservableValue<out String>?, _: String?, newValue: String ->
            rightNode.isVisible = newValue.isNotEmpty()
        }

        prefWidth = 350.0
        minWidth = 350.0
        maxWidth = 350.0

        textProperty.addListener { _, _, newValue -> checkPatternValidity(newValue) }
    }

    private fun checkPatternValidity(text: String) {
        style = if (Filter.isPattern(text)) {
            if (Filter.makePatternNoCache(text) == null) {
                // invalid pattern
                "-fx-text-fill: red"
            } else {
                //valid pattern
                "-fx-text-fill: blue"
            }
        } else {
            // regular search text, reset to default style
            null
        }
    }
}