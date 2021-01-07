package mediathek.javafx.filterpanel

import javafx.beans.value.ObservableValue
import javafx.event.EventHandler
import javafx.scene.Cursor
import javafx.scene.input.KeyCode
import javafx.scene.input.KeyEvent
import org.controlsfx.control.textfield.CustomTextField
import org.controlsfx.glyphfont.FontAwesome
import org.controlsfx.glyphfont.GlyphFontRegistry

class JFXSearchPanel : CustomTextField() {
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
    }
}