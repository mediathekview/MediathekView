package mediathek.javafx.filterpanel;

import javafx.beans.property.StringProperty;
import javafx.scene.Cursor;
import javafx.scene.Node;
import javafx.scene.input.KeyCode;
import org.controlsfx.control.textfield.CustomTextField;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.GlyphFont;
import org.controlsfx.glyphfont.GlyphFontRegistry;

public class JFXSearchPanel extends CustomTextField {
    public JFXSearchPanel() {
        super();
        init();

        setPrefWidth(350d);
        setMinWidth(350d);
        setMaxWidth(350d);
    }

    private void init() {
        GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");
        setLeft(fontAwesome.create(FontAwesome.Glyph.SEARCH));
        setRight(fontAwesome.create(FontAwesome.Glyph.REMOVE));

        setOnKeyPressed(event -> {
            if (event.getCode() == KeyCode.ESCAPE) {
                if (!getText().isEmpty())
                    setText("");
                event.consume();
            }
        });

        final Node rightNode = getRight();
        rightNode.setOnMouseClicked(evt -> setText(""));
        rightNode.setCursor(Cursor.DEFAULT);
        rightNode.setVisible(false);

        final StringProperty textProperty = textProperty();
        textProperty.addListener((observable, oldValue, newValue) -> rightNode.setVisible(!newValue.isEmpty()));

    }
}
