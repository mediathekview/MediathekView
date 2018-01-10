package mediathek.javafx;

import javafx.beans.property.StringProperty;
import javafx.scene.Cursor;
import javafx.scene.Node;
import org.controlsfx.control.textfield.CustomTextField;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.GlyphFont;
import org.controlsfx.glyphfont.GlyphFontRegistry;

public class JFXSearchPanel extends CustomTextField {
    public JFXSearchPanel() {
        super();
        init();
    }

    private void init() {
        GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");
        setLeft(fontAwesome.create(FontAwesome.Glyph.SEARCH));
        setRight(fontAwesome.create(FontAwesome.Glyph.REMOVE));

        setOnKeyPressed(event -> {
            switch (event.getCode()) {
                case ESCAPE:
                    if (!getText().isEmpty())
                        setText("");
                    event.consume();
                    break;
            }
        });

        final Node rightNode = getRight();
        rightNode.setOnMouseClicked(evt -> setText(""));
        rightNode.setCursor(Cursor.DEFAULT);
        rightNode.setVisible(false);

        final StringProperty textProperty = textProperty();
        textProperty.addListener((observable, oldValue, newValue) -> {
            if (newValue.isEmpty())
                rightNode.setVisible(false);
            else
                rightNode.setVisible(true);
        });

    }
}
