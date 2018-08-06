package mediathek.javafx;

import javafx.scene.control.Button;
import javafx.scene.control.Tooltip;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.GlyphFont;
import org.controlsfx.glyphfont.GlyphFontRegistry;

/**
 * a JavaFX button which will simply perform the garbage collection when clicked
 */
public class GarbageCollectionButton extends Button {
    private static final GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");

    public GarbageCollectionButton() {
        super("", fontAwesome.create(FontAwesome.Glyph.RECYCLE));
        setText("");
        setTooltip(new Tooltip("Garbage Collection durchführen"));
        setOnAction(e -> System.gc());
    }
}
