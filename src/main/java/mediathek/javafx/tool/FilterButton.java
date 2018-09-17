package mediathek.javafx.tool;

import javafx.scene.control.Button;
import javafx.scene.control.Tooltip;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.GlyphFont;
import org.controlsfx.glyphfont.GlyphFontRegistry;

public class FilterButton extends Button {
    public FilterButton() {
        super();
        GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");
        setGraphic(fontAwesome.create(FontAwesome.Glyph.FILTER).size(16d));
        setTooltip(new Tooltip("Filter anzeigen/ausblenden"));
    }
}
