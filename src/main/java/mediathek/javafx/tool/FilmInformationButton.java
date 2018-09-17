package mediathek.javafx.tool;

import javafx.scene.control.Button;
import javafx.scene.control.Tooltip;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.GlyphFont;
import org.controlsfx.glyphfont.GlyphFontRegistry;

public class FilmInformationButton extends Button {

    public FilmInformationButton() {
        super();
        GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");
        setGraphic(fontAwesome.create(FontAwesome.Glyph.INFO_CIRCLE).size(16d));
        setTooltip(new Tooltip("Filminformation anzeigen"));
    }
}
