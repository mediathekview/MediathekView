package mediathek.gui.toolbar;

import javafx.scene.control.Button;
import javafx.scene.control.ToolBar;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.GlyphFont;
import org.controlsfx.glyphfont.GlyphFontRegistry;

public class FXDownloadToolBar extends ToolBar {
    public final Button btnUpdateDownloads;
    public final Button btnFilmInfo;
    public final Button btnStartAllDownloads;
    public final Button btnPlayFilm;
    public final Button btnZurueckstellen;
    public final Button btnRemoveDownload;
    public final Button btnCleanup;
    public final Button btnFilter;

    public FXDownloadToolBar() {
        super();

        GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");

        btnFilmInfo = new Button("", fontAwesome.create(FontAwesome.Glyph.INFO_CIRCLE).size(16d));
        btnFilmInfo.setTooltip(new Tooltip("Filminformation anzeigen"));

        btnUpdateDownloads = new Button("",fontAwesome.create(FontAwesome.Glyph.REFRESH).size(16d));
        btnUpdateDownloads.setTooltip(new Tooltip("Downloadliste aktualisieren"));

        btnStartAllDownloads = new Button("",fontAwesome.create(FontAwesome.Glyph.ANGLE_DOUBLE_DOWN).size(16d));
        btnStartAllDownloads.setTooltip(new Tooltip("Alle Downloads starten"));

        btnPlayFilm = new Button("",fontAwesome.create(FontAwesome.Glyph.PLAY).size(16d));
        btnPlayFilm.setTooltip(new Tooltip("Film abspielen"));

        btnZurueckstellen = new Button("",fontAwesome.create(FontAwesome.Glyph.CLOCK_ALT).size(16d));
        btnZurueckstellen.setTooltip(new Tooltip("Downloads zurückstellen"));

        btnRemoveDownload = new Button("",fontAwesome.create(FontAwesome.Glyph.TRASH_ALT).size(16d));
        btnRemoveDownload.setTooltip(new Tooltip("Downloads entfernen"));

        btnCleanup = new Button("",fontAwesome.create(FontAwesome.Glyph.ERASER).size(16d));
        btnCleanup.setTooltip(new Tooltip("Liste säubern"));

        btnFilter = new Button("", fontAwesome.create(FontAwesome.Glyph.FILTER).size(16d));
        btnFilter.setTooltip(new Tooltip("Filter anzeigen/ausblenden"));

        Region spacer = new Region();
        HBox.setHgrow(spacer, Priority.ALWAYS);

        getItems().addAll(btnFilmInfo,
                btnUpdateDownloads,
                btnStartAllDownloads,
                btnPlayFilm,
                btnZurueckstellen,
                btnRemoveDownload,
                btnCleanup,
                spacer,
                btnFilter);
    }
}
