package mediathek.gui.toolbar;

import javafx.application.Platform;
import javafx.scene.control.Button;
import javafx.scene.control.ToolBar;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.tool.Listener;
import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.gui.GuiDownloads;
import mediathek.javafx.tool.FilmInformationButton;
import mediathek.javafx.tool.FilterButton;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.GlyphFont;
import org.controlsfx.glyphfont.GlyphFontRegistry;

import javax.swing.*;

public class FXDownloadToolBar extends ToolBar {
    private final Button btnUpdateDownloads;

    public FXDownloadToolBar(GuiDownloads tabDownloads) {
        super();

        Region spacer = new Region();
        HBox.setHgrow(spacer, Priority.ALWAYS);

        Button btnFilmInfo = new FilmInformationButton();
        btnFilmInfo.setOnAction(e -> SwingUtilities.invokeLater(() -> MediathekGui.ui().getFilmInfoDialog().showInfo()));

        GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");

        btnUpdateDownloads = new Button("",fontAwesome.create(FontAwesome.Glyph.REFRESH).size(16d));
        btnUpdateDownloads.setTooltip(new Tooltip("Downloadliste aktualisieren"));
        btnUpdateDownloads.setOnAction(e -> SwingUtilities.invokeLater(tabDownloads::aktualisieren));

        Button btnStartAllDownloads = new Button("",fontAwesome.create(FontAwesome.Glyph.ANGLE_DOUBLE_DOWN).size(16d));
        btnStartAllDownloads.setTooltip(new Tooltip("Alle Downloads starten"));
        btnStartAllDownloads.setOnAction(e -> SwingUtilities.invokeLater(() -> tabDownloads.starten(true)));

        Button btnPlayFilm = new Button("",fontAwesome.create(FontAwesome.Glyph.PLAY).size(16d));
        btnPlayFilm.setTooltip(new Tooltip("Film abspielen"));
        btnPlayFilm.setOnAction(e -> SwingUtilities.invokeLater(tabDownloads::filmAbspielen));

        Button btnZurückstellen = new Button("",fontAwesome.create(FontAwesome.Glyph.CLOCK_ALT).size(16d));
        btnZurückstellen.setTooltip(new Tooltip("Downloads zurückstellen"));
        btnZurückstellen.setOnAction(e -> SwingUtilities.invokeLater(tabDownloads::zurueckstellen));

        Button btnRemoveDownload = new Button("",fontAwesome.create(FontAwesome.Glyph.TRASH_ALT).size(16d));
        btnRemoveDownload.setTooltip(new Tooltip("Downloads entfernen"));
        btnRemoveDownload.setOnAction(e -> SwingUtilities.invokeLater(tabDownloads::loeschen));

        Button btnCleanup = new Button("",fontAwesome.create(FontAwesome.Glyph.ERASER).size(16d));
        btnCleanup.setTooltip(new Tooltip("Liste säubern"));
        btnCleanup.setOnAction(e -> SwingUtilities.invokeLater(tabDownloads::aufraeumen));

        Button btnFilter = new FilterButton();
        btnFilter.setOnAction(e -> SwingUtilities.invokeLater(() -> {
            boolean b = !Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_FILTER_VIS));
            MVConfig.add(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_FILTER_VIS, Boolean.toString(b));
            Listener.notify(Listener.EREIGNIS_PANEL_DOWNLOAD_FILTER_ANZEIGEN, FXDownloadToolBar.class.getName());
        }));

        getItems().addAll(btnFilmInfo,
                btnUpdateDownloads,
                btnStartAllDownloads,
                btnPlayFilm,
                btnZurückstellen,
                btnRemoveDownload,
                btnCleanup,
                spacer,
                btnFilter);

        installFilmlistListener();
    }

    private void installFilmlistListener() {
        Daten.getInstance().getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                Platform.runLater(() -> {
                    if (btnUpdateDownloads != null) {
                        btnUpdateDownloads.setDisable(true);
                    }
                });
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                Platform.runLater(() -> {
                    if (btnUpdateDownloads != null) {
                        btnUpdateDownloads.setDisable(false);
                    }
                });
            }
        });
    }
}
