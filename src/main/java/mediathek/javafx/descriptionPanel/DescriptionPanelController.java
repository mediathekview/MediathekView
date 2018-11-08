package mediathek.javafx.descriptionPanel;

import javafx.event.Event;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.control.MenuItem;
import javafx.scene.input.MouseButton;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import mSearch.daten.DatenFilm;
import mediathek.MediathekGui;
import mediathek.gui.dialog.DialogFilmBeschreibung;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.net.URI;
import java.util.Optional;

/**
 * The controller for the film description panel
 */
public class DescriptionPanelController {
    @FXML
    private Hyperlink websiteLink;

    @FXML
    private WebView webView;

    @FXML
    private Tab descriptionTab;

    private WebEngine webEngine;
    private EventHandler<Event> closeHandler;
    private DatenFilm currentFilm = null;

    private void setupWebsiteLink() {
        websiteLink.setOnAction(e -> {
            try {
                final Desktop desktop = Desktop.getDesktop();
                if (desktop.isSupported(Desktop.Action.BROWSE)) {
                    desktop.browse(new URI(currentFilm.getWebsiteLink()));
                }
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        });
    }

    public void showFilmDescription(@NotNull Optional<DatenFilm> optFilm) {
        optFilm.ifPresentOrElse(film -> {
            currentFilm = film;
            websiteLink.setVisible(true);
            websiteLink.setVisited(false);
            websiteLink.setTooltip(new Tooltip(film.getWebsiteLink()));

            webEngine.setUserStyleSheetLocation(getClass().getResource("/mediathek/res/css/description.css").toString());
            webEngine.loadContent(
                    "<html>"
                            + "<span class=\'headline\'>" + (film.getSender().isEmpty() ? "" : film.getSender() + "  -  ")
                            + film.getTitle() + "</span><br/><br/>"
                            + "<span class=\'description\'>" + film.getDescription() + "</span>"
                            + "</html>");
        }, () -> {
            webEngine.loadContent("");
            websiteLink.setTooltip(null);
            websiteLink.setVisible(false);
            currentFilm = null;
        });
    }

    private void createContextMenu() {
        ContextMenu contextMenu = new ContextMenu();
        MenuItem edit = new MenuItem("Beschreibung ändern");
        edit.setOnAction(e -> {
            SwingUtilities.invokeLater(() -> {
                DialogFilmBeschreibung dialog = new DialogFilmBeschreibung(MediathekGui.ui(), currentFilm);
                dialog.setVisible(true);
            });
        });

        contextMenu.getItems().add(edit);
        contextMenu.setOnShowing(e -> {
            //if there is no film do not offer edit capability
            if (currentFilm == null)
                edit.setDisable(true);
            else
                edit.setDisable(false);
        });

        webView.setOnMousePressed(e -> {
            if (e.getButton() == MouseButton.SECONDARY)
                contextMenu.show(webView, e.getScreenX(), e.getScreenY());
            else
                contextMenu.hide();
        });
    }

    public void setOnCloseRequest(EventHandler<Event> e) {
        closeHandler = e;
        descriptionTab.setOnCloseRequest(evt -> closeHandler.handle(evt));
    }

    public void initialize() {
        websiteLink.setVisible(false);
        setupWebsiteLink();

        webView.setContextMenuEnabled(false);
        createContextMenu();

        webEngine = webView.getEngine();
        webEngine.loadContent("<html></html>");
    }
}
