package mediathek.javafx.descriptionPanel;

import javafx.event.Event;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.scene.control.MenuItem;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.*;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.*;
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
    private TextFlow textField;
    @FXML
    private Tab descriptionTab;

    @FXML
    private ScrollPane scrollPane;

    private EventHandler<Event> closeHandler;
    private DatenFilm currentFilm = null;
    private ContextMenu contextMenu;

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
        textField.getChildren().clear();

        optFilm.ifPresentOrElse(film -> {
            currentFilm = film;
            websiteLink.setVisible(true);
            websiteLink.setVisited(false);
            websiteLink.setTooltip(new Tooltip(film.getWebsiteLink()));

            Font defaultFont = Font.getDefault();
            Text headLine = new Text((film.getSender().isEmpty() ? "" : film.getSender() + "  -  ") + film.getTitle());
            headLine.setFont(Font.font(defaultFont.getName(), FontWeight.BOLD, FontPosture.REGULAR, defaultFont.getSize()));
            headLine.setFill(Color.RED);

            Text description = new Text(film.getDescription());
            description.setFont(Font.font(defaultFont.getName(), FontWeight.NORMAL, FontPosture.REGULAR, defaultFont.getSize()));

            textField.getChildren().addAll(headLine,
                    new Text("\n"),
                    new Text("\n"),
                    description);
        }, () -> {
            websiteLink.setTooltip(null);
            websiteLink.setVisible(false);
            currentFilm = null;
        });
    }

    private ContextMenu createContextMenu() {
        ContextMenu contextMenu = new ContextMenu();
        MenuItem edit = new MenuItem("Beschreibung ändern");
        edit.setOnAction(e -> SwingUtilities.invokeLater(() -> {
            DialogFilmBeschreibung dialog = new DialogFilmBeschreibung(MediathekGui.ui(), currentFilm);
            dialog.setVisible(true);
        }));

        contextMenu.getItems().add(edit);
        contextMenu.setOnShowing(e -> {
            //if there is no film do not offer edit capability
            if (currentFilm == null)
                edit.setDisable(true);
            else
                edit.setDisable(false);
        });

        return contextMenu;
    }

    public void setOnCloseRequest(EventHandler<Event> e) {
        closeHandler = e;
        descriptionTab.setOnCloseRequest(evt -> closeHandler.handle(evt));
    }

    public void initialize() {
        websiteLink.setVisible(false);
        setupWebsiteLink();

        contextMenu = createContextMenu();
        scrollPane.setOnContextMenuRequested(event -> contextMenu.show(textField, event.getScreenX(), event.getScreenY()));
    }
}
