package mediathek.javafx.descriptionPanel;

import javafx.event.Event;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.text.*;
import mSearch.daten.DatenFilm;
import mediathek.MediathekGui;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.gui.dialog.DialogFilmBeschreibung;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.net.URISyntaxException;
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
    private static final Logger logger = LogManager.getLogger(DescriptionPanelController.class);

    private void setupWebsiteLink() {
        websiteLink.setOnAction(e -> {
            final var link = currentFilm.getWebsiteLink();
            SwingUtilities.invokeLater(() -> {
                try {
                    UrlHyperlinkAction.openURL(MediathekGui.ui(), link);
                } catch (URISyntaxException ex) {
                    logger.error("Failed to launch web browser for URL: {}", link);
                }
            });
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
