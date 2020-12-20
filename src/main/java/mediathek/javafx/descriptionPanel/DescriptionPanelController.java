package mediathek.javafx.descriptionPanel;

import javafx.embed.swing.JFXPanel;
import javafx.event.Event;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.text.*;
import mediathek.config.Konstanten;
import mediathek.daten.DatenFilm;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.gui.dialog.DialogFilmBeschreibung;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.GuiFunktionen;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.io.IOException;
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

    private DatenFilm currentFilm;
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

    public static DescriptionPanelController install(JFXPanel fxDescriptionPanel) throws IOException {
        FXMLLoader loader = new FXMLLoader();
        loader.setLocation(Konstanten.FXML_FILM_DESCRIPTION_PANEL_URL);

        TabPane descriptionPane = loader.load();
        final DescriptionPanelController descriptionPanelController = loader.getController();
        descriptionPanelController.setOnCloseRequest(e -> {
            SwingUtilities.invokeLater(() -> fxDescriptionPanel.setVisible(false));
            e.consume();
        });

        fxDescriptionPanel.setScene(new Scene(descriptionPane));
        return descriptionPanelController;
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

    public static String getStringFromTextFlow(TextFlow tf) {
        StringBuilder sb = new StringBuilder();
        tf.getChildren().stream()
                .filter(t -> Text.class.equals(t.getClass()))
                .forEach(t -> sb.append(((Text) t).getText()));
        return sb.toString();
    }

    private ContextMenu createContextMenu() {
        ContextMenu contextMenu = new ContextMenu();

        MenuItem edit = new MenuItem("Beschreibung ändern");
        edit.setOnAction(e -> SwingUtilities.invokeLater(() -> {
        DialogFilmBeschreibung dialog = new DialogFilmBeschreibung(MediathekGui.ui(), currentFilm);
        dialog.setVisible(true);
    }));

        MenuItem copyToClipboard = new MenuItem("In Zwischenablage kopieren");
        copyToClipboard.setOnAction(e -> {
            var text = getStringFromTextFlow(textField);
            GuiFunktionen.copyToClipboard(text);
        });

        var items = contextMenu.getItems();
        items.add(edit);
        items.add(new SeparatorMenuItem());
        items.add(copyToClipboard);

        contextMenu.setOnShowing(e -> {
            //if there is no film do not offer edit or copy capability
            if (currentFilm == null) {
                edit.setDisable(true);
                copyToClipboard.setDisable(true);
            }
            else {
                edit.setDisable(false);
                copyToClipboard.setDisable(false);
            }
        });

        return contextMenu;
    }

    public void setOnCloseRequest(EventHandler<Event> e) {
        descriptionTab.setOnCloseRequest(e);
    }

    public void initialize() {
        websiteLink.setVisible(false);
        setupWebsiteLink();

        contextMenu = createContextMenu();
        scrollPane.setOnContextMenuRequested(event -> contextMenu.show(textField, event.getScreenX(), event.getScreenY()));
    }
}
