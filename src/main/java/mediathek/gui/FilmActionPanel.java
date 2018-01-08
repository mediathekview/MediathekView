package mediathek.gui;

import javafx.animation.PauseTransition;
import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.StringProperty;
import javafx.collections.ObservableList;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.geometry.Pos;
import javafx.scene.Cursor;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.util.Duration;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.tool.Listener;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.tool.Filter;
import org.controlsfx.control.Notifications;
import org.controlsfx.control.PopOver;
import org.controlsfx.control.textfield.CustomTextField;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.GlyphFont;
import org.controlsfx.glyphfont.GlyphFontRegistry;

import javax.swing.*;

/**
 * This class sets up the GuiFilme tool panel and search bar.
 * search is exposed via a readonly property for filtering in GuiFilme.
 */
public class FilmActionPanel {
    private final Daten daten;
    public ReadOnlyStringWrapper roSearchStringProperty = new ReadOnlyStringWrapper();
    private CustomTextField jfxSearchField;


    public FilmActionPanel(Daten daten) {
        this.daten = daten;

        filterPopover = createFilterPopover();
    }

    private Parent createLeft() {
        GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");

        Button btnDownload = new Button("", fontAwesome.create(FontAwesome.Glyph.CLOUD_DOWNLOAD));
        btnDownload.setTooltip(new Tooltip("Neue Filmliste laden"));
        btnDownload.setOnAction(e -> SwingUtilities.invokeLater(() -> daten.getFilmeLaden().loadFilmlistDialog(daten, false)));
        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                Platform.runLater(() -> btnDownload.setDisable(true));
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                Platform.runLater(() -> btnDownload.setDisable(false));
            }
        });

        Button btnFilmInformation = new Button("", fontAwesome.create(FontAwesome.Glyph.INFO_CIRCLE));
        btnFilmInformation.setTooltip(new Tooltip("Filminformation anzeigen"));
        btnFilmInformation.setOnAction(e -> SwingUtilities.invokeLater(Daten.filmInfo::showInfo));

        HBox hb = new HBox();
        hb.setPadding(new Insets(5, 5, 5, 5));
        hb.setSpacing(4.0);
        hb.setAlignment(Pos.CENTER_LEFT);

        ObservableList<Node> list = hb.getChildren();
        list.add(btnDownload);

        Separator separator = new Separator();
        separator.setOrientation(Orientation.VERTICAL);
        list.add(separator);

        list.add(btnFilmInformation);

        Separator separator2 = new Separator();
        separator2.setOrientation(Orientation.VERTICAL);
        list.add(separator2);

        Button btnPlay = new Button("", fontAwesome.create(FontAwesome.Glyph.PLAY));
        btnPlay.setTooltip(new Tooltip("Film abspielen"));
        btnPlay.setOnAction(e -> SwingUtilities.invokeLater(Daten.guiFilme::guiFilmeFilmAbspielen));
        list.add(btnPlay);

        Button btnRecord = new Button("", fontAwesome.create(FontAwesome.Glyph.DOWNLOAD));
        btnRecord.setOnAction(e -> SwingUtilities.invokeLater(Daten.guiFilme::guiFilmeFilmSpeichern));
        btnRecord.setTooltip(new Tooltip("Film aufzeichnen"));
        list.add(btnRecord);

        return hb;
    }

    private void checkPatternValidity() {
        jfxSearchField.setStyle("-fx-text-fill: red");

        // Schriftfarbe ändern wenn eine RegEx
        String text = jfxSearchField.getText();
        if (Filter.isPattern(text)) {
            if (Filter.makePattern(text) == null) {
                //soll Pattern sein, ist aber falsch
                jfxSearchField.setStyle("-fx-text-fill: red");
            } else {
                jfxSearchField.setStyle("-fx-text-fill: blue");
            }
        } else {
            jfxSearchField.setStyle("-fx-text-fill: black");
        }
    }

    private void setupSearchFieldVisibility() {
        Platform.runLater(() -> jfxSearchField.setVisible(!Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_VIS_FILTER))));
    }

    private Parent createRight() {
        GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");
        jfxSearchField = new CustomTextField();
        jfxSearchField.setTooltip(new Tooltip("Thema/Titel suchen"));

        jfxSearchField.setLeft(fontAwesome.create(FontAwesome.Glyph.SEARCH));

        jfxSearchField.setRight(fontAwesome.create(FontAwesome.Glyph.REMOVE));

        Listener.addListener(new Listener(Listener.EREIGNIS_PANEL_FILTER_ANZEIGEN, ToolBar.class.getSimpleName()) {
            @Override
            public void ping() {
                setupSearchFieldVisibility();
            }
        });
        final Node rightNode = jfxSearchField.getRight();
        rightNode.setOnMouseClicked(evt -> jfxSearchField.setText(""));
        rightNode.setCursor(Cursor.DEFAULT);
        rightNode.setVisible(false);

        final StringProperty textProperty = jfxSearchField.textProperty();
        textProperty.addListener((observable, oldValue, newValue) -> {
            final Node icon = jfxSearchField.getRight();
            if (newValue.isEmpty())
                icon.setVisible(false);
            else
                icon.setVisible(true);
        });
        PauseTransition pause2 = new PauseTransition(Duration.millis(150));
        textProperty.addListener((observable, oldValue, newValue) -> {
            pause2.setOnFinished(evt -> checkPatternValidity());
            pause2.playFromStart();
        });
        PauseTransition pause3 = new PauseTransition(Duration.millis(500));
        textProperty.addListener((observable, oldValue, newValue) -> {
            pause3.setOnFinished(evt -> SwingUtilities.invokeLater(Daten.guiFilme::guiFilmeFiltern));
            pause3.playFromStart();
        });

        roSearchStringProperty.bind(textProperty);


        ToggleButton btnAdvancedFilter = new ToggleButton("", fontAwesome.create(FontAwesome.Glyph.FILTER));
        btnAdvancedFilter.setOnAction(event -> {
            Notifications.create()
                    .title("Download abgeschlossen")
                    .text("Und wir sind fertig :-)\nIst aber nur ein Testdialog")
                    .showInformation();

            SwingUtilities.invokeLater(() -> {
                boolean b = !Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_VIS_FILTER));
                MVConfig.add(MVConfig.Configs.SYSTEM_VIS_FILTER, Boolean.toString(b));
                setupSearchFieldVisibility();
                Listener.notify(Listener.EREIGNIS_PANEL_FILTER_ANZEIGEN, ToolBar.class.getName());
            });
        });

        HBox hb = new HBox();
        hb.setPadding(new Insets(5, 5, 5, 5));
        hb.setSpacing(4);
        hb.setAlignment(Pos.CENTER_RIGHT);

        Button popOverTest = new Button("", fontAwesome.create(FontAwesome.Glyph.FILTER));
        popOverTest.setOnAction(e -> filterPopover.show(popOverTest));
        ObservableList<Node> list = hb.getChildren();
        list.add(popOverTest);
        Separator separator = new Separator();
        separator.setOrientation(Orientation.VERTICAL);
        list.add(separator);
        list.add(jfxSearchField);
        list.add(btnAdvancedFilter);

        setupSearchFieldVisibility();

        return hb;
    }

    private final PopOver filterPopover;

    public BooleanProperty showOnlyHd;

    public PopOver createFilterPopover() {
        VBox vBox = new VBox();
        vBox.setSpacing(4.0);
        CheckBox cbShowOnlyHd = new CheckBox("Nur HD-Filme anzeigen");
        showOnlyHd = cbShowOnlyHd.selectedProperty();
        vBox.getChildren().add(cbShowOnlyHd);
        vBox.getChildren().add(new CheckBox("Nur Filme mit Untertitel anzeigen"));
        vBox.getChildren().add(new CheckBox("Nur neue Filme anzeigen"));
        vBox.getChildren().add(new CheckBox("Gesehene Filme ausblenden"));
        vBox.getChildren().add(new CheckBox("Abos ausblenden"));

        TitledPane t1 = new TitledPane("Allgemeine Anzeigeeinstellungen", vBox);
        TitledPane t2 = new TitledPane("T2", new Button("B2"));
        TitledPane t3 = new TitledPane("Filterprofile", new Button("B3"));
        Accordion accordion = new Accordion();
        accordion.getPanes().addAll(t1, t2, t3);

        PopOver popover = new PopOver();
        popover.setTitle("Erweiterte Filtereinstellungen");
        popover.setAnimated(true);
        popover.setCloseButtonEnabled(true);
        popover.setDetachable(true);
        popover.setArrowLocation(PopOver.ArrowLocation.TOP_RIGHT);

        VBox vb = new VBox();
        vb.setSpacing(4.0);
        vb.setPadding(new Insets(5, 5, 5, 5));
        vb.getChildren().addAll(accordion);
        popover.setContentNode(vb);

        return popover;
    }

    public Scene getFilmActionPanelScene() {
        HBox hb = new HBox();
        Region spacer = new Region();
        HBox.setHgrow(spacer, Priority.ALWAYS);
        hb.getChildren().addAll(createLeft(), spacer, createRight());

        return new Scene(hb);
    }
}
