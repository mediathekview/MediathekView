package mediathek.gui;

import javafx.animation.PauseTransition;
import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.StringProperty;
import javafx.collections.ObservableList;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.util.Duration;
import javafx.util.StringConverter;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.tool.Listener;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.javafx.JFXSearchPanel;
import mediathek.tool.Filter;
import org.controlsfx.control.PopOver;
import org.controlsfx.control.RangeSlider;
import org.controlsfx.control.textfield.CustomTextField;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.GlyphFont;
import org.controlsfx.glyphfont.GlyphFontRegistry;
import org.controlsfx.tools.Borders;

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

    private void setupSearchField() {
        jfxSearchField = new JFXSearchPanel();
        jfxSearchField.setTooltip(new Tooltip("Thema/Titel durchsuchen"));

        final StringProperty textProperty = jfxSearchField.textProperty();

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
    }

    private Parent createRight() {
        GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");

        setupSearchField();

        Button btnAdvancedFilter = new Button("", fontAwesome.create(FontAwesome.Glyph.QUESTION_CIRCLE));
        btnAdvancedFilter.setOnAction(event -> SwingUtilities.invokeLater(() -> {
            boolean b = !Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_VIS_FILTER));
            //FIXME VIS_FILTER kann entfernt werden oder?
            MVConfig.add(MVConfig.Configs.SYSTEM_VIS_FILTER, Boolean.toString(b));
            Listener.notify(Listener.EREIGNIS_PANEL_FILTER_ANZEIGEN, ToolBar.class.getName());
        }));
        btnAdvancedFilter.setTooltip(new Tooltip("Altes Filterpanel anzeigen"));

        HBox hb = new HBox();
        hb.setPadding(new Insets(5, 5, 5, 5));
        hb.setSpacing(4);
        hb.setAlignment(Pos.CENTER_RIGHT);

        Button popOverTest = new Button("", fontAwesome.create(FontAwesome.Glyph.FILTER));
        popOverTest.setOnAction(e -> filterPopover.show(popOverTest));
        ObservableList<Node> list = hb.getChildren();

        BlacklistButton btnBlacklist = new BlacklistButton();
        list.add(btnBlacklist);
        Separator sep2 = new Separator();
        sep2.setOrientation(Orientation.VERTICAL);
        list.add(sep2);
        list.add(popOverTest);
        Separator separator = new Separator();
        separator.setOrientation(Orientation.VERTICAL);
        list.add(separator);
        list.add(jfxSearchField);
        list.add(btnAdvancedFilter);

        return hb;
    }

    public class BlacklistButton extends Button {
        private final Image offImage = new Image(getClass().getResourceAsStream("/mediathek/res/programm/button-blacklist-aus.png"));
        private final ImageView offImageView = new ImageView(offImage);
        private final Image onImage = new Image(getClass().getResourceAsStream("/mediathek/res/programm/button-blacklist-ein.png"));
        private final ImageView onImageView = new ImageView(onImage);
        private final BooleanProperty activeProperty = new SimpleBooleanProperty(false);
        private final Tooltip tooltipOn = new Tooltip("Blacklist ausschalten");
        private final Tooltip tooltipOff = new Tooltip("Blacklist einschalten");


        public BlacklistButton() {
            super("");

            //set initial state
            activeProperty.addListener((observable, oldValue, newValue) -> {
                if (newValue) {
                    setGraphic(onImageView);
                    setTooltip(tooltipOn);
                } else {
                    setGraphic(offImageView);
                    setTooltip(tooltipOff);
                }
            });
            final boolean storedVal = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ON));
            activeProperty.setValue(storedVal);
            activeProperty.addListener((observable, oldValue, newValue) -> SwingUtilities.invokeLater(() -> {
                MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_ON, Boolean.toString(newValue));
                daten.getListeBlacklist().filterListe();
                Listener.notify(Listener.EREIGNIS_BLACKLIST_GEAENDERT, MVFilterPanel.class.getSimpleName());
            }));

            setOnAction(value -> activeProperty.setValue(!activeProperty.getValue()));
        }
    }

    public final PopOver filterPopover;

    public BooleanProperty showOnlyHd;

    public BooleanProperty showSubtitlesOnly;

    public BooleanProperty showNewOnly;

    public BooleanProperty showUnseenOnly;

    public BooleanProperty dontShowAbos;

    private TitledPane createCommonViewSettingsPane() {
        VBox vBox = new VBox();
        vBox.setSpacing(4.0);

        CheckBox cbShowOnlyHd = new CheckBox("Nur HD-Filme anzeigen");
        showOnlyHd = cbShowOnlyHd.selectedProperty();
        vBox.getChildren().add(cbShowOnlyHd);

        CheckBox cbShowSubtitlesOnly = new CheckBox("Nur Filme mit Untertitel anzeigen");
        showSubtitlesOnly = cbShowSubtitlesOnly.selectedProperty();
        vBox.getChildren().add(cbShowSubtitlesOnly);

        CheckBox cbShowNewOnly = new CheckBox("Nur neue Filme anzeigen");
        showNewOnly = cbShowNewOnly.selectedProperty();
        vBox.getChildren().add(cbShowNewOnly);

        CheckBox cbShowUnseenOnly = new CheckBox("Gesehene Filme ausblenden");
        showUnseenOnly = cbShowUnseenOnly.selectedProperty();
        vBox.getChildren().add(cbShowUnseenOnly);

        CheckBox cbDontShowAbos = new CheckBox("Abos ausblenden");
        dontShowAbos = cbDontShowAbos.selectedProperty();
        vBox.getChildren().add(cbDontShowAbos);

        CheckBox cbDontShowImpairedFilms = new CheckBox("Hörfassungen etc ausblenden");
        cbDontShowImpairedFilms.setDisable(true);
        vBox.getChildren().add(cbDontShowImpairedFilms);


        vBox.getChildren().add(createFilmLengthSlider());

        return new TitledPane("Allgemeine Anzeigeeinstellungen", vBox);
    }

    private Node createFilmLengthSlider() {
        HBox hb = new HBox();
        hb.getChildren().add(new Label("Mindestlänge:"));
        Label lblMin = new Label("min");
        hb.getChildren().add(lblMin);

        HBox hb2 = new HBox();
        hb2.getChildren().add(new Label("Maximallänge:"));
        Label lblMax = new Label("max");
        hb2.getChildren().add(lblMax);
        VBox vb2 = new VBox();
        vb2.getChildren().add(hb);
        vb2.getChildren().add(hb2);

        final RangeSlider hSlider = new RangeSlider(0, 110, 10, 90);
        hSlider.setShowTickMarks(true);
        hSlider.setShowTickLabels(true);
        hSlider.setBlockIncrement(1);
        hSlider.setMajorTickUnit(10);
        hSlider.setLabelFormatter(new StringConverter<Number>() {
            @Override
            public String toString(Number object) {
                if (object.intValue() == 110)
                    return "∞";
                else
                    return String.valueOf(object.intValue());
            }

            @Override
            public Number fromString(String string) {
                return Double.parseDouble(string);
            }
        });
        lblMin.setText(String.valueOf((int) hSlider.getLowValue()));
        lblMax.setText(String.valueOf((int) hSlider.getHighValue()));
        hSlider.lowValueProperty().addListener((observable, oldValue, newValue) -> lblMin.setText(String.valueOf(newValue.intValue())));
        hSlider.highValueProperty().addListener((observable, oldValue, newValue) -> {
            if (newValue.intValue() == 110)
                lblMax.setText("∞");
            else
                lblMax.setText(String.valueOf(newValue.intValue()));
        });
        vb2.getChildren().add(hSlider);

        return Borders.wrap(vb2)
                .lineBorder()
                .innerPadding(4)
                .outerPadding(4)
                .buildAll();
    }

    private Accordion createAccordion() {
        TitledPane t1 = createCommonViewSettingsPane();

        Accordion accordion = new Accordion();
        accordion.getPanes().add(t1);
        accordion.setExpandedPane(t1);
        return accordion;
    }

    public PopOver createFilterPopover() {
        PopOver popover = new PopOver();
        popover.setTitle("Erweiterte Filtereinstellungen");
        popover.setAnimated(true);
        popover.setCloseButtonEnabled(false);
        popover.setDetachable(false);
        popover.setArrowLocation(PopOver.ArrowLocation.TOP_RIGHT);

        VBox vb = new VBox();
        vb.setSpacing(4.0);
        vb.setPadding(new Insets(5, 5, 5, 5));
        vb.getChildren().addAll(createAccordion());
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
