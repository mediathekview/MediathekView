package mediathek.gui;

import javafx.animation.PauseTransition;
import javafx.application.Platform;
import javafx.beans.property.*;
import javafx.collections.FXCollections;
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
import javafx.scene.layout.*;
import javafx.util.Duration;
import javafx.util.StringConverter;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.tool.ApplicationConfiguration;
import mSearch.tool.Listener;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.javafx.JFXSearchPanel;
import mediathek.tool.Filter;
import org.apache.commons.configuration2.Configuration;
import org.controlsfx.control.PopOver;
import org.controlsfx.control.RangeSlider;
import org.controlsfx.control.textfield.CustomTextField;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.GlyphFont;
import org.controlsfx.glyphfont.GlyphFontRegistry;
import org.controlsfx.tools.Borders;

import javax.swing.*;
import java.util.NoSuchElementException;

/**
 * This class sets up the GuiFilme tool panel and search bar.
 * search is exposed via a readonly property for filtering in GuiFilme.
 */
public class FilmActionPanel {
    public final static int UNLIMITED_VALUE = 110;
    public final PopOver filterPopover;
    private final Daten daten;
    private final Configuration config = ApplicationConfiguration.getConfiguration();
    private final PauseTransition pause2 = new PauseTransition(Duration.millis(150));
    private final PauseTransition pause3 = new PauseTransition(Duration.millis(500));
    public ReadOnlyStringWrapper roSearchStringProperty = new ReadOnlyStringWrapper();
    public BooleanProperty showOnlyHd;
    public BooleanProperty showSubtitlesOnly;
    public BooleanProperty showNewOnly;
    public BooleanProperty showUnseenOnly;
    public BooleanProperty showLivestreamsOnly;
    public BooleanProperty dontShowAbos;
    public BooleanProperty dontShowTrailers;
    public BooleanProperty dontShowSignLanguage;
    public BooleanProperty dontShowAudioVersions;
    public ReadOnlyObjectProperty<String> zeitraumProperty;
    public ComboBox<String> senderBox;
    public ComboBox<String> themaBox;
    public RangeSlider filmLengthSlider;
    public Spinner<String> zeitraumSpinner;
    private CustomTextField jfxSearchField;
    private Button btnDownload;
    private Button btnFilmInformation;
    private Button btnPlay;
    private Button btnRecord;
    private Button btnNewFilter;
    private BlacklistButton btnBlacklist;

    public FilmActionPanel(Daten daten) {
        this.daten = daten;

        filterPopover = createFilterPopover();

        restoreConfigSettings();

        setupConfigListeners();
    }

    private void restoreConfigSettings() {
        showOnlyHd.set(config.getBoolean(ApplicationConfiguration.FILTER_PANEL_SHOW_HD_ONLY, false));
        showSubtitlesOnly.set(config.getBoolean(ApplicationConfiguration.FILTER_PANEL_SHOW_SUBTITLES_ONLY, false));
        showNewOnly.set(config.getBoolean(ApplicationConfiguration.FILTER_PANEL_SHOW_NEW_ONLY, false));
        showUnseenOnly.set(config.getBoolean(ApplicationConfiguration.FILTER_PANEL_SHOW_UNSEEN_ONLY, false));
        showLivestreamsOnly.set(config.getBoolean(ApplicationConfiguration.FILTER_PANEL_SHOW_LIVESTREAMS_ONLY, false));

        dontShowAbos.set(config.getBoolean(ApplicationConfiguration.FILTER_PANEL_DONT_SHOW_ABOS, false));
        dontShowTrailers.set(config.getBoolean(ApplicationConfiguration.FILTER_PANEL_DONT_SHOW_TRAILERS, false));
        dontShowSignLanguage.set(config.getBoolean(ApplicationConfiguration.FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE, false));
        dontShowAudioVersions.set(config.getBoolean(ApplicationConfiguration.FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS, false));

        try {
            filmLengthSlider.lowValueProperty().set(config.getDouble(ApplicationConfiguration.FILTER_PANEL_FILM_LENGTH_MIN));
            filmLengthSlider.highValueProperty().set(config.getDouble(ApplicationConfiguration.FILTER_PANEL_FILM_LENGTH_MAX));
        } catch (Exception ignored) {
        }

        try {
            zeitraumSpinner.getValueFactory().setValue(config.getString(ApplicationConfiguration.FILTER_PANEL_ZEITRAUM));
        } catch (NoSuchElementException ignored) {
        }
    }

    private void setupConfigListeners() {
        showOnlyHd.addListener((observable, oldValue, newValue) -> config.setProperty(ApplicationConfiguration.FILTER_PANEL_SHOW_HD_ONLY, newValue));
        showSubtitlesOnly.addListener(((observable, oldValue, newValue) -> config.setProperty(ApplicationConfiguration.FILTER_PANEL_SHOW_SUBTITLES_ONLY, newValue)));
        showNewOnly.addListener(((observable, oldValue, newValue) -> config.setProperty(ApplicationConfiguration.FILTER_PANEL_SHOW_NEW_ONLY, newValue)));
        showUnseenOnly.addListener(((observable, oldValue, newValue) -> config.setProperty(ApplicationConfiguration.FILTER_PANEL_SHOW_UNSEEN_ONLY, newValue)));
        showLivestreamsOnly.addListener(((observable, oldValue, newValue) -> config.setProperty(ApplicationConfiguration.FILTER_PANEL_SHOW_LIVESTREAMS_ONLY, newValue)));

        dontShowAbos.addListener(((observable, oldValue, newValue) -> config.setProperty(ApplicationConfiguration.FILTER_PANEL_DONT_SHOW_ABOS, newValue)));
        dontShowTrailers.addListener(((observable, oldValue, newValue) -> config.setProperty(ApplicationConfiguration.FILTER_PANEL_DONT_SHOW_TRAILERS, newValue)));
        dontShowSignLanguage.addListener(((observable, oldValue, newValue) -> config.setProperty(ApplicationConfiguration.FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE, newValue)));
        dontShowAudioVersions.addListener(((observable, oldValue, newValue) -> config.setProperty(ApplicationConfiguration.FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS, newValue)));

        filmLengthSlider.lowValueProperty().addListener(((observable, oldValue, newValue) -> config.setProperty(ApplicationConfiguration.FILTER_PANEL_FILM_LENGTH_MIN, newValue)));
        filmLengthSlider.highValueProperty().addListener(((observable, oldValue, newValue) -> config.setProperty(ApplicationConfiguration.FILTER_PANEL_FILM_LENGTH_MAX, newValue)));

        zeitraumSpinner.valueProperty().addListener(((observable, oldValue, newValue) -> config.setProperty(ApplicationConfiguration.FILTER_PANEL_ZEITRAUM, newValue)));
    }

    private Parent createLeft() {
        GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");

        btnDownload = new Button("", fontAwesome.create(FontAwesome.Glyph.CLOUD_DOWNLOAD));
        btnDownload.setTooltip(new Tooltip("Neue Filmliste laden"));
        btnDownload.setOnAction(e -> SwingUtilities.invokeLater(() -> daten.getFilmeLaden().loadFilmlistDialog(daten, false)));

        btnFilmInformation = new Button("", fontAwesome.create(FontAwesome.Glyph.INFO_CIRCLE));
        btnFilmInformation.setTooltip(new Tooltip("Filminformation anzeigen"));
        btnFilmInformation.setOnAction(e -> SwingUtilities.invokeLater(Daten.filmInfo::showInfo));

        HBox hb = new HBox();
        hb.setPadding(new Insets(5, 5, 5, 5));
        hb.setSpacing(4.0);
        hb.setAlignment(Pos.CENTER_LEFT);

        ObservableList<Node> list = hb.getChildren();
        list.add(btnDownload);

        list.add(new VerticalSeparator());
        list.add(btnFilmInformation);
        list.add(new VerticalSeparator());

        btnPlay = new Button("", fontAwesome.create(FontAwesome.Glyph.PLAY));
        btnPlay.setTooltip(new Tooltip("Film abspielen"));
        btnPlay.setOnAction(evt -> SwingUtilities.invokeLater(() -> Daten.guiFilme.playAction.actionPerformed(null)));
        list.add(btnPlay);

        btnRecord = new Button("", fontAwesome.create(FontAwesome.Glyph.DOWNLOAD));
        btnRecord.setOnAction(e -> SwingUtilities.invokeLater(() -> Daten.guiFilme.saveFilmAction.actionPerformed(null)));
        btnRecord.setTooltip(new Tooltip("Film aufzeichnen"));
        list.add(btnRecord);
        list.add(new VerticalSeparator());
        btnBlacklist = new BlacklistButton();
        list.add(btnBlacklist);

        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                setupLeftButtons(true);
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                setupLeftButtons(false);
            }
        });

        return hb;
    }

    private void setupLeftButtons(boolean disabled) {
        Platform.runLater(() -> {
            btnDownload.setDisable(disabled);
            btnFilmInformation.setDisable(disabled);
            btnPlay.setDisable(disabled);
            btnRecord.setDisable(disabled);
        });
    }

    private void checkPatternValidity() {
        jfxSearchField.setStyle("-fx-text-fill: red");

        // Schriftfarbe ändern wenn eine RegEx
        final String text = jfxSearchField.getText();
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

        pause2.setOnFinished(evt -> checkPatternValidity());
        textProperty.addListener((observable, oldValue, newValue) -> pause2.playFromStart());

        pause3.setOnFinished(evt -> SwingUtilities.invokeLater(() -> Daten.guiFilme.filterFilmAction.actionPerformed(null)));
        textProperty.addListener((observable, oldValue, newValue) -> pause3.playFromStart());

        roSearchStringProperty.bind(textProperty);
    }

    private Parent createRight() {
        GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");

        setupSearchField();

        HBox hb = new HBox();
        hb.setPadding(new Insets(5, 5, 5, 5));
        hb.setSpacing(4);
        hb.setAlignment(Pos.CENTER_RIGHT);

        btnNewFilter = new Button("", fontAwesome.create(FontAwesome.Glyph.FILTER));
        btnNewFilter.setOnAction(e -> filterPopover.show(btnNewFilter));
        ObservableList<Node> list = hb.getChildren();

        list.add(btnNewFilter);
        list.add(new VerticalSeparator());
        list.add(jfxSearchField);

        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                setupRightButtons(true);
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                setupRightButtons(false);
            }
        });

        return hb;
    }

    private void setupRightButtons(boolean disabled) {
        Platform.runLater(() -> {
            btnNewFilter.setDisable(disabled);
            btnBlacklist.setDisable(disabled);
            jfxSearchField.setDisable(disabled);
        });

    }

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

        CheckBox cbShowOnlyLivestreams = new CheckBox("Nur Live Streams anzeigen");
        showLivestreamsOnly = cbShowOnlyLivestreams.selectedProperty();
        vBox.getChildren().add(cbShowOnlyLivestreams);

        Separator sep = new Separator();
        vBox.getChildren().add(sep);

        CheckBox cbShowUnseenOnly = new CheckBox("Gesehene Filme nicht anzeigen");
        showUnseenOnly = cbShowUnseenOnly.selectedProperty();
        vBox.getChildren().add(cbShowUnseenOnly);

        CheckBox cbDontShowAbos = new CheckBox("Abos nicht anzeigen");
        dontShowAbos = cbDontShowAbos.selectedProperty();
        vBox.getChildren().add(cbDontShowAbos);

        CheckBox cbDontShowGebaerdensprache = new CheckBox("Gebärdensprache nicht anzeigen");
        dontShowSignLanguage = cbDontShowGebaerdensprache.selectedProperty();
        vBox.getChildren().add(cbDontShowGebaerdensprache);

        CheckBox cbDontShowTrailers = new CheckBox("Trailer/Teaser/Vorschau nicht anzeigen");
        dontShowTrailers = cbDontShowTrailers.selectedProperty();
        vBox.getChildren().add(cbDontShowTrailers);

        CheckBox cbDontShowAudioVersions = new CheckBox("Hörfassungen ausblenden");
        dontShowAudioVersions = cbDontShowAudioVersions.selectedProperty();
        vBox.getChildren().add(cbDontShowAudioVersions);

        sep = new Separator();
        vBox.getChildren().add(sep);

        FlowPane root = new FlowPane();
        root.setHgap(4);
        root.getChildren().add(new Label("Sender:"));
        senderBox = new ComboBox<>();
        senderBox.getItems().addAll("");
        senderBox.getSelectionModel().select(0);
        root.getChildren().add(senderBox);

        root.setAlignment(Pos.CENTER_LEFT);
        vBox.getChildren().add(root);

        FlowPane hb = new FlowPane();
        hb.setHgap(4);
        hb.getChildren().add(new Label("Thema:"));
        themaBox = new ComboBox<>();
        themaBox.getItems().addAll("");
        themaBox.getSelectionModel().select(0);
        themaBox.setPrefWidth(200);
        hb.getChildren().add(themaBox);
        hb.setAlignment(Pos.CENTER_LEFT);
        vBox.getChildren().add(hb);

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

        filmLengthSlider = new RangeSlider(0, UNLIMITED_VALUE, 0, UNLIMITED_VALUE);
        filmLengthSlider.setShowTickMarks(true);
        filmLengthSlider.setShowTickLabels(true);
        filmLengthSlider.setBlockIncrement(1);
        filmLengthSlider.setMajorTickUnit(10);
        filmLengthSlider.setLabelFormatter(new StringConverter<Number>() {
            @Override
            public String toString(Number object) {
                if (object.intValue() == UNLIMITED_VALUE)
                    return "∞";
                else
                    return String.valueOf(object.intValue());
            }

            @Override
            public Number fromString(String string) {
                return Double.parseDouble(string);
            }
        });

        lblMin.setText(String.valueOf((int) filmLengthSlider.getLowValue()));
        lblMax.setText(filmLengthSlider.getLabelFormatter().toString(filmLengthSlider.getHighValue()));
        filmLengthSlider.lowValueProperty().addListener((observable, oldValue, newValue) -> lblMin.setText(String.valueOf(newValue.intValue())));
        filmLengthSlider.highValueProperty().addListener((observable, oldValue, newValue) -> lblMax.setText(filmLengthSlider.getLabelFormatter().toString(newValue)));
        vb2.getChildren().add(filmLengthSlider);

        return Borders.wrap(vb2)
                .lineBorder()
                .innerPadding(4)
                .outerPadding(4)
                .buildAll();
    }

    private Node createZeitraumPane() {
        Label zeitraum = new Label("Zeitraum:");
        ObservableList<String> months = FXCollections.observableArrayList("∞");
        for (int i = 1; i <= 30; i++)
            months.add(String.valueOf(i));

        SpinnerValueFactory<String> valueFactory = new SpinnerValueFactory.ListSpinnerValueFactory<>(months);

        valueFactory.setValue("∞");

        zeitraumSpinner = new Spinner<>();
        zeitraumSpinner.setValueFactory(valueFactory);
        zeitraumSpinner.setEditable(false);
        zeitraumProperty = zeitraumSpinner.valueProperty();

        Label days = new Label("Tage");

        FlowPane root = new FlowPane();
        root.setHgap(4);
        root.getChildren().addAll(zeitraum, zeitraumSpinner, days);
        return root;
    }

    private Accordion createAccordion() {
        TitledPane t1 = createCommonViewSettingsPane();
        TitledPane t2 = new TitledPane("Filmlänge", createFilmLengthSlider());
        TitledPane t3 = new TitledPane("Zeitraum", createZeitraumPane());


        Accordion accordion = new Accordion();
        accordion.getPanes().addAll(t1, t2, t3);
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
        popover.setPrefWidth(200);

        VBox vb = new VBox();
        vb.setSpacing(4.0);
        vb.setPadding(new Insets(5, 5, 5, 5));
        vb.getChildren().add(createAccordion());
        popover.setContentNode(vb);

        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                Platform.runLater(() -> popover.getContentNode().setDisable(true));
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                Platform.runLater(() -> popover.getContentNode().setDisable(false));

            }
        });

        return popover;
    }

    public Scene getFilmActionPanelScene() {
        HBox hb = new HBox();
        Region spacer = new Region();
        HBox.setHgrow(spacer, Priority.ALWAYS);
        hb.getChildren().addAll(createLeft(), spacer, createRight());

        return new Scene(hb);
    }

    private class VerticalSeparator extends Separator {
        public VerticalSeparator() {
            super();
            setOrientation(Orientation.VERTICAL);
        }
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
            final boolean isOn = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ON));
            if (isOn)
                setupOn();
            else
                setupOff();

            //set initial state
            activeProperty.addListener((observable, oldValue, newValue) -> {
                if (newValue) {
                    setupOn();
                } else {
                    setupOff();
                }
            });

            activeProperty.setValue(isOn);
            activeProperty.addListener((observable, oldValue, newValue) -> SwingUtilities.invokeLater(() -> {
                MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_ON, Boolean.toString(newValue));
                daten.getListeBlacklist().filterListe();
                Listener.notify(Listener.EREIGNIS_BLACKLIST_GEAENDERT, FilmActionPanel.class.getSimpleName());
            }));

            setOnAction(value -> activeProperty.setValue(!activeProperty.getValue()));

            Listener.addListener(new Listener(Listener.EREIGNIS_BLACKLIST_GEAENDERT, FilmActionPanel.class.getSimpleName()) {
                @Override
                public void ping() {
                    //config was changed outside
                    final boolean on = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ON));
                    Platform.runLater(() -> {
                        activeProperty.setValue(on);
                    });
                }
            });

        }

        private void setupOn() {
            setGraphic(onImageView);
            setTooltip(tooltipOn);
        }

        private void setupOff() {
            setGraphic(offImageView);
            setTooltip(tooltipOff);
        }
    }
}
