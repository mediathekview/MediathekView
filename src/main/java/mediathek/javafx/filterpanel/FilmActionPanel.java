package mediathek.javafx.filterpanel;

import javafx.animation.PauseTransition;
import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.StringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.layout.*;
import javafx.util.Duration;
import javafx.util.StringConverter;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.tool.ApplicationConfiguration;
import mediathek.config.Daten;
import mediathek.gui.dialog.DialogLeer;
import mediathek.gui.dialogEinstellungen.PanelBlacklist;
import mediathek.gui.messages.FilmListWriteStartEvent;
import mediathek.gui.messages.FilmListWriteStopEvent;
import mediathek.javafx.VerticalSeparator;
import mediathek.tool.Filter;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.configuration2.Configuration;
import org.controlsfx.control.PopOver;
import org.controlsfx.control.RangeSlider;
import org.controlsfx.control.textfield.CustomTextField;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.GlyphFont;
import org.controlsfx.glyphfont.GlyphFontRegistry;
import org.controlsfx.tools.Borders;

import javax.swing.*;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * This class sets up the GuiFilme tool panel and search bar.
 * search is exposed via a readonly property for filtering in GuiFilme.
 */
public class FilmActionPanel {
    public final static int UNLIMITED_VALUE = 110;
    private final PopOver filterPopover;
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
    private Spinner<String> zeitraumSpinner;
    private CustomTextField jfxSearchField;
    private Button btnDownload;
    private Button btnFilmInformation;
    private Button btnPlay;
    private Button btnRecord;
    private Button btnNewFilter;
    private BlacklistButton btnBlacklist;
    private Button btnEditBlacklist;

    public FilmActionPanel(Daten daten) {
        this.daten = daten;

        filterPopover = createFilterPopover();

        restoreConfigSettings();

        setupConfigListeners();

        Daten.getInstance().getMessageBus().subscribe(this);
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

    private final GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");

    private Parent createLeft() {
        HBox hb = new HBox();
        hb.setPadding(new Insets(5, 5, 5, 5));
        hb.setSpacing(4.0);
        hb.setAlignment(Pos.CENTER_LEFT);

        ObservableList<Node> list = hb.getChildren();
        list.add(createDownloadButton());
        list.add(new VerticalSeparator());

        list.add(createFilmInformationButton());
        list.add(new VerticalSeparator());

        list.add(createPlayButton());
        list.add(createRecordButton());
        list.add(new VerticalSeparator());

        btnBlacklist = new BlacklistButton(daten);
        list.add(btnBlacklist);
        list.add(createEditBlacklistButton());

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

    @Handler
    private void handleFilmlistWriteStartEvent(FilmListWriteStartEvent e) {
        Platform.runLater(() -> btnDownload.setDisable(true));
    }

    @Handler
    private void handleFilmlistWriteStopEvent(FilmListWriteStopEvent e) {
        Platform.runLater(() -> btnDownload.setDisable(false));
    }

    private Button createDownloadButton() {
        btnDownload = new Button("", fontAwesome.create(FontAwesome.Glyph.CLOUD_DOWNLOAD));
        btnDownload.setTooltip(new Tooltip("Neue Filmliste laden"));
        btnDownload.setOnAction(e -> SwingUtilities.invokeLater(() -> daten.getFilmeLaden().loadFilmlistDialog(daten, false)));

        return btnDownload;
    }

    private Button createPlayButton() {
        btnPlay = new Button("", fontAwesome.create(FontAwesome.Glyph.PLAY));
        btnPlay.setTooltip(new Tooltip("Film abspielen"));
        btnPlay.setOnAction(evt -> SwingUtilities.invokeLater(() -> Daten.guiFilme.playAction.actionPerformed(null)));

        return btnPlay;
    }

    private Button createFilmInformationButton() {
        btnFilmInformation = new Button("", fontAwesome.create(FontAwesome.Glyph.INFO_CIRCLE));
        btnFilmInformation.setTooltip(new Tooltip("Filminformation anzeigen"));
        btnFilmInformation.setOnAction(e -> SwingUtilities.invokeLater(Daten.filmInfo::showInfo));

        return btnFilmInformation;
    }

    private Button createRecordButton() {
        btnRecord = new Button("", fontAwesome.create(FontAwesome.Glyph.DOWNLOAD));
        btnRecord.setOnAction(e -> SwingUtilities.invokeLater(() -> Daten.guiFilme.saveFilmAction.actionPerformed(null)));
        btnRecord.setTooltip(new Tooltip("Film aufzeichnen"));

        return btnRecord;
    }

    private Button createEditBlacklistButton() {
        btnEditBlacklist = new Button("", fontAwesome.create(FontAwesome.Glyph.SKYATLAS));
        btnEditBlacklist.setTooltip(new Tooltip("Blacklist bearbeiten"));
        btnEditBlacklist.setOnAction(e -> SwingUtilities.invokeLater(() -> {
            DialogLeer dialog = new DialogLeer(null, true);
            dialog.init("Blacklist", new PanelBlacklist(daten, null, PanelBlacklist.class.getName() + "_3"));
            dialog.setVisible(true);
        }));

        return btnEditBlacklist;
    }

    private void setupLeftButtons(boolean disabled) {
        Platform.runLater(() -> {
            btnDownload.setDisable(disabled);
            btnFilmInformation.setDisable(disabled);
            btnPlay.setDisable(disabled);
            btnRecord.setDisable(disabled);
            btnEditBlacklist.setDisable(disabled);
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
        jfxSearchField.setPromptText("Titel/Thema");

        final StringProperty textProperty = jfxSearchField.textProperty();

        pause2.setOnFinished(evt -> checkPatternValidity());
        textProperty.addListener((observable, oldValue, newValue) -> pause2.playFromStart());

        pause3.setOnFinished(evt -> SwingUtilities.invokeLater(() -> Daten.guiFilme.filterFilmAction.actionPerformed(null)));
        textProperty.addListener((observable, oldValue, newValue) -> pause3.playFromStart());

        roSearchStringProperty.bind(textProperty);
    }

    private Parent createRight() {
        setupSearchField();

        HBox hb = new HBox();
        hb.setPadding(new Insets(5, 5, 5, 5));
        hb.setSpacing(4);
        hb.setAlignment(Pos.CENTER_RIGHT);

        btnNewFilter = new Button("", fontAwesome.create(FontAwesome.Glyph.FILTER));
        btnNewFilter.setTooltip(new Tooltip("Filtereinstellungen anzeigen"));
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

        /*
        whenever the senderbox selected value changes, reload the thema from filtered list.
         */
        senderBox.valueProperty().addListener(
                (observable, oldValue, newValue) -> {
                    if (newValue != null) {
                        themaBox.getItems().clear();
                        themaBox.getItems().add("");
                        final List<String> lst = daten.getListeFilmeNachBlackList().getThemen(newValue);
                        themaBox.getItems().addAll(lst);
                    }
                }
        );

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

    private PopOver createFilterPopover() {
        PopOver popover = new PopOver();
        popover.setTitle("Erweiterte Filtereinstellungen");
        popover.setAnimated(true);
        popover.setCloseButtonEnabled(true);
        popover.setAutoFix(true);
        popover.setDetachable(true);
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
}
