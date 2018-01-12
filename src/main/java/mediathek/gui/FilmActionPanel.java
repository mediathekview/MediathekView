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
    public final PopOver filterPopover;
    private final Daten daten;
    public ReadOnlyStringWrapper roSearchStringProperty = new ReadOnlyStringWrapper();
    public BooleanProperty showOnlyHd;
    public BooleanProperty showSubtitlesOnly;
    public BooleanProperty showNewOnly;
    public BooleanProperty showUnseenOnly;
    public BooleanProperty dontShowAbos;
    private CustomTextField jfxSearchField;
    private Button btnDownload;
    private Button btnFilmInformation;
    private Button btnPlay;
    private Button btnRecord;
    private Button btnAdvancedFilter;
    private Button btnNewFilter;
    private BlacklistButton btnBlacklist;

    public FilmActionPanel(Daten daten) {
        this.daten = daten;

        filterPopover = createFilterPopover();
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
            pause3.setOnFinished(evt -> SwingUtilities.invokeLater(() -> Daten.guiFilme.filterFilmAction.actionPerformed(null)));
            pause3.playFromStart();
        });

        roSearchStringProperty.bind(textProperty);
    }

    private Parent createRight() {
        GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");

        setupSearchField();

        btnAdvancedFilter = new Button("", fontAwesome.create(FontAwesome.Glyph.QUESTION_CIRCLE));
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

        btnNewFilter = new Button("", fontAwesome.create(FontAwesome.Glyph.FILTER));
        btnNewFilter.setOnAction(e -> filterPopover.show(btnNewFilter));
        ObservableList<Node> list = hb.getChildren();

        list.add(btnNewFilter);
        list.add(new VerticalSeparator());
        list.add(jfxSearchField);
        list.add(btnAdvancedFilter);

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
            btnAdvancedFilter.setDisable(disabled);
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

        CheckBox cbShowUnseenOnly = new CheckBox("Gesehene Filme ausblenden");
        showUnseenOnly = cbShowUnseenOnly.selectedProperty();
        vBox.getChildren().add(cbShowUnseenOnly);

        CheckBox cbDontShowAbos = new CheckBox("Abos ausblenden");
        dontShowAbos = cbDontShowAbos.selectedProperty();
        vBox.getChildren().add(cbDontShowAbos);

        CheckBox cbDontShowImpairedFilms = new CheckBox("Hörfassungen etc ausblenden");
        cbDontShowImpairedFilms.setDisable(true);
        vBox.getChildren().add(cbDontShowImpairedFilms);

        CheckBox cbShowOnlyLivestreams = new CheckBox("Nur Live Streams anzeigen");
        showOnlyLivestreams = cbShowOnlyLivestreams.selectedProperty();
        vBox.getChildren().add(cbShowOnlyLivestreams);

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

    public ComboBox<String> senderBox;
    public ComboBox<String> themaBox;

    public BooleanProperty showOnlyLivestreams;

    public RangeSlider filmLengthSlider;

    public final static int UNLIMITED_VALUE = 110;

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

    public ReadOnlyObjectProperty<String> zeitraumProperty;
    public Spinner<String> zeitraumSpinner;

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
}
