package mediathek.javafx.filterpanel;

import impl.org.controlsfx.autocompletion.SuggestionProvider;
import javafx.animation.PauseTransition;
import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.StringProperty;
import javafx.collections.ListChangeListener;
import javafx.geometry.Insets;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.layout.*;
import javafx.util.Duration;
import mediathek.config.Daten;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.gui.actions.ManageAboAction;
import mediathek.gui.messages.FilmListWriteStartEvent;
import mediathek.gui.messages.FilmListWriteStopEvent;
import mediathek.javafx.CenteredBorderPane;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.Filter;
import mediathek.tool.GermanStringSorter;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.configuration2.Configuration;
import org.controlsfx.control.CheckListView;
import org.controlsfx.control.RangeSlider;
import org.controlsfx.control.textfield.TextFields;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.GlyphFont;
import org.controlsfx.glyphfont.GlyphFontRegistry;
import org.controlsfx.tools.Borders;

import javax.swing.*;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * This class sets up the GuiFilme tool panel and search bar.
 * search is exposed via a readonly property for filtering in GuiFilme.
 */
public class FilmActionPanel {
    private static final String PROMPT_THEMA_TITEL = "Thema/Titel";
    private static final String PROMPT_IRGENDWO = "Thema/Titel/Beschreibung";
    private final Daten daten;
    private final Configuration config = ApplicationConfiguration.getConfiguration();
    private final PauseTransition pause2 = new PauseTransition(Duration.millis(150));
    private final PauseTransition pause3 = new PauseTransition(Duration.millis(500));
    private final GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");
    private final Tooltip themaTitelTooltip = new Tooltip("Thema/Titel durchsuchen");
    private final Tooltip irgendwoTooltip = new Tooltip("Thema/Titel/Beschreibung durchsuchen");
    private final Tooltip TOOLTIP_SEARCH_IRGENDWO = new Tooltip("Suche in Beschreibung aktiviert");
    private final Tooltip TOOLTIP_SEARCH_REGULAR = new Tooltip("Suche in Beschreibung deaktiviert");
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
    public BooleanProperty searchThroughDescription;
    public ReadOnlyObjectProperty<String> zeitraumProperty;
    public ComboBox<String> themaBox;
    public RangeSlider filmLengthSlider;
    public CheckListView<String> senderList;
    public JDialog filterDialog;
    public ManageAboAction manageAboAction;
    private Spinner<String> zeitraumSpinner;
    /**
     * Stores the list of thema strings used for autocompletion.
     */
    private SuggestionProvider<String> themaSuggestionProvider;
    private FXFilmToolBar toolBar = new FXFilmToolBar();

    public FilmActionPanel(Daten daten) {
        this.daten = daten;

        createFilterDialog();

        restoreConfigSettings();

        setupConfigListeners();

        daten.getMessageBus().subscribe(this);
    }

    private void createFilterDialog() {
        VBox vb = getFilterDialogContent();
        SwingUtilities.invokeLater(() -> filterDialog = new SwingFilterDialog(MediathekGui.ui(), vb));
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
            zeitraumSpinner.getValueFactory().setValue(config.getString(ApplicationConfiguration.FILTER_PANEL_ZEITRAUM, ZeitraumSpinner.UNLIMITED_VALUE));
        } catch (Exception ignored) {
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

    @Handler
    private void handleFilmlistWriteStartEvent(FilmListWriteStartEvent e) {
        Platform.runLater(() -> toolBar.btnDownloadFilmList.setDisable(true));
    }

    @Handler
    private void handleFilmlistWriteStopEvent(FilmListWriteStopEvent e) {
        Platform.runLater(() -> toolBar.btnDownloadFilmList.setDisable(false));
    }

    private void checkPatternValidity() {
        toolBar.jfxSearchField.setStyle("-fx-text-fill: red");

        // Schriftfarbe ändern wenn eine RegEx
        final String text = toolBar.jfxSearchField.getText();
        if (Filter.isPattern(text)) {
            if (Filter.makePattern(text) == null) {
                //soll Pattern sein, ist aber falsch
                toolBar.jfxSearchField.setStyle("-fx-text-fill: red");
            } else {
                toolBar.jfxSearchField.setStyle("-fx-text-fill: blue");
            }
        } else {
            toolBar.jfxSearchField.setStyle("-fx-text-fill: black");
        }
    }

    private void setupSearchField() {
        toolBar.jfxSearchField.setTooltip(themaTitelTooltip);
        toolBar.jfxSearchField.setPromptText(PROMPT_THEMA_TITEL);

        final StringProperty textProperty = toolBar.jfxSearchField.textProperty();

        pause2.setOnFinished(evt -> checkPatternValidity());
        textProperty.addListener((observable, oldValue, newValue) -> pause2.playFromStart());

        pause3.setOnFinished(evt -> SwingUtilities.invokeLater(() -> MediathekGui.ui().tabFilme.filterFilmAction.actionPerformed(null)));
        textProperty.addListener((observable, oldValue, newValue) -> pause3.playFromStart());

        roSearchStringProperty.bind(textProperty);
    }

    private void setupSearchThroughDescriptionButton() {
        final boolean enabled = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.SEARCH_USE_FILM_DESCRIPTIONS, false);
        toolBar.btnSearchThroughDescription.setSelected(enabled);

        if (enabled)
            setupForIrgendwoSearch();
        else
            setupForRegularSearch();

        toolBar.btnSearchThroughDescription.setOnAction(e -> {
            final boolean bSearchThroughDescription = toolBar.btnSearchThroughDescription.isSelected();
            ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.SEARCH_USE_FILM_DESCRIPTIONS,
                    bSearchThroughDescription);

            if (bSearchThroughDescription)
                setupForIrgendwoSearch();
            else
                setupForRegularSearch();
        });

        searchThroughDescription = toolBar.btnSearchThroughDescription.selectedProperty();
    }

    private void setupForRegularSearch() {
        toolBar.jfxSearchField.setTooltip(themaTitelTooltip);
        toolBar.jfxSearchField.setPromptText(PROMPT_THEMA_TITEL);

        toolBar.btnSearchThroughDescription.setTooltip(TOOLTIP_SEARCH_REGULAR);
    }

    private void setupForIrgendwoSearch() {
        toolBar.jfxSearchField.setTooltip(irgendwoTooltip);
        toolBar.jfxSearchField.setPromptText(PROMPT_IRGENDWO);

        toolBar.btnSearchThroughDescription.setTooltip(TOOLTIP_SEARCH_IRGENDWO);
    }

    private VBox createCommonViewSettingsPane() {
        Button btnDeleteFilterSettings = new DeleteFilterSettingsButton();

        CheckBox cbShowOnlyHd = new CheckBox("Nur HD-Filme anzeigen");
        showOnlyHd = cbShowOnlyHd.selectedProperty();

        CheckBox cbShowSubtitlesOnly = new CheckBox("Nur Filme mit Untertitel anzeigen");
        showSubtitlesOnly = cbShowSubtitlesOnly.selectedProperty();

        CheckBox cbShowNewOnly = new CheckBox("Nur neue Filme anzeigen");
        showNewOnly = cbShowNewOnly.selectedProperty();

        CheckBox cbShowOnlyLivestreams = new CheckBox("Nur Live Streams anzeigen");
        showLivestreamsOnly = cbShowOnlyLivestreams.selectedProperty();

        CheckBox cbShowUnseenOnly = new CheckBox("Gesehene Filme nicht anzeigen");
        showUnseenOnly = cbShowUnseenOnly.selectedProperty();

        CheckBox cbDontShowAbos = new CheckBox("Abos nicht anzeigen");
        dontShowAbos = cbDontShowAbos.selectedProperty();

        CheckBox cbDontShowGebaerdensprache = new CheckBox("Gebärdensprache nicht anzeigen");
        dontShowSignLanguage = cbDontShowGebaerdensprache.selectedProperty();

        CheckBox cbDontShowTrailers = new CheckBox("Trailer/Teaser/Vorschau nicht anzeigen");
        dontShowTrailers = cbDontShowTrailers.selectedProperty();

        CheckBox cbDontShowAudioVersions = new CheckBox("Hörfassungen ausblenden");
        dontShowAudioVersions = cbDontShowAudioVersions.selectedProperty();

        VBox vBox = new VBox();
        vBox.setSpacing(4d);

        Node senderBox = new SenderBoxNode();
        VBox.setVgrow(senderBox, Priority.ALWAYS);

        vBox.getChildren().addAll(
                btnDeleteFilterSettings,
                new Separator(),
                cbShowOnlyHd,
                cbShowSubtitlesOnly,
                cbShowNewOnly,
                cbShowOnlyLivestreams,
                new Separator(),
                cbShowUnseenOnly,
                cbDontShowAbos,
                cbDontShowGebaerdensprache,
                cbDontShowTrailers,
                cbDontShowAudioVersions,
                new Separator(),
                senderBox,
                new Separator(),
                new ThemaBoxNode(),
                new Separator(),
                new FilmLenghtSliderNode(),
                new Separator(),
                new ZeitraumPane());

        setupSenderListeners();

        return vBox;
    }

    private void setupSenderListeners() {
        PauseTransition trans = new PauseTransition(Duration.millis(500d));
        trans.setOnFinished(e -> updateThemaBox());
        senderList.getCheckModel()
                .getCheckedItems().
                addListener((ListChangeListener<String>) c -> trans.playFromStart());
    }

    public void updateThemaBox() {
        final var items = themaBox.getItems();
        items.clear();
        items.add("");

        List<String> finalList = new ArrayList<>();
        List<String> selectedSenders = senderList.getCheckModel().getCheckedItems();

        if (selectedSenders.isEmpty()) {
            final List<String> lst = daten.getListeFilmeNachBlackList().getThemen("");
            finalList.addAll(lst);
            lst.clear();
        } else {
            for (String sender : selectedSenders) {
                final List<String> lst = daten.getListeFilmeNachBlackList().getThemen(sender);
                finalList.addAll(lst);
                lst.clear();
            }
        }

        items.addAll(finalList.stream()
                .distinct()
                .sorted(GermanStringSorter.getInstance())
                .collect(Collectors.toList()));
        finalList.clear();

        themaSuggestionProvider.clearSuggestions();
        themaSuggestionProvider.addPossibleSuggestions(items);
        themaBox.getSelectionModel().select(0);
    }

    private VBox getFilterDialogContent() {
        VBox vb = new VBox();
        vb.setSpacing(4.0);
        vb.setPadding(new Insets(5, 5, 5, 5));
        vb.getChildren().add(createCommonViewSettingsPane());

        return vb;
    }

    private void setupToolBar() {
        toolBar = new FXFilmToolBar();
        toolBar.btnDownloadFilmList.setOnAction(e -> SwingUtilities.invokeLater(() -> MediathekGui.ui().performFilmListLoadOperation(false)));
        toolBar.btnFilmInfo.setOnAction(e -> SwingUtilities.invokeLater(MediathekGui.ui().getFilmInfoDialog()::showInfo));
        toolBar.btnPlay.setOnAction(evt -> SwingUtilities.invokeLater(() -> MediathekGui.ui().tabFilme.playAction.actionPerformed(null)));
        toolBar.btnRecord.setOnAction(e -> SwingUtilities.invokeLater(() -> MediathekGui.ui().tabFilme.saveFilmAction.actionPerformed(null)));
        toolBar.btnManageAbos.setOnAction(e -> SwingUtilities.invokeLater(() -> {
            if (manageAboAction.isEnabled())
                manageAboAction.actionPerformed(null);
        }));
        toolBar.btnShowFilter.setOnAction(e -> SwingUtilities.invokeLater(() -> {
            if (filterDialog != null) {
                if (!filterDialog.isVisible()) {
                    filterDialog.setVisible(true);
                }
            }
        }));
    }

    public Scene getFilmActionPanelScene() {
        Region spacer = new Region();
        HBox.setHgrow(spacer, Priority.ALWAYS);

        setupToolBar();

        setupSearchField();

        setupSearchThroughDescriptionButton();

        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                Platform.runLater(() -> toolBar.setDisable(true));
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                Platform.runLater(() -> toolBar.setDisable(false));
            }
        });

        return new Scene(toolBar);
    }

    class SenderBoxNode extends VBox {
        public SenderBoxNode() {
            senderList = new SenderListBox();
            VBox.setVgrow(senderList, Priority.ALWAYS);
            getChildren().addAll(
                    new Label("Sender:"),
                    senderList);
        }
    }

    class ThemaBoxNode extends HBox {
        public ThemaBoxNode() {
            setSpacing(4d);

            themaBox = new ComboBox<>();
            themaBox.getItems().addAll("");
            themaBox.getSelectionModel().select(0);
            themaBox.setPrefWidth(350d);

            themaBox.setEditable(true);
            themaSuggestionProvider = SuggestionProvider.create(themaBox.getItems());
            TextFields.bindAutoCompletion(themaBox.getEditor(), themaSuggestionProvider);

            getChildren().addAll(new CenteredBorderPane(new Label("Thema:")), themaBox);
        }
    }

    class FilmLenghtSliderNode extends VBox {
        public FilmLenghtSliderNode() {
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

            filmLengthSlider = new FilmLengthSlider();

            lblMin.setText(String.valueOf((int) filmLengthSlider.getLowValue()));
            lblMax.setText(filmLengthSlider.getLabelFormatter().toString(filmLengthSlider.getHighValue()));
            filmLengthSlider.lowValueProperty().addListener((observable, oldValue, newValue) -> lblMin.setText(String.valueOf(newValue.intValue())));
            filmLengthSlider.highValueProperty().addListener((observable, oldValue, newValue) -> lblMax.setText(filmLengthSlider.getLabelFormatter().toString(newValue)));
            vb2.getChildren().add(filmLengthSlider);

            var result = Borders.wrap(vb2)
                    .lineBorder()
                    .innerPadding(4)
                    .outerPadding(4)
                    .buildAll();

            getChildren().add(result);
        }
    }

    class ZeitraumPane extends FlowPane {
        public ZeitraumPane() {
            Label zeitraum = new Label("Zeitraum:");

            zeitraumSpinner = new ZeitraumSpinner();
            zeitraumProperty = zeitraumSpinner.valueProperty();

            Label days = new Label("Tage");

            setHgap(4);
            getChildren().addAll(zeitraum, zeitraumSpinner, days);
        }
    }

    private class SenderListBox extends CheckListView<String> {
        public SenderListBox() {
            super(daten.getListeFilmeNachBlackList().getSenders());
            setPrefHeight(150d);
            setMinHeight(100d);
        }
    }

    private class DeleteFilterSettingsButton extends Button {
        public DeleteFilterSettingsButton() {
            super("", fontAwesome.create(FontAwesome.Glyph.TRASH_ALT));
            setTooltip(new Tooltip("Filter zurücksetzen"));
            setOnAction(e -> {
                showOnlyHd.setValue(false);
                showSubtitlesOnly.setValue(false);
                showNewOnly.setValue(false);
                showLivestreamsOnly.setValue(false);
                showUnseenOnly.setValue(false);
                dontShowAbos.setValue(false);
                dontShowSignLanguage.setValue(false);
                dontShowTrailers.setValue(false);
                dontShowAudioVersions.setValue(false);

                senderList.getCheckModel().clearChecks();
                themaBox.getSelectionModel().select("");

                filmLengthSlider.lowValueProperty().setValue(0);
                filmLengthSlider.highValueProperty().setValue(FilmLengthSlider.UNLIMITED_VALUE);

                zeitraumSpinner.getValueFactory().setValue(ZeitraumSpinner.UNLIMITED_VALUE);
            });
        }
    }
}
