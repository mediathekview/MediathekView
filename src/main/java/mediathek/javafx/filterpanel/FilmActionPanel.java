package mediathek.javafx.filterpanel;

import impl.org.controlsfx.autocompletion.SuggestionProvider;
import javafx.animation.PauseTransition;
import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.StringProperty;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.util.Duration;
import mediathek.config.Daten;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.gui.actions.ManageAboAction;
import mediathek.gui.messages.FilmListWriteStartEvent;
import mediathek.gui.messages.FilmListWriteStopEvent;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.Filter;
import mediathek.tool.GermanStringSorter;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.configuration2.Configuration;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.controlsfx.control.CheckListView;
import org.controlsfx.control.RangeSlider;
import org.controlsfx.control.textfield.TextFields;

import javax.swing.*;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;
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
    /**
     * Stores the list of thema strings used for autocompletion.
     */
    private SuggestionProvider<String> themaSuggestionProvider;
    private FXFilmToolBar toolBar = new FXFilmToolBar();
    private final CommonViewSettingsPane viewSettingsPane;

    public FilmActionPanel(Daten daten) {
        this.daten = daten;

        viewSettingsPane = new CommonViewSettingsPane();
        SwingUtilities.invokeLater(() -> filterDialog = new SwingFilterDialog(MediathekGui.ui(), viewSettingsPane));

        restoreConfigSettings();

        setupConfigListeners();

        daten.getMessageBus().subscribe(this);
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
            viewSettingsPane.zeitraumSpinner.getValueFactory().setValue(config.getString(ApplicationConfiguration.FILTER_PANEL_ZEITRAUM, ZeitraumSpinner.UNLIMITED_VALUE));
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

        viewSettingsPane.zeitraumSpinner.valueProperty().addListener(((observable, oldValue, newValue) -> config.setProperty(ApplicationConfiguration.FILTER_PANEL_ZEITRAUM, newValue)));
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

    class CommonViewSettingsPane extends VBox implements Initializable {
        private final Logger logger = LogManager.getLogger(CommonViewSettingsPane.class);
        @FXML private Button btnDeleteFilterSettings;
        @FXML private CheckBox cbShowOnlyHd;
        @FXML private CheckBox cbShowSubtitlesOnly;
        @FXML private CheckBox cbShowNewOnly;
        @FXML private CheckBox cbShowOnlyLivestreams;
        @FXML private CheckBox cbShowUnseenOnly;
        @FXML private CheckBox cbDontShowAbos;
        @FXML private CheckBox cbDontShowGebaerdensprache;
        @FXML private CheckBox cbDontShowTrailers;
        @FXML private CheckBox cbDontShowAudioVersions;
        @FXML private SenderBoxNode senderBoxNode;
        @FXML private ThemaComboBox _themaComboBox;
        @FXML private FilmLenghtSliderNode filmLengthSliderNode;
        @FXML private ZeitraumSpinner zeitraumSpinner;

        public CommonViewSettingsPane() {
            super();

            try {
                URL url = getClass().getResource("/mediathek/res/programm/fxml/filter_settings_pane.fxml");
                FXMLLoader fxmlLoader = new FXMLLoader(url);
                fxmlLoader.setRoot(this);
                fxmlLoader.setController(this);
                fxmlLoader.load();
            } catch (IOException e) {
                logger.error("Failed to load FXML!", e);
            }}

        @Override
        public void initialize(URL url, ResourceBundle resourceBundle) {
            btnDeleteFilterSettings.setOnAction(e -> {
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

                viewSettingsPane.zeitraumSpinner.getValueFactory().setValue(ZeitraumSpinner.UNLIMITED_VALUE);
            });

            showOnlyHd = cbShowOnlyHd.selectedProperty();
            showSubtitlesOnly = cbShowSubtitlesOnly.selectedProperty();
            showNewOnly = cbShowNewOnly.selectedProperty();
            showLivestreamsOnly = cbShowOnlyLivestreams.selectedProperty();

            showUnseenOnly = cbShowUnseenOnly.selectedProperty();
            dontShowAbos = cbDontShowAbos.selectedProperty();
            dontShowSignLanguage = cbDontShowGebaerdensprache.selectedProperty();
            dontShowTrailers = cbDontShowTrailers.selectedProperty();
            dontShowAudioVersions = cbDontShowAudioVersions.selectedProperty();

            senderList = senderBoxNode.senderBox;
            senderBoxNode.pauseTransition.setOnFinished(e -> updateThemaBox());

            themaBox = _themaComboBox;
            themaSuggestionProvider = SuggestionProvider.create(themaBox.getItems());
            TextFields.bindAutoCompletion(themaBox.getEditor(), themaSuggestionProvider);

            filmLengthSlider = filmLengthSliderNode._filmLengthSlider;

            zeitraumProperty = zeitraumSpinner.valueProperty();

        }
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

}
