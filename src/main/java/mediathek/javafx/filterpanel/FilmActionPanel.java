package mediathek.javafx.filterpanel;

import impl.org.controlsfx.autocompletion.SuggestionProvider;
import javafx.animation.PauseTransition;
import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.StringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.scene.Scene;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Tooltip;
import javafx.util.Duration;
import javafx.util.StringConverter;
import mediathek.config.Daten;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.gui.actions.ManageAboAction;
import mediathek.gui.messages.FilmListWriteStartEvent;
import mediathek.gui.messages.FilmListWriteStopEvent;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.*;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.configuration2.event.ConfigurationEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.controlsfx.control.CheckListView;
import org.controlsfx.control.IndexedCheckModel;
import org.controlsfx.control.RangeSlider;
import org.controlsfx.control.textfield.TextFields;

import javax.swing.*;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * This class sets up the GuiFilme tool panel and search bar. search is exposed via a readonly
 * property for filtering in GuiFilme.
 */
public class FilmActionPanel {
  private static final Logger logger = LogManager.getLogger();
  private final PauseTransition finalActionTrans = new PauseTransition(Duration.millis(500));
  private final Tooltip tooltipSearchIrgendwo = new Tooltip("Suche in Beschreibung aktiviert");
  private final Tooltip tooltipSearchRegular = new Tooltip("Suche in Beschreibung deaktiviert");
  private final Tooltip bookmarklistSelected = new Tooltip("Alle Filme anzeigen");
  private final Tooltip bookmarklistDeselected = new Tooltip("Gemerkte Filme anzeigen");
  private FilterConfiguration filterConfig;
  private final ObservableList<FilterDTO> availableFilters;
  public ReadOnlyStringWrapper roSearchStringProperty = new ReadOnlyStringWrapper();
  public BooleanProperty showOnlyHd;
  public BooleanProperty showSubtitlesOnly;
  public BooleanProperty showNewOnly;
  public BooleanProperty showBookMarkedOnly;
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
  /** Stores the list of thema strings used for autocompletion. */
  private SuggestionProvider<String> themaSuggestionProvider;

  private FXFilmToolBar toolBar;
  private CommonViewSettingsPane viewSettingsPane;
  private boolean themaLoading = false;
  private boolean senderLoading = false;
  private CachedFilterConfiguration chachedFilterConfig;

  public FilmActionPanel() {
    this.filterConfig = new FilterConfiguration();

    setupViewSettingsPane();
    ApplicationConfiguration.getInstance().addEventListener(ConfigurationEvent.SET_PROPERTY,event -> {
      setFilterConfiguration();
    });
    setFilterConfiguration();
    setupSaveFilterSettingsButton();
    setupRestoreFilterSettingsButton();
    setupClearCurrentFilterButton();

    SwingUtilities.invokeLater(
        () -> filterDialog = new SwingFilterDialog(MediathekGui.ui(), viewSettingsPane));

    restoreConfigSettings();

    setupConfigListeners();
    availableFilters = FXCollections.observableArrayList(filterConfig.getAvailableFilters());
    setupFilterSelection();
    setupDeleteCurrentFilterButton();
    setupAddNewFilterButton();

      MessageBus.getMessageBus().subscribe(this);
  }

  private void setupRestoreFilterSettingsButton() {
    viewSettingsPane.registerRestoreButtonListener(actionEvent -> {
        viewSettingsPane.setRestoreButtonDisabled(true);
        if(chachedFilterConfig != null) {
          chachedFilterConfig.restore();
          restoreConfigSettings();
        }
    });
  }

  private void setupSaveFilterSettingsButton() {
    viewSettingsPane.registerSaveButtonListener(actionEvent ->  {
      viewSettingsPane.setSaveButtonDisabled(true);
      if(chachedFilterConfig!=null)
      {
        chachedFilterConfig.save();
      }
    });
  }

  private void setFilterConfiguration() {
    if (isLivePersistence()) {
      this.filterConfig = new FilterConfiguration();
      viewSettingsPane.setRestoreButtonDisabled(true);
      viewSettingsPane.setSaveButtonDisabled(true);
      viewSettingsPane.setRestoreButtonVisibility(false);
      viewSettingsPane.setSaveButtonVisibility(false);
    } else {
      this.chachedFilterConfig = new CachedFilterConfiguration();
      this.filterConfig = chachedFilterConfig;
      viewSettingsPane.setRestoreButtonDisabled(true);
      viewSettingsPane.setSaveButtonDisabled(true);
      viewSettingsPane.setRestoreButtonVisibility(true);
      viewSettingsPane.setSaveButtonVisibility(true);
      setupCachedFilterConfigListener();
    }
  }

  private void setupCachedFilterConfigListener() {
    this.chachedFilterConfig.registerEventListener(configurationEvent -> {
      viewSettingsPane.setSaveButtonDisabled(false);
      viewSettingsPane.setRestoreButtonDisabled(false);
    });
  }

  private boolean isLivePersistence() {
    return ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.CONFIG_LIVE_PERSIST_FILTERS,true);
  }

  private void setupAddNewFilterButton() {
    viewSettingsPane.setAddNewFilterButtonEventHandler(
        event -> {
          FilterDTO newFilter =
              new FilterDTO(
                  UUID.randomUUID(), String.format("Filter %d", availableFilters.size() + 1));
          filterConfig.addNewFilter(newFilter);
          viewSettingsPane.disableDeleteCurrentFilterButton(false);
          viewSettingsPane.selectFilter(newFilter);
        });
  }

  private void setupDeleteCurrentFilterButton() {
    if (availableFilters.size() <= 1) {
      viewSettingsPane.disableDeleteCurrentFilterButton(true);
    }

    viewSettingsPane.btnDeleteCurrentFilter.setOnAction(
        event -> {
          FilterDTO filterToDelete = filterConfig.getCurrentFilter();
          filterConfig.deleteFilter(filterToDelete);

          if (availableFilters.size() <= 1) {
            viewSettingsPane.disableDeleteCurrentFilterButton(true);
          }
        });
  }

  private void setupFilterSelection() {
    viewSettingsPane.setAvailableFilters(availableFilters);
    FilterConfiguration.addAvailableFiltersObserver(
        () -> {
          availableFilters.clear();
          availableFilters.addAll(filterConfig.getAvailableFilters());
        });
    FilterConfiguration.addCurrentFiltersObserver(
        filter -> {
          viewSettingsPane.selectFilter(filter);
          restoreConfigSettings();
        });

    viewSettingsPane.setFilterSelectionChangeListener(
        (observableValue, oldValue, newValue) -> {
          if (newValue != null && !newValue.equals(oldValue)) {
            filterConfig.setCurrentFilter(newValue);
          }
        });

    viewSettingsPane.setFilterSelectionStringConverter(
        new StringConverter<>() {

          @Override
          public String toString(FilterDTO filter) {
            if (filter == null) {
              return null;
            }
            return filter.name();
          }

          @Override
          public FilterDTO fromString(String name) {
            return filterConfig.findFilterForName(name).orElseGet(() -> renameCurrentFilter(name));
          }
        });
  }

  private FilterDTO renameCurrentFilter(String newValue) {
    FilterDTO currentFilter = filterConfig.getCurrentFilter();
    if (logger.isDebugEnabled()) {
      logger.debug(
          "Can't find a filter with name \"{}\". Renaming the current filter \"{}\" to it.",
          newValue,
          currentFilter.name());
    }
    filterConfig.renameCurrentFilter(newValue);
    return filterConfig.getCurrentFilter();
  }

  private void setupClearCurrentFilterButton() {
    viewSettingsPane.btnDeleteFilterSettings.setOnAction(
        e -> {
          filterConfig.clearCurrentFilter();
          restoreConfigSettings();
        });
  }

  private void setupViewSettingsPane() {
    viewSettingsPane = new CommonViewSettingsPane();

    showOnlyHd = viewSettingsPane.cbShowOnlyHd.selectedProperty();
    showSubtitlesOnly = viewSettingsPane.cbShowSubtitlesOnly.selectedProperty();
    showNewOnly = viewSettingsPane.cbShowNewOnly.selectedProperty();
    showBookMarkedOnly = viewSettingsPane.cbShowBookMarkedOnly.selectedProperty();
    showLivestreamsOnly = viewSettingsPane.cbShowOnlyLivestreams.selectedProperty();

    showUnseenOnly = viewSettingsPane.cbShowUnseenOnly.selectedProperty();
    dontShowAbos = viewSettingsPane.cbDontShowAbos.selectedProperty();
    dontShowSignLanguage = viewSettingsPane.cbDontShowGebaerdensprache.selectedProperty();
    dontShowTrailers = viewSettingsPane.cbDontShowTrailers.selectedProperty();
    dontShowAudioVersions = viewSettingsPane.cbDontShowAudioVersions.selectedProperty();

    senderList = viewSettingsPane.senderBoxNode.senderBox;
    senderList
        .getItems()
        .addListener(
            (ListChangeListener<? super String>) listChangeListener -> loadSavedSenderChecks());
    senderList
        .getCheckModel()
        .getCheckedItems()
        .addListener((ListChangeListener<? super String>) listChangeListener -> updateThemaBox());

    themaBox = viewSettingsPane.themaComboBox;
    themaSuggestionProvider = SuggestionProvider.create(themaBox.getItems());
    TextFields.bindAutoCompletion(themaBox.getEditor(), themaSuggestionProvider);

    filmLengthSlider = viewSettingsPane.filmLengthSliderNode._filmLengthSlider;

    zeitraumProperty = viewSettingsPane.zeitraumSpinner.valueProperty();
  }

  private void loadSavedSenderChecks() {
    senderLoading = true;
    IndexedCheckModel<String> senderCheckModel = senderList.getCheckModel();
    List<String> loadedSender = filterConfig.getSender();
    senderCheckModel.clearChecks();
    loadedSender.forEach(senderCheckModel::check);
    updateThemaBox();
    senderLoading = false;
  }

  private void loadSavedThema() {
    themaBox.getSelectionModel().select(filterConfig.getThema());
  }

  private void restoreConfigSettings() {
    viewSettingsPane.selectFilter(filterConfig.getCurrentFilter());
    showOnlyHd.set(filterConfig.isShowHdOnly());
    showSubtitlesOnly.set(filterConfig.isShowSubtitlesOnly());
    showNewOnly.set(filterConfig.isShowNewOnly());
    showBookMarkedOnly.set(filterConfig.isShowBookMarkedOnly());
    showUnseenOnly.set(filterConfig.isShowUnseenOnly());
    showLivestreamsOnly.set(filterConfig.isShowLivestreamsOnly());

    dontShowAbos.set(filterConfig.isDontShowAbos());
    dontShowTrailers.set(filterConfig.isDontShowTrailers());
    dontShowSignLanguage.set(filterConfig.isDontShowSignLanguage());
    dontShowAudioVersions.set(filterConfig.isDontShowAudioVersions());

    loadSavedSenderChecks();
    updateThemaBox();

    try {
      double loadedMin = filterConfig.getFilmLengthMin();
      if (loadedMin > filmLengthSlider.getHighValue()) {
        filmLengthSlider.setHighValueChanging(true);
        filmLengthSlider.setHighValue(filterConfig.getFilmLengthMax());
        filmLengthSlider.setHighValueChanging(false);

        filmLengthSlider.setLowValueChanging(true);
        filmLengthSlider.setLowValue(loadedMin);
        filmLengthSlider.setLowValueChanging(false);
      } else {
        filmLengthSlider.setLowValueChanging(true);
        filmLengthSlider.setLowValue(loadedMin);
        filmLengthSlider.setLowValueChanging(false);

        filmLengthSlider.setHighValueChanging(true);
        filmLengthSlider.setHighValue(filterConfig.getFilmLengthMax());
        filmLengthSlider.setHighValueChanging(false);
      }

    } catch (Exception exception) {
      logger.debug(
          "Beim wiederherstellen der Filter Einstellungen für die Filmlänge ist ein Fehler aufgetreten!",
          exception);
    }

    try {
      viewSettingsPane.zeitraumSpinner.getValueFactory().setValue(filterConfig.getZeitraum());
    } catch (Exception exception) {
      logger.debug(
          "Beim wiederherstellen der Filter Einstellungen für den Zeitraum ist ein Fehler aufgetreten!",
          exception);
    }
  }

  private void setupConfigListeners() {
    showOnlyHd.addListener(
        (observable, oldValue, newValue) -> filterConfig.setShowHdOnly(newValue));
    showSubtitlesOnly.addListener(
        ((observable, oldValue, newValue) -> filterConfig.setShowSubtitlesOnly(newValue)));
    showBookMarkedOnly.addListener(
        ((observable, oldValue, newValue) -> filterConfig.setShowBookMarkedOnly(newValue)));
    showNewOnly.addListener(
        ((observable, oldValue, newValue) -> filterConfig.setShowNewOnly(newValue)));
    showUnseenOnly.addListener(
        ((observable, oldValue, newValue) -> filterConfig.setShowUnseenOnly(newValue)));
    showLivestreamsOnly.addListener(
        ((observable, oldValue, newValue) -> filterConfig.setShowLivestreamsOnly(newValue)));

    dontShowAbos.addListener(
        ((observable, oldValue, newValue) -> filterConfig.setDontShowAbos(newValue)));
    dontShowTrailers.addListener(
        ((observable, oldValue, newValue) -> filterConfig.setDontShowTrailers(newValue)));
    dontShowSignLanguage.addListener(
        ((observable, oldValue, newValue) -> filterConfig.setDontShowSignLanguage(newValue)));
    dontShowAudioVersions.addListener(
        ((observable, oldValue, newValue) -> filterConfig.setDontShowAudioVersions(newValue)));

    senderList
        .getCheckModel()
        .getCheckedItems()
        .addListener(
            (ListChangeListener<String>)
                change -> {
                  if (!senderLoading && !themaLoading) {
                    filterConfig.setSender(new ArrayList<>(change.getList()));
                  }
                });
    themaBox
        .getSelectionModel()
        .selectedItemProperty()
        .addListener(
            ((observable, oldValue, newValue) -> {
              if (!themaLoading && !senderLoading) {
                filterConfig.setThema(newValue);
              }
            }));

    filmLengthSlider
        .lowValueProperty()
        .addListener(
            ((observable, oldValue, newValue) ->
                filterConfig.setFilmLengthMin(newValue.doubleValue())));
    filmLengthSlider
        .highValueProperty()
        .addListener(
            ((observable, oldValue, newValue) ->
                filterConfig.setFilmLengthMax(newValue.doubleValue())));

    viewSettingsPane
        .zeitraumSpinner
        .valueProperty()
        .addListener(((observable, oldValue, newValue) -> filterConfig.setZeitraum(newValue)));
  }

  @Handler
  private void handleFilmlistWriteStartEvent(FilmListWriteStartEvent e) {
    Platform.runLater(() -> toolBar.btnDownloadFilmList.setDisable(true));
  }

  @Handler
  private void handleFilmlistWriteStopEvent(FilmListWriteStopEvent e) {
    Platform.runLater(() -> toolBar.btnDownloadFilmList.setDisable(false));
  }

    private void searchFieldFinalAction() {
        final var text = toolBar.jfxSearchField.getText();
        if (Filter.isPattern(text)) {
            //only invoke filtering if we have regexp pattern and the pattern is valid
            if (Filter.makePatternNoCache(text) != null) {
                invokeTableViewFiltering();
            }
        } else {
            invokeTableViewFiltering();
        }
    }

    private void setupSearchField() {
        toolBar.jfxSearchField.setMode(FXSearchControlFieldMode.THEMA_TITEL);

        final StringProperty textProperty = toolBar.jfxSearchField.textProperty();
        roSearchStringProperty.bind(textProperty);

        finalActionTrans.setOnFinished(e -> searchFieldFinalAction());
        textProperty.addListener((observable, oldValue, newValue) -> finalActionTrans.playFromStart());
    }

    /**
     * Reload table view to apply filter text.
     */
    private void invokeTableViewFiltering() {
        SwingUtilities.invokeLater(() -> MediathekGui.ui().tabFilme.filterFilmAction.actionPerformed(null));
    }

    private void setupSearchThroughDescriptionButton() {
    final boolean enabled =
        ApplicationConfiguration.getConfiguration()
            .getBoolean(ApplicationConfiguration.SEARCH_USE_FILM_DESCRIPTIONS, false);
    toolBar.btnSearchThroughDescription.setSelected(enabled);

    if (enabled) setupForIrgendwoSearch();
    else setupForRegularSearch();

    toolBar.btnSearchThroughDescription.setOnAction(
        e -> {
          final boolean bSearchThroughDescription =
              toolBar.btnSearchThroughDescription.isSelected();
          ApplicationConfiguration.getConfiguration()
              .setProperty(
                  ApplicationConfiguration.SEARCH_USE_FILM_DESCRIPTIONS, bSearchThroughDescription);

          if (bSearchThroughDescription) setupForIrgendwoSearch();
          else setupForRegularSearch();
        });

    searchThroughDescription = toolBar.btnSearchThroughDescription.selectedProperty();
  }

  private void setupForRegularSearch() {
    toolBar.jfxSearchField.setMode(FXSearchControlFieldMode.THEMA_TITEL);

    toolBar.btnSearchThroughDescription.setTooltip(tooltipSearchRegular);
  }

  private void setupForIrgendwoSearch() {
    toolBar.jfxSearchField.setMode(FXSearchControlFieldMode.IRGENDWO);

    toolBar.btnSearchThroughDescription.setTooltip(tooltipSearchIrgendwo);
  }

  private void setupShowBookmarkedMoviesButton() {
    toolBar.btnShowBookmarkedMovies.setSelected(false);
    toolBar.btnShowBookmarkedMovies.setTooltip(bookmarklistDeselected);
    toolBar.btnShowBookmarkedMovies.setOnAction(
        event ->
            viewSettingsPane
                .cbShowBookMarkedOnly
                .selectedProperty()
                .set(toolBar.btnShowBookmarkedMovies.isSelected()));
    showBookMarkedOnly.addListener(
        (observable, oldValue, benabled) -> {
          toolBar.btnShowBookmarkedMovies.setTooltip(
              benabled ? bookmarklistSelected : bookmarklistDeselected);
          toolBar.btnShowBookmarkedMovies.setSelected(benabled);
          if (benabled) {
            toolBar.jfxSearchField.clear();
          }
        });
  }

    private void updateThemaBox() {
        if (themaLoading) {
      return;
    }
    themaLoading = true;
    List<String> themen = new ArrayList<>();
    themen.add("");
        List<String> selectedSenders = senderList.getCheckModel().getCheckedItems();

    final var blackList = Daten.getInstance().getListeFilmeNachBlackList();
    if (selectedSenders.isEmpty()) {
      themen.addAll(blackList.getThemen(""));
    } else {
      for (String sender : selectedSenders) {
        themen.addAll(blackList.getThemen(sender));
      }
    }
    themaBox.setItems(
            FXCollections.observableList(
            themen.stream()
                .distinct()
                .sorted(GermanStringSorter.getInstance())
                .collect(Collectors.toList())));
    themaSuggestionProvider.clearSuggestions();
    themaSuggestionProvider.addPossibleSuggestions(themen);
    loadSavedThema();
    themaLoading = false;

    if (!themaBox.getItems().contains(themaBox.getSelectionModel().getSelectedItem())) {
      themaBox.getSelectionModel().selectFirst();
    }
  }

  private void setupToolBar() {
    toolBar = new FXFilmToolBar();
    toolBar.btnDownloadFilmList.setOnAction(e -> SwingUtilities.invokeLater(
                () -> MediathekGui.ui().performFilmListLoadOperation(false)));
    toolBar.btnFilmInfo.setOnAction(
        e -> SwingUtilities.invokeLater(MediathekGui.ui().getFilmInfoDialog()::showInfo));
    toolBar.btnPlay.setOnAction(evt ->
            SwingUtilities.invokeLater(
                () -> MediathekGui.ui().tabFilme.playAction.actionPerformed(null)));
    toolBar.btnRecord.setOnAction(e ->
            SwingUtilities.invokeLater(
                () -> MediathekGui.ui().tabFilme.saveFilmAction.actionPerformed(null)));
    toolBar.btnBookmark.setOnAction(e ->
            SwingUtilities.invokeLater(
                () -> MediathekGui.ui().tabFilme.bookmarkFilmAction.actionPerformed(null)));
    toolBar.btnManageBookMarks.setOnAction(e ->
            SwingUtilities.invokeLater(
                () -> MediathekGui.ui().tabFilme.bookmarkManageListAction.actionPerformed(null)));
    toolBar.btnManageAbos.setOnAction(e ->
            SwingUtilities.invokeLater(() -> {
                  if (manageAboAction.isEnabled()) manageAboAction.actionPerformed(null);
                }));
    toolBar.btnShowFilter.setOnAction(e ->
            SwingUtilities.invokeLater(() -> {
                  if (filterDialog != null) {
                    filterDialog.setVisible(
                        !filterDialog.isVisible()); // Toggle Dialog display on button press
                  }
                }));
  }

    public Scene getFilmActionPanelScene() {
        setupToolBar();

        setupSearchField();

        setupSearchThroughDescriptionButton();

        setupShowBookmarkedMoviesButton();

        Daten.getInstance().getFilmeLaden().addAdListener(
                new ListenerFilmeLaden() {
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

  public void filmlisteLadenFertigCallback() {
    loadSavedSenderChecks();
    updateThemaBox();
  }
}

