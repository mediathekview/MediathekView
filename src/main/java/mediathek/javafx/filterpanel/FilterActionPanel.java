package mediathek.javafx.filterpanel;

import ca.odell.glazedlists.BasicEventList;
import ca.odell.glazedlists.EventList;
import ca.odell.glazedlists.TransactionList;
import ca.odell.glazedlists.javafx.EventObservableList;
import impl.org.controlsfx.autocompletion.SuggestionProvider;
import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.util.StringConverter;
import mediathek.config.Daten;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.EventListWithEmptyFirstEntry;
import mediathek.tool.FilterConfiguration;
import mediathek.tool.FilterDTO;
import mediathek.tool.GermanStringSorter;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.controlsfx.control.RangeSlider;
import org.controlsfx.control.textfield.TextFields;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * This class sets up the GuiFilme filter dialog.
 * property for filtering in GuiFilme.
 */
public class FilterActionPanel {
    private static final Logger logger = LogManager.getLogger();
    private final FilterConfiguration filterConfig;
    private final ObservableList<FilterDTO> availableFilters;
    /**
     * The "base" thema list
     */
    private final EventList<String> sourceThemaList = new BasicEventList<>();
    /**
     * The JavaFX list based on {@link #sourceThemaList}.
     */
    private final EventObservableList<String> observableThemaList = new EventObservableList<>(new EventListWithEmptyFirstEntry(sourceThemaList));
    private OldSwingJavaFxFilterDialog filterDialog;
    private RangeSlider filmLengthSlider;
    private ReadOnlyObjectProperty<String> zeitraumProperty;
    private BooleanProperty dontShowAudioVersions;
    private BooleanProperty dontShowSignLanguage;
    private BooleanProperty dontShowTrailers;
    private BooleanProperty dontShowAbos;
    private BooleanProperty dontShowDuplicates;
    private BooleanProperty showLivestreamsOnly;
    private BooleanProperty showUnseenOnly;
    private BooleanProperty showBookMarkedOnly;
    private BooleanProperty showSubtitlesOnly;
    private BooleanProperty showOnlyHighQuality;
    private BooleanProperty showNewOnly;
    /**
     * Stores the list of thema strings used for autocompletion.
     */
    private SuggestionProvider<String> themaSuggestionProvider;
    private CommonViewSettingsPane viewSettingsPane;

    public FilterActionPanel(@NotNull JToggleButton filterToggleBtn) {
        this.filterConfig = new FilterConfiguration();

        setupViewSettingsPane();
        setupDeleteFilterButton();
        setupRenameFilterButton();

        SwingUtilities.invokeLater(
                () -> filterDialog = new OldSwingJavaFxFilterDialog(MediathekGui.ui(), viewSettingsPane, filterToggleBtn));

        restoreConfigSettings();

        setupConfigListeners();
        availableFilters = FXCollections.observableArrayList(filterConfig.getAvailableFilters());
        setupFilterSelection();
        setupDeleteCurrentFilterButton();
        setupAddNewFilterButton();
    }

    public OldSwingJavaFxFilterDialog getFilterDialog() {
        return filterDialog;
    }

    public RangeSlider getFilmLengthSlider() {
        return filmLengthSlider;
    }

    public ReadOnlyObjectProperty<String> zeitraumProperty() {
        return zeitraumProperty;
    }

    public boolean isDontShowAudioVersions() {
        return dontShowAudioVersions.get();
    }

    public BooleanProperty dontShowAudioVersionsProperty() {
        return dontShowAudioVersions;
    }

    public boolean isDontShowSignLanguage() {
        return dontShowSignLanguage.get();
    }

    public BooleanProperty dontShowSignLanguageProperty() {
        return dontShowSignLanguage;
    }

    public boolean isDontShowTrailers() {
        return dontShowTrailers.get();
    }

    public BooleanProperty dontShowTrailersProperty() {
        return dontShowTrailers;
    }

    public boolean isDontShowAbos() {
        return dontShowAbos.get();
    }

    public BooleanProperty dontShowAbosProperty() {
        return dontShowAbos;
    }

    public BooleanProperty dontShowDuplicatesProperty() { return dontShowDuplicates;}
    public boolean isDontShowDuplicates() { return dontShowDuplicates.get();}

    public boolean isShowLivestreamsOnly() {
        return showLivestreamsOnly.get();
    }

    public BooleanProperty showLivestreamsOnlyProperty() {
        return showLivestreamsOnly;
    }

    public boolean isShowUnseenOnly() {
        return showUnseenOnly.get();
    }

    public BooleanProperty showUnseenOnlyProperty() {
        return showUnseenOnly;
    }

    public boolean isShowBookMarkedOnly() {
        return showBookMarkedOnly.get();
    }

    public BooleanProperty showBookMarkedOnlyProperty() {
        return showBookMarkedOnly;
    }

    public boolean isShowSubtitlesOnly() {
        return showSubtitlesOnly.get();
    }

    public BooleanProperty showSubtitlesOnlyProperty() {
        return showSubtitlesOnly;
    }

    public boolean isShowOnlyHighQuality() {
        return showOnlyHighQuality.get();
    }

    public BooleanProperty showOnlyHighQualityProperty() {
        return showOnlyHighQuality;
    }

    public boolean isShowNewOnly() {
        return showNewOnly.get();
    }

    public BooleanProperty showNewOnlyProperty() {
        return showNewOnly;
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
                () -> Platform.runLater(() -> {
                    availableFilters.clear();
                    availableFilters.addAll(filterConfig.getAvailableFilters());
                }));
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
        logger.debug("Can't find a filter with name \"{}\". Renaming the current filter \"{}\" to it.",
                newValue, currentFilter.name());
        filterConfig.renameCurrentFilter(newValue);
        return filterConfig.getCurrentFilter();
    }

    private void setupDeleteFilterButton() {
        viewSettingsPane.btnDeleteFilterSettings.setOnAction(e -> {
            viewSettingsPane.senderCheckList.getCheckModel().clearChecks();

            filterConfig.clearCurrentFilter();
            restoreConfigSettings();
        });
    }

    private void setupRenameFilterButton() {
        viewSettingsPane.btnRenameFilter.setOnAction(e -> {
            final var fltName = filterConfig.getCurrentFilter().name();
            SwingUtilities.invokeLater(() -> {
                String s = (String)JOptionPane.showInputDialog(
                        MediathekGui.ui(),
                        "Neuer Name des Filters:",
                        "Filter umbenennen",
                        JOptionPane.PLAIN_MESSAGE,
                        null,
                        null,
                        fltName);
                if (s != null) {
                    if (!s.isEmpty()) {
                        final var fName = s.trim();
                        if (!fName.equals(fltName)){
                            renameCurrentFilter(fName);
                            logger.trace("Renamed filter \"{}\" to \"{}\"", fltName, fName);
                        }
                        else
                            logger.warn("New and old filter name are identical...doing nothing");
                    }
                    else
                        logger.warn("Rename filter text was empty...doing nothing");
                }
                else
                    logger.trace("User cancelled rename");
            });
        });
    }

    private void setupViewSettingsPane() {
        viewSettingsPane = new CommonViewSettingsPane();

        showOnlyHighQuality = viewSettingsPane.cbShowOnlyHd.selectedProperty();
        showSubtitlesOnly = viewSettingsPane.cbShowSubtitlesOnly.selectedProperty();
        showNewOnly = viewSettingsPane.cbShowNewOnly.selectedProperty();
        showBookMarkedOnly = viewSettingsPane.cbShowBookMarkedOnly.selectedProperty();
        showLivestreamsOnly = viewSettingsPane.cbShowOnlyLivestreams.selectedProperty();

        showUnseenOnly = viewSettingsPane.cbShowUnseenOnly.selectedProperty();
        dontShowAbos = viewSettingsPane.cbDontShowAbos.selectedProperty();
        dontShowSignLanguage = viewSettingsPane.cbDontShowGebaerdensprache.selectedProperty();
        dontShowTrailers = viewSettingsPane.cbDontShowTrailers.selectedProperty();
        dontShowAudioVersions = viewSettingsPane.cbDontShowAudioVersions.selectedProperty();
        dontShowDuplicates = viewSettingsPane.cbDontShowDuplicates.selectedProperty();

        setupThemaComboBox();
        viewSettingsPane.senderCheckList.getCheckModel().getCheckedItems().
                addListener((ListChangeListener<String>) c -> updateThemaComboBox());

        filmLengthSlider = viewSettingsPane.filmLengthSliderNode._filmLengthSlider;

        zeitraumProperty = viewSettingsPane.zeitraumSpinner.valueProperty();
    }

    private void setupThemaComboBox() {
        viewSettingsPane.themaComboBox.setItems(observableThemaList);
        themaSuggestionProvider = SuggestionProvider.create(sourceThemaList);
        TextFields.bindAutoCompletion(viewSettingsPane.themaComboBox.getEditor(), themaSuggestionProvider);
    }

    public CommonViewSettingsPane getViewSettingsPane() {
        return viewSettingsPane;
    }

    private void restoreConfigSettings() {
        viewSettingsPane.selectFilter(filterConfig.getCurrentFilter());
        showOnlyHighQuality.set(filterConfig.isShowHdOnly());
        showSubtitlesOnly.set(filterConfig.isShowSubtitlesOnly());
        showNewOnly.set(filterConfig.isShowNewOnly());
        showBookMarkedOnly.set(filterConfig.isShowBookMarkedOnly());
        showUnseenOnly.set(filterConfig.isShowUnseenOnly());
        showLivestreamsOnly.set(filterConfig.isShowLivestreamsOnly());

        dontShowAbos.set(filterConfig.isDontShowAbos());
        dontShowTrailers.set(filterConfig.isDontShowTrailers());
        dontShowSignLanguage.set(filterConfig.isDontShowSignLanguage());
        dontShowAudioVersions.set(filterConfig.isDontShowAudioVersions());
        dontShowDuplicates.set(filterConfig.isDontShowDuplicates());

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
            logger.debug("Beim wiederherstellen der Filter Einstellungen für die Filmlänge ist ein Fehler aufgetreten!",
                    exception);
        }

        try {
            viewSettingsPane.zeitraumSpinner.getValueFactory().setValue(filterConfig.getZeitraum());
        } catch (Exception exception) {
            logger.debug("Beim wiederherstellen der Filter Einstellungen für den Zeitraum ist ein Fehler aufgetreten!",
                    exception);
        }
    }

    private void setupConfigListeners() {
        showOnlyHighQuality.addListener(
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
        dontShowDuplicates.addListener(
                ((observable, oldValue, newValue) -> filterConfig.setDontShowDuplicates(newValue)));

        filmLengthSlider.lowValueProperty().addListener(
                ((observable, oldValue, newValue) -> filterConfig.setFilmLengthMin(newValue.doubleValue())));
        filmLengthSlider.highValueProperty().addListener(
                ((observable, oldValue, newValue) -> filterConfig.setFilmLengthMax(newValue.doubleValue())));

        zeitraumProperty.addListener(((os, oV, newValue) -> filterConfig.setZeitraum(newValue)));
    }

    /**
     * Retrieve the list of all thema based on sender select checkbox list.
     *
     * @param selectedSenders the list of selected senders
     * @return list of all applicable themas.
     */
    private List<String> getThemaList(@NotNull List<String> selectedSenders) {
        List<String> finalList = new ArrayList<>();

        final var blackList = Daten.getInstance().getListeFilmeNachBlackList();
        if (selectedSenders.isEmpty()) {
            finalList.addAll(blackList.getThemen(""));
        } else {
            for (String sender : selectedSenders) {
                finalList.addAll(blackList.getThemen(sender));
            }
        }

        return finalList;
    }

    /**
     * Update the Thema list and the autocompletion provider after a sender checkbox list change.
     */
    public void updateThemaComboBox() {
        //update the thema list -> updates the combobox automagically
        //use transaction list to minimize updates...
        var transactionThemaList = new TransactionList<>(sourceThemaList);
        transactionThemaList.beginEvent(true);
        transactionThemaList.clear();

        var selectedSenders = viewSettingsPane.senderCheckList.getCheckModel().getCheckedItems();
        var tempThemaList = getThemaList(selectedSenders).stream().distinct()
                .sorted(GermanStringSorter.getInstance())
                .toList();
        transactionThemaList.addAll(tempThemaList);
        transactionThemaList.commitEvent();

        //update autocompletion provider here only as the other listeners fire too much
        themaSuggestionProvider.clearSuggestions();
        themaSuggestionProvider.addPossibleSuggestions(sourceThemaList);

        viewSettingsPane.themaComboBox.getSelectionModel().select(0);
    }
}
