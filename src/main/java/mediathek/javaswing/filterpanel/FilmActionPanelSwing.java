package mediathek.javaswing.filterpanel;

import ca.odell.glazedlists.BasicEventList;
import ca.odell.glazedlists.EventList;
import ca.odell.glazedlists.TransactionList;
import impl.org.controlsfx.autocompletion.SuggestionProvider;
import javafx.collections.ListChangeListener;
import javafx.util.StringConverter;
import mediathek.config.Daten;
import mediathek.javafx.filterpanel.CommonViewSettingsPane;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.FilterConfiguration;
import mediathek.tool.FilterDTO;
import mediathek.tool.GermanStringSorter;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.controlsfx.control.textfield.TextFields;
import org.jetbrains.annotations.NotNull;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.JToggleButton;
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionListener;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import ca.odell.glazedlists.*;
import ca.odell.glazedlists.swing.AutoCompleteSupport;

public class FilmActionPanelSwing {
    private static final Logger logger = LogManager.getLogger();
    private final FilterConfiguration filterConfig;
    private CommonViewSettingsPaneSwing viewSettingsPane;
    private OldSwingFilterDialog filterDialog;
    private final EventList<String> sourceThemaList = new BasicEventList<>();
    private final DefaultComboBoxModel<String> observableThemaList = new DefaultComboBoxModel<>();
    private final DefaultComboBoxModel<FilterDTO> availableFilters = new DefaultComboBoxModel<>();
    private RangeSliderSwing filmLengthSlider;
    private SuggestionProvider<String> themaSuggestionProvider;


    public FilmActionPanelSwing(@NotNull JToggleButton filterToggleBtn) {
        this.filterConfig = new FilterConfiguration();

        setupViewSettingsPane();
        setupDeleteFilterButton();

        SwingUtilities.invokeLater(
                () -> filterDialog = new OldSwingFilterDialog(MediathekGui.ui(), viewSettingsPane, filterToggleBtn));

        restoreConfigSettings();

        setupConfigListeners();
        availableFilters.addAll(filterConfig.getAvailableFilters());
        setupFilterSelection();
        setupDeleteCurrentFilterButton();
        setupAddNewFilterButton();
    }

    public OldSwingFilterDialog getFilterDialog() {
        return filterDialog;
    }

    private RangeSliderSwing getFilmLengthSlider() {
        return filmLengthSlider;
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
        if (availableFilters.getSize() <= 1) {
            viewSettingsPane.disableDeleteCurrentFilterButton(true);
        }

        viewSettingsPane.btnDeleteCurrentFilter.addActionListener(
                event -> {
                    FilterDTO filterToDelete = filterConfig.getCurrentFilter();
                    filterConfig.deleteFilter(filterToDelete);

                    if (availableFilters.getSize() <= 1) {
                        viewSettingsPane.disableDeleteCurrentFilterButton(true);
                    }
                });
    }

    private void setupFilterSelection() {
        viewSettingsPane.setAvailableFilters(availableFilters);
        FilterConfiguration.addAvailableFiltersObserver(
                () -> {
                    availableFilters.removeAllElements();
                    availableFilters.addAll(filterConfig.getAvailableFilters());
                });
        FilterConfiguration.addCurrentFiltersObserver(
                filter -> {
                    viewSettingsPane.selectFilter(filter);
                    restoreConfigSettings();
                });

        viewSettingsPane.setFilterSelectionChangeListener(e -> {
            FilterDTO newValue = (FilterDTO) viewSettingsPane.filterSelect.getSelectedItem();

            if (newValue != null && !newValue.equals(filterConfig.getCurrentFilter())) {
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
        viewSettingsPane.btnDeleteFilterSettings.addActionListener(e -> {
            viewSettingsPane.senderCheckList.clearCheckBoxListSelection();

            filterConfig.clearCurrentFilter();
            restoreConfigSettings();
        });
    }

    private void setupViewSettingsPane() {
        viewSettingsPane = new CommonViewSettingsPaneSwing();

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

        setupThemaComboBox();
        viewSettingsPane.senderCheckList.getSelectionModel().addListSelectionListener(ListSelectionListener);
                addListener((ListChangeListener<String>) c -> updateThemaComboBox());

        filmLengthSlider = viewSettingsPane.filmLengthSliderNode.filmLengthSlider;

        zeitraumProperty = viewSettingsPane.zeitraumSpinner.valueProperty();
    }

    private void setupThemaComboBox() {
        viewSettingsPane.themaComboBox.setModel(observableThemaList);
        //themaSuggestionProvider = SuggestionProvider.create(sourceThemaList);
        TextFields.bindAutoCompletion(viewSettingsPane.themaComboBox.getEditor(), themaSuggestionProvider);
    }

    public CommonViewSettingsPaneSwing getViewSettingsPane() {
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

        var selectedSenders = viewSettingsPane.senderCheckList.getSelectedValuesList();
        var tempThemaList = getThemaList(selectedSenders).stream().distinct()
                .sorted(GermanStringSorter.getInstance())
                .toList();
        transactionThemaList.addAll(tempThemaList);
        transactionThemaList.commitEvent();

        //update autocompletion provider here only as the other listeners fire too much
        themaSuggestionProvider.clearSuggestions();
        themaSuggestionProvider.addPossibleSuggestions(sourceThemaList);

        viewSettingsPane.themaComboBox.setSelectedIndex(0);
    }
}

