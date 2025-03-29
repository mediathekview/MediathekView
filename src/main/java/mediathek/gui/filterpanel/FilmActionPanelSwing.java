package mediathek.gui.filterpanel;

import ca.odell.glazedlists.BasicEventList;
import ca.odell.glazedlists.EventList;
import ca.odell.glazedlists.TransactionList;
import com.jidesoft.swing.RangeSlider;
import mediathek.config.Daten;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.FilterConfiguration;
import mediathek.tool.FilterDTO;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.jetbrains.annotations.NotNull;
import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.function.Function;

public class FilmActionPanelSwing {
    private static final Logger logger = LogManager.getLogger();
    private final FilterConfiguration filterConfig;
    private CommonViewSettingsPaneSwing viewSettingsPane;
    private OldSwingFilterDialog filterDialog;
    private final EventList<String> sourceThemaList = new BasicEventList<>();
    private final DefaultComboBoxModel<String> observableThemaList = new DefaultComboBoxModel<>();
    private final DefaultComboBoxModel<FilterDTO> availableFilters = new DefaultComboBoxModel<>();
    private RangeSlider filmLengthSlider;
    private JCheckBox showOnlyHighQuality = new JCheckBox();
    private JCheckBox showSubtitlesOnly = new JCheckBox();
    private JCheckBox showNewOnly  = new JCheckBox();
    private JCheckBox showBookMarkedOnly = new JCheckBox();
    private JCheckBox showLivestreamsOnly = new JCheckBox();
    private JCheckBox showUnseenOnly = new JCheckBox();
    private JCheckBox dontShowAbos = new JCheckBox();
    private JCheckBox dontShowSignLanguage = new JCheckBox();
    private JCheckBox dontShowTrailers = new JCheckBox();
    private JCheckBox dontShowAudioVersions = new JCheckBox();
    private ZeitraumSpinnerSwing zeitraumProperty = new ZeitraumSpinnerSwing();

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

    private void setupAddNewFilterButton() {
        viewSettingsPane.setAddNewFilterButtonEventHandler(
                event -> {
                    FilterDTO newFilter =
                            new FilterDTO(
                                    UUID.randomUUID(), String.format("Filter %d", availableFilters.getSize() + 1));
                    filterConfig.addNewFilter(newFilter);
                    viewSettingsPane.disableDeleteCurrentFilterButton(false);
                    viewSettingsPane.senderCheckList.clearCheckBoxListSelection();
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
        viewSettingsPane.setFilterSelectionStringConverter(new Function<FilterDTO, String>() {
            @Override
            public String apply(FilterDTO filter) {
                if (filter == null) {
                    return null;
                }
                return filter.name();
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

        showOnlyHighQuality = viewSettingsPane.cbShowOnlyHd;
        showSubtitlesOnly = viewSettingsPane.cbShowSubtitlesOnly;
        showNewOnly = viewSettingsPane.cbShowNewOnly;
        showBookMarkedOnly = viewSettingsPane.cbShowBookMarkedOnly;
        showLivestreamsOnly = viewSettingsPane.cbShowOnlyLivestreams;

        showUnseenOnly = viewSettingsPane.cbShowUnseenOnly;
        dontShowAbos = viewSettingsPane.cbDontShowAbos;
        dontShowSignLanguage = viewSettingsPane.cbDontShowGebaerdensprache;
        dontShowTrailers = viewSettingsPane.cbDontShowTrailers;
        dontShowAudioVersions = viewSettingsPane.cbDontShowAudioVersions;

        setupThemaComboBox();
        viewSettingsPane.senderCheckList.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
            @Override
            public void valueChanged(ListSelectionEvent e) {
                if (!e.getValueIsAdjusting()) {
                    updateThemaComboBox();
                }
            }
        });

       filmLengthSlider = viewSettingsPane.filmLengthSliderNode.filmLengthSlider;

        zeitraumProperty = viewSettingsPane.zeitraumSpinner;
    }


    private void setupThemaComboBox() {
        // Mach die ComboBox editierbar für Autovervollständigung
        viewSettingsPane.themaComboBox.setEditable(true);

        // Hol den JTextField der ComboBox, um die Autovervollständigung zu implementieren
        JTextField textField = (JTextField) viewSettingsPane.themaComboBox.getEditor().getEditorComponent();

        // Füge einen KeyListener hinzu, um die Autovervollständigung zu realisieren
        textField.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent e) {
                // Sobald eine Taste gedrückt wird, filtere die Liste nach den Eingaben
                String inputText = textField.getText().toLowerCase();
                filterThemaList(inputText);
            }
        });
    }

    private void filterThemaList(String inputText) {
        List<String> filteredList = new ArrayList<>();

        for (int i = 0; i < sourceThemaList.size(); i++) {
            String thema = sourceThemaList.get(i);
            if (thema.toLowerCase().contains(inputText)) {
                filteredList.add(thema);
            }
        }

        // Aktualisiere das ComboBox-Modell mit den gefilterten Themen
        observableThemaList.removeAllElements();
        for (String thema : filteredList) {
            observableThemaList.addElement(thema);
        }
        viewSettingsPane.themaComboBox.setSelectedIndex(0); // Setzt den ersten Eintrag als ausgewählt
    }


    private void restoreConfigSettings() {
        viewSettingsPane.selectFilter(filterConfig.getCurrentFilter());
        showOnlyHighQuality.setSelected(filterConfig.isShowHdOnly());
        showSubtitlesOnly.setSelected(filterConfig.isShowSubtitlesOnly());
        showNewOnly.setSelected(filterConfig.isShowNewOnly());
        showBookMarkedOnly.setSelected(filterConfig.isShowBookMarkedOnly());
        showUnseenOnly.setSelected(filterConfig.isShowUnseenOnly());
        showLivestreamsOnly.setSelected(filterConfig.isShowLivestreamsOnly());

        dontShowAbos.setSelected(filterConfig.isDontShowAbos());
        dontShowTrailers.setSelected(filterConfig.isDontShowTrailers());
        dontShowSignLanguage.setSelected(filterConfig.isDontShowSignLanguage());
        dontShowAudioVersions.setSelected(filterConfig.isDontShowAudioVersions());

        try {
            double loadedMin = filterConfig.getFilmLengthMin();
            if (loadedMin > filmLengthSlider.getHighValue()) {
                //filmLengthSlider.setValueChanging(true);
                filmLengthSlider.setHighValue((int) filterConfig.getFilmLengthMax());
                //filmLengthSlider.setUpperValueChanging(false);

                //filmLengthSlider.setValueChanging(true);
                filmLengthSlider.setLowValue((int) loadedMin);
                //filmLengthSlider.setValueChanging(false);
            } else {
                //filmLengthSlider.setValueChanging(true);
                filmLengthSlider.setLowValue((int) loadedMin);
                //filmLengthSlider.setValueChanging(false);

                //filmLengthSlider.setUpperValueChanging(true);
                filmLengthSlider.setHighValue((int) filterConfig.getFilmLengthMax());
                //filmLengthSlider.setUpperValueChanging(false);
            }

        } catch (Exception exception) {
            logger.debug("Beim wiederherstellen der Filter Einstellungen für die Filmlänge ist ein Fehler aufgetreten!",
                    exception);
        }

        try {
            viewSettingsPane.zeitraumSpinner.setValue(filterConfig.getZeitraum());
        } catch (Exception exception) {
            logger.debug("Beim wiederherstellen der Filter Einstellungen für den Zeitraum ist ein Fehler aufgetreten!",
                    exception);
        }
    }

    public CommonViewSettingsPaneSwing getViewSettingsPane(){
        return viewSettingsPane;
    }

    public OldSwingFilterDialog getFilterDialog() {
        return filterDialog;
    }

    public RangeSlider getFilmLengthSlider() {
        return filmLengthSlider;
    }

    public ZeitraumSpinnerSwing zeitraumProperty() {
        return zeitraumProperty;
    }

    public boolean isDontShowAudioVersions() {
        return dontShowAudioVersions.isSelected();
    }

    public JCheckBox dontShowAudioVersionsProperty() {
        return dontShowAudioVersions;
    }

    public boolean isDontShowSignLanguage() {
        return dontShowSignLanguage.isSelected();
    }

    public JCheckBox dontShowSignLanguageProperty() {
        return dontShowSignLanguage;
    }

    public boolean isDontShowTrailers() {
        return dontShowTrailers.isSelected();
    }

    public JCheckBox dontShowTrailersProperty() {
        return dontShowTrailers;
    }

    public boolean isDontShowAbos() {
        return dontShowAbos.isSelected();
    }

    public JCheckBox dontShowAbosProperty() {
        return dontShowAbos;
    }

    public boolean isShowLivestreamsOnly() {
        return showLivestreamsOnly.isSelected();
    }

    public JCheckBox showLivestreamsOnlyProperty() {
        return showLivestreamsOnly;
    }

    public boolean isShowUnseenOnly() {
        return showUnseenOnly.isSelected();
    }

    public JCheckBox showUnseenOnlyProperty() {
        return showUnseenOnly;
    }

    public boolean isShowBookMarkedOnly() {
        return showBookMarkedOnly.isSelected();
    }

    public JCheckBox showBookMarkedOnlyProperty() {
        return showBookMarkedOnly;
    }

    public boolean isShowSubtitlesOnly() {
        return showSubtitlesOnly.isSelected();
    }

    public JCheckBox showSubtitlesOnlyProperty() {
        return showSubtitlesOnly;
    }

    public boolean isShowOnlyHighQuality() {
        return showOnlyHighQuality.isSelected();
    }

    public JCheckBox showOnlyHighQualityProperty() {
        return showOnlyHighQuality;
    }

    public boolean isShowNewOnly() {
        return showNewOnly.isSelected();
    }

    public JCheckBox showNewOnlyProperty() {
        return showNewOnly;
    }

    private void setupConfigListeners() {
        // ActionListener für JCheckBox-Komponenten
        showOnlyHighQuality.addActionListener(e -> filterConfig.setShowHdOnly(showOnlyHighQuality.isSelected()));
        showSubtitlesOnly.addActionListener(e -> filterConfig.setShowSubtitlesOnly(showSubtitlesOnly.isSelected()));
        showBookMarkedOnly.addActionListener(e -> filterConfig.setShowBookMarkedOnly(showBookMarkedOnly.isSelected()));
        showNewOnly.addActionListener(e -> filterConfig.setShowNewOnly(showNewOnly.isSelected()));
        showUnseenOnly.addActionListener(e -> filterConfig.setShowUnseenOnly(showUnseenOnly.isSelected()));
        showLivestreamsOnly.addActionListener(e -> filterConfig.setShowLivestreamsOnly(showLivestreamsOnly.isSelected()));

        dontShowAbos.addActionListener(e -> filterConfig.setDontShowAbos(dontShowAbos.isSelected()));
        dontShowTrailers.addActionListener(e -> filterConfig.setDontShowTrailers(dontShowTrailers.isSelected()));
        dontShowSignLanguage.addActionListener(e -> filterConfig.setDontShowSignLanguage(dontShowSignLanguage.isSelected()));
        dontShowAudioVersions.addActionListener(e -> filterConfig.setDontShowAudioVersions(dontShowAudioVersions.isSelected()));

        // ChangeListener für JSlider-Komponenten (Film length slider)
        filmLengthSlider.addChangeListener(e -> filterConfig.setFilmLengthMin(filmLengthSlider.getLowValue()));
        filmLengthSlider.addChangeListener(e -> filterConfig.setFilmLengthMax(filmLengthSlider.getHighValue()));

        // ChangeListener für Spinner-Komponenten (Zeitraum)
        zeitraumProperty.addChangeListener(e -> filterConfig.setZeitraum(String.valueOf(zeitraumProperty.getValue())));
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

        var selectedSenders = viewSettingsPane.senderCheckList.getCheckBoxListSelectedValues();

        /*        var tempThemaList = getThemaList(selectedSenders).stream().distinct()
                .sorted(GermanStringSorter.getInstance())
                .toList();
        transactionThemaList.addAll(tempThemaList);
        transactionThemaList.commitEvent();*/

        viewSettingsPane.themaComboBox.setSelectedIndex(0);
    }


}

