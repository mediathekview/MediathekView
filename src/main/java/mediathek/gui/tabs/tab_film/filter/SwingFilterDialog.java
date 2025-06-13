/*
 * Copyright (c) 2025 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * Created by JFormDesigner on Sun Apr 06 12:46:21 CEST 2025
 */

package mediathek.gui.tabs.tab_film.filter;

import ca.odell.glazedlists.BasicEventList;
import ca.odell.glazedlists.EventList;
import ca.odell.glazedlists.FilterList;
import ca.odell.glazedlists.swing.GlazedListsSwing;
import com.jidesoft.swing.CheckBoxList;
import com.jidesoft.swing.JideSplitButton;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.controller.SenderFilmlistLoadApprover;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.gui.messages.ReloadTableDataEvent;
import mediathek.gui.messages.TableModelChangeEvent;
import mediathek.gui.tabs.tab_film.filter.zeitraum.ZeitraumSpinner;
import mediathek.gui.tabs.tab_film.filter_selection.FilterSelectionComboBox;
import mediathek.gui.tabs.tab_film.filter_selection.FilterSelectionComboBoxModel;
import mediathek.mainwindow.MediathekGui;
import mediathek.swing.AutoCompletionComboBox2;
import mediathek.swing.IconUtils;
import mediathek.tool.*;
import net.engio.mbassy.listener.Handler;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.configuration2.Configuration;
import org.apache.commons.configuration2.sync.LockMode;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jdesktop.swingx.VerticalLayout;
import org.jetbrains.annotations.NotNull;
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid;

import javax.swing.*;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.UUID;

/**
 * @author christianfranzke
 */
public class SwingFilterDialog extends JDialog {
    private static final Logger logger = LogManager.getLogger();
    private final FilterSelectionComboBoxModel filterSelectionComboBoxModel;
    private final Configuration config = ApplicationConfiguration.getConfiguration();
    private final JToggleButton filterToggleButton;
    private final FilterConfiguration filterConfig;
    /**
     * The "base" thema list
     */
    private final EventList<String> sourceThemaList = new BasicEventList<>();
    private final RenameFilterAction renameFilterAction = new RenameFilterAction();
    private final DeleteCurrentFilterAction deleteCurrentFilterAction = new DeleteCurrentFilterAction();
    private final AddNewFilterAction addNewFilterAction = new AddNewFilterAction();
    private final ResetCurrentFilterAction resetCurrentFilterAction = new ResetCurrentFilterAction();

    public SwingFilterDialog(@NotNull Window owner, @NotNull FilterSelectionComboBoxModel model,
                             @NotNull JToggleButton filterToggleButton,
                             @NotNull FilterConfiguration filterConfig) {
        super(owner);
        this.filterSelectionComboBoxModel = model;
        this.filterToggleButton = filterToggleButton;
        this.filterConfig = filterConfig;

        initComponents();

        setupRoundControls();

        btnSplit.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/ellipsis-vertical.svg"));
        populateSplitButton();

        ToggleVisibilityKeyHandler handler = new ToggleVisibilityKeyHandler(this);
        handler.installHandler(filterToggleButton.getAction());

        setupButtons();
        setupCheckBoxes();
        setupThemaComboBox();
        setupFilmLengthSlider();
        setupZeitraumSpinner();

        restoreConfigSettings();
        filterSelectionComboBoxModel.addListDataListener(new FilterSelectionDataListener());

        restoreWindowSizeFromConfig();
        restoreDialogVisibility();
        addComponentListener(new FilterDialogComponentListener());

        MessageBus.getMessageBus().subscribe(this);

        Daten.getInstance().getFilmeLaden().addAdListener(new FilmeLadenListener());
    }

    private void setupRoundControls() {
        cboxFilterSelection.putClientProperty("JComponent.roundRect", true);
        jcbThema.putClientProperty("JComponent.roundRect", true);
        spZeitraum.putClientProperty("JComponent.roundRect", true);
    }

    private void populateSplitButton() {
        btnSplit.add(renameFilterAction);
        btnSplit.add(addNewFilterAction);
        btnSplit.add(deleteCurrentFilterAction);
        btnSplit.addSeparator();
        btnSplit.add(resetCurrentFilterAction);
    }

    private void setupButtons() {
        checkDeleteCurrentFilterButtonState();
        btnResetThema.setAction(new ResetThemaAction());
    }

    private void setupCheckBoxes() {
        cbShowNewOnly.addActionListener(_ -> {
            filterConfig.setShowNewOnly(cbShowNewOnly.isSelected());
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
        cbShowBookMarkedOnly.addActionListener(_ -> {
            filterConfig.setShowBookMarkedOnly(cbShowBookMarkedOnly.isSelected());
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
        cbShowOnlyHq.addActionListener(_ -> {
            filterConfig.setShowHighQualityOnly(cbShowOnlyHq.isSelected());
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
        cbShowSubtitlesOnly.addActionListener(_ -> {
            filterConfig.setShowSubtitlesOnly(cbShowSubtitlesOnly.isSelected());
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
        cbShowOnlyLivestreams.addActionListener(_ -> {
            filterConfig.setShowLivestreamsOnly(cbShowOnlyLivestreams.isSelected());
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
        cbShowUnseenOnly.addActionListener(_ -> {
            filterConfig.setShowUnseenOnly(cbShowUnseenOnly.isSelected());
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
        cbDontShowAbos.addActionListener(_ -> {
            filterConfig.setDontShowAbos(cbDontShowAbos.isSelected());
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
        cbDontShowSignLanguage.addActionListener(_ -> {
            filterConfig.setDontShowSignLanguage(cbDontShowSignLanguage.isSelected());
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
        cbDontShowTrailers.addActionListener(_ -> {
            filterConfig.setDontShowTrailers(cbDontShowTrailers.isSelected());
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
        cbDontShowAudioVersions.addActionListener(_ -> {
            filterConfig.setDontShowAudioVersions(cbDontShowAudioVersions.isSelected());
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
        cbDontShowDuplicates.addActionListener(_ -> {
            filterConfig.setDontShowDuplicates(cbDontShowDuplicates.isSelected());
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });

        cboxFilterSelection.setMaximumSize(new Dimension(500, 100));
    }

    private void setupFilmLengthSlider() {
        //JFormDesigner dies when we morph bean from JSlider to RangeSlider
        var slider = (FilmLengthSlider) filmLengthSlider;

        lblMinFilmLengthValue.setText(String.valueOf(slider.getLowValue()));
        lblMaxFilmLengthValue.setText(slider.getHighValueText());

        slider.addChangeListener(_ -> {
            lblMinFilmLengthValue.setText(String.valueOf(slider.getLowValue()));
            lblMaxFilmLengthValue.setText(slider.getHighValueText());

            if (!slider.getValueIsAdjusting()) {
                filterConfig.setFilmLengthMin(slider.getLowValue());
                filterConfig.setFilmLengthMax(slider.getHighValue());
                MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
            }
        });
    }

    /**
     * Retrieve the list of all thema based on sender select checkbox list.
     *
     * @param selectedSenders the list of selected senders
     * @return list of all applicable themas.
     */
    private List<String> getThemaList(@NotNull java.util.List<String> selectedSenders) {
        List<String> finalList = new ArrayList<>();

        var blackList = Daten.getInstance().getListeFilmeNachBlackList();
        if (selectedSenders.isEmpty()) {
            finalList.addAll(blackList.getThemenUnprocessed(""));
        }
        else {
            for (String sender : selectedSenders) {
                finalList.addAll(blackList.getThemenUnprocessed(sender));
            }
        }

        return finalList.parallelStream().distinct()
                .sorted(GermanStringSorter.getInstance()).toList();
    }

    private void updateThemaComboBox() {
        //update the thema list -> updates the combobox automagically
        String aktuellesThema = (String) jcbThema.getSelectedItem();

        var selectedSenders = filterConfig.getCheckedChannels().stream().toList();
        var tempThemaList = getThemaList(selectedSenders);

        sourceThemaList.getReadWriteLock().writeLock().lock();
        sourceThemaList.clear();
        sourceThemaList.addAll(tempThemaList);
        sourceThemaList.getReadWriteLock().writeLock().unlock();

        if (!sourceThemaList.contains(aktuellesThema) && aktuellesThema != null && !aktuellesThema.isEmpty()) {
            sourceThemaList.add(aktuellesThema);
        }
        jcbThema.setSelectedItem(aktuellesThema);
    }

    private JPopupMenu createPopupMenu() {
        var popupMenu = new JPopupMenu();
        popupMenu.add(new ResetThemaButtonAction());
        return popupMenu;
    }

    private void setupThemaComboBox() {
        jcbThema.setComponentPopupMenu(createPopupMenu());

        var model = GlazedListsSwing.eventComboBoxModel(new EventListWithEmptyFirstEntry(sourceThemaList));
        jcbThema.setModel(model);
        //otherwise stored filter will not be accepted as entry may not be in list
        var thema = filterConfig.getThema();
        if (!sourceThemaList.contains(thema)) {
            sourceThemaList.add(thema);
        }
        jcbThema.setSelectedItem(thema);
        jcbThema.addActionListener(_ -> {
            var sel = (String) jcbThema.getSelectedItem();
            if (sel != null) {
                filterConfig.setThema(sel);
            }
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
    }

    private void setupZeitraumSpinner() {
        try {
            spZeitraum.restoreFilterConfig(filterConfig);
            spZeitraum.installFilterConfigurationChangeListener(filterConfig);
        }
        catch (Exception e) {
            logger.error("Failed to setup zeitraum spinner", e);
        }
    }

    private void checkDeleteCurrentFilterButtonState() {
        deleteCurrentFilterAction.setEnabled(filterConfig.getAvailableFilterCount() > 1);
    }

    private void restoreConfigSettings() {
        cbShowNewOnly.setSelected(filterConfig.isShowNewOnly());
        cbShowBookMarkedOnly.setSelected(filterConfig.isShowBookMarkedOnly());
        cbShowOnlyHq.setSelected(filterConfig.isShowHighQualityOnly());
        cbShowSubtitlesOnly.setSelected(filterConfig.isShowSubtitlesOnly());
        cbShowOnlyLivestreams.setSelected(filterConfig.isShowLivestreamsOnly());
        cbShowUnseenOnly.setSelected(filterConfig.isShowUnseenOnly());
        cbDontShowAbos.setSelected(filterConfig.isDontShowAbos());
        cbDontShowSignLanguage.setSelected(filterConfig.isDontShowSignLanguage());
        cbDontShowTrailers.setSelected(filterConfig.isDontShowTrailers());
        cbDontShowAudioVersions.setSelected(filterConfig.isDontShowAudioVersions());
        cbDontShowDuplicates.setSelected(filterConfig.isDontShowDuplicates());

        jcbThema.setSelectedItem(filterConfig.getThema());

        ((SenderCheckBoxList) senderList).restoreFilterConfig();
        ((FilmLengthSlider) filmLengthSlider).restoreFilterConfig(filterConfig);
        spZeitraum.restoreFilterConfig(filterConfig);
    }

    private void enableControls(boolean enable) {
        cboxFilterSelection.setEnabled(enable);

        btnSplit.setEnabled(enable);
        renameFilterAction.setEnabled(enable);
        addNewFilterAction.setEnabled(enable);
        resetCurrentFilterAction.setEnabled(enable);

        btnResetThema.setEnabled(enable);

        cbShowNewOnly.setEnabled(enable);
        cbShowBookMarkedOnly.setEnabled(enable);
        cbShowOnlyHq.setEnabled(enable);
        cbShowSubtitlesOnly.setEnabled(enable);
        cbShowOnlyLivestreams.setEnabled(enable);
        cbShowUnseenOnly.setEnabled(enable);
        cbDontShowAbos.setEnabled(enable);
        cbDontShowSignLanguage.setEnabled(enable);
        cbDontShowTrailers.setEnabled(enable);
        cbDontShowAudioVersions.setEnabled(enable);
        cbDontShowDuplicates.setEnabled(enable);

        label3.setEnabled(enable);
        senderList.setEnabled(enable);

        label4.setEnabled(enable);
        jcbThema.setEnabled(enable);

        label5.setEnabled(enable);
        label7.setEnabled(enable);
        lblMinFilmLengthValue.setEnabled(enable);
        lblMaxFilmLengthValue.setEnabled(enable);
        filmLengthSlider.setEnabled(enable);

        spZeitraum.setEnabled(enable);
        label1.setEnabled(enable);
        label2.setEnabled(enable);
    }

    @Override
    public void setEnabled(boolean enabled) {
        super.setEnabled(enabled);
        enableControls(enabled);
    }

    @Handler
    private void handleTableModelChangeEvent(TableModelChangeEvent e) {
        SwingUtilities.invokeLater(() -> {
            var enable = !e.active;
            setEnabled(enable);
            enableControls(enable);

            if (e.active) {
                deleteCurrentFilterAction.setEnabled(false);
            }
            else {
                checkDeleteCurrentFilterButtonState();
            }
        });
    }

    private void restoreDialogVisibility() {
        boolean visible = config.getBoolean(ApplicationConfiguration.FilterDialog.VISIBLE, false);
        setVisible(visible);
    }

    private void restoreWindowSizeFromConfig() {
        try {
            config.lock(LockMode.READ);
            final int width = config.getInt(ApplicationConfiguration.FilterDialog.WIDTH);
            final int height = config.getInt(ApplicationConfiguration.FilterDialog.HEIGHT);
            final int x = config.getInt(ApplicationConfiguration.FilterDialog.X);
            final int y = config.getInt(ApplicationConfiguration.FilterDialog.Y);

            setBounds(x, y, width, height);
        }
        catch (NoSuchElementException ignored) {
            //do not restore anything
        }
        finally {
            config.unlock(LockMode.READ);
        }

    }

    private void createUIComponents() {
        cboxFilterSelection = new FilterSelectionComboBox(filterSelectionComboBoxModel);
    }

    public static class ToggleVisibilityKeyHandler {
        private static final String TOGGLE_FILTER_VISIBILITY = "toggle_dialog_visibility";
        private final JRootPane rootPane;

        public ToggleVisibilityKeyHandler(JDialog dlg) {
            this.rootPane = dlg.getRootPane();
        }

        public void installHandler(Action action) {
            final var inputMap = rootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
            inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_F12, 0), TOGGLE_FILTER_VISIBILITY);
            rootPane.getActionMap().put(TOGGLE_FILTER_VISIBILITY, action);
        }
    }

    private class ResetCurrentFilterAction extends AbstractAction {
        public ResetCurrentFilterAction() {
            putValue(Action.SMALL_ICON, IconUtils.of(FontAwesomeSolid.RECYCLE));
            putValue(Action.SHORT_DESCRIPTION, "Aktuellen Filter zurücksetzen");
            putValue(Action.NAME, "Aktuellen Filter zurücksetzen" + "...");
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            var result = JOptionPane.showConfirmDialog(MediathekGui.ui(),
                    "Sind Sie sicher dass Sie den Filter zurücksetzen möchten?", "Filter zurücksetzen",
                    JOptionPane.YES_NO_OPTION);
            if (result == JOptionPane.YES_OPTION) {
                filterConfig.clearCurrentFilter();
                restoreConfigSettings();
            }
        }
    }

    private class SenderCheckBoxList extends CheckBoxList {
        private static final String CONFIG_SENDERLIST_VERTICAL_WRAP = "senderlist.vertical_wrap";
        private final JCheckBoxMenuItem miVerticalWrap = new JCheckBoxMenuItem("Senderliste vertikal umbrechen", false);

        public SenderCheckBoxList() {
            setVisibleRowCount(-1);

            setupSenderList();
            restoreVerticalWrapState();
        }

        private void restoreVerticalWrapState() {
            if (ApplicationConfiguration.getConfiguration().getBoolean(CONFIG_SENDERLIST_VERTICAL_WRAP, true)) {
                miVerticalWrap.doClick();
            }
        }

        protected void setupSenderList() {
            //here we show all senders as a filter might be set up for them...
            var allSenders = Daten.getInstance().getAllSendersList();
            var filteredList = new FilterList<>(allSenders, SenderFilmlistLoadApprover::isApproved);
            setModel(GlazedListsSwing.eventListModel(filteredList));
            getCheckBoxListSelectionModel().addListSelectionListener(e -> {
                if (!e.getValueIsAdjusting()) {
                    var newSelectedSenderList = getSelectedSenders();
                    filterConfig.setCheckedChannels(newSelectedSenderList);

                    updateThemaComboBox();
                    MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
                }
            });

            setupContextMenu();
        }

        protected void setupContextMenu() {
            var contextMenu = new JPopupMenu();
            var menuItem = new JMenuItem("Alle Senderfilter zurücksetzen");
            menuItem.addActionListener(_ -> selectNone());
            contextMenu.add(menuItem);
            contextMenu.addSeparator();

            miVerticalWrap.addActionListener(_ -> {
                boolean selected = miVerticalWrap.isSelected();
                ApplicationConfiguration.getConfiguration().setProperty(CONFIG_SENDERLIST_VERTICAL_WRAP, selected);
                if (selected) {
                    setLayoutOrientation(JList.VERTICAL_WRAP);
                }
                else {
                    setLayoutOrientation(JList.VERTICAL);
                }
                repaint();
            });
            contextMenu.add(miVerticalWrap);
            setComponentPopupMenu(contextMenu);
        }

        protected List<String> getSelectedSenders() {
            var newSelectedSenderList = new ArrayList<String>();
            final var senderListModel = getModel();
            final var cblsm = getCheckBoxListSelectionModel();
            for (int i = 0; i < senderListModel.getSize(); i++) {
                if (cblsm.isSelectedIndex(i)) {
                    var item = senderListModel.getElementAt(i);
                    newSelectedSenderList.add(item.toString());
                }
            }
            return newSelectedSenderList;
        }

        public void restoreFilterConfig() {
            final var checkedSenders = filterConfig.getCheckedChannels();
            final var cblsm = getCheckBoxListSelectionModel();
            final var senderListModel = getModel();

            selectNone();
            cblsm.setValueIsAdjusting(true);
            for (int i = 0; i < senderListModel.getSize(); i++) {
                var item = (String) senderListModel.getElementAt(i);
                if (checkedSenders.contains(item)) {
                    getCheckBoxListSelectionModel().addSelectionInterval(i, i);
                }
            }
            cblsm.setValueIsAdjusting(false);
        }
    }

    private class AddNewFilterAction extends AbstractAction {
        private static final String STR_ACTION_NAME = "Neuen Filter anlegen";

        public AddNewFilterAction() {
            putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/plus.svg"));
            putValue(Action.SHORT_DESCRIPTION, STR_ACTION_NAME);
            putValue(Action.NAME, STR_ACTION_NAME + "...");
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            String newFilterName = (String) JOptionPane.showInputDialog(MediathekGui.ui(), "Filtername:",
                    STR_ACTION_NAME, JOptionPane.PLAIN_MESSAGE, null, null,
                    String.format("Filter %d", filterConfig.getAvailableFilters().size() + 1));
            if (newFilterName != null) {
                filterConfig.findFilterForName(newFilterName).ifPresentOrElse(_ ->
                        JOptionPane.showMessageDialog(MediathekGui.ui(),
                                "Ein Filter mit dem gewählten Namen existiert bereits!",
                                STR_ACTION_NAME, JOptionPane.ERROR_MESSAGE), () -> {
                    FilterDTO newFilter = new FilterDTO(UUID.randomUUID(), newFilterName);
                    filterConfig.addNewFilter(newFilter);
                    checkDeleteCurrentFilterButtonState();
                    filterSelectionComboBoxModel.setSelectedItem(newFilter);
                });

            }
        }
    }

    private class DeleteCurrentFilterAction extends AbstractAction {
        private static final String STR_DELETE_CURRENT_FILTER = "Aktuellen Filter löschen";

        public DeleteCurrentFilterAction() {
            putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/trash-can.svg"));
            putValue(Action.SHORT_DESCRIPTION, STR_DELETE_CURRENT_FILTER);
            putValue(Action.NAME, STR_DELETE_CURRENT_FILTER + "...");
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            var res = JOptionPane.showConfirmDialog(MediathekGui.ui(), "Möchten Sie wirklich den aktuellen Filter löschen?",
                    Konstanten.PROGRAMMNAME, JOptionPane.YES_NO_OPTION);
            if (res == JOptionPane.YES_OPTION) {
                FilterDTO filterToDelete = filterConfig.getCurrentFilter();
                filterConfig.deleteFilter(filterToDelete);

                checkDeleteCurrentFilterButtonState();
            }
        }
    }

    private class ResetThemaAction extends AbstractAction {
        protected static final String STR_RESET_THEMA = "Thema zurücksetzen";

        public ResetThemaAction() {
            putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/trash-can.svg"));
            putValue(Action.SHORT_DESCRIPTION, STR_RESET_THEMA);
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            filterConfig.setThema("");
            jcbThema.setSelectedIndex(0);
        }
    }

    private class ResetThemaButtonAction extends ResetThemaAction {
        public ResetThemaButtonAction() {
            putValue(Action.NAME, STR_RESET_THEMA);
        }
    }

    private class RenameFilterAction extends AbstractAction {
        private static final String STR_RENAME_FILTER = "Filter umbenennen";

        public RenameFilterAction() {
            putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/pen-to-square.svg"));
            putValue(Action.SHORT_DESCRIPTION, STR_RENAME_FILTER);
            putValue(Action.NAME, STR_RENAME_FILTER + "...");
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            final var fltName = filterConfig.getCurrentFilter().name();
            String s = (String) JOptionPane.showInputDialog(MediathekGui.ui(), "Neuer Name des Filters:", "Filter umbenennen", JOptionPane.PLAIN_MESSAGE, null, null, fltName);
            if (s != null) {
                if (!s.isEmpty()) {
                    final var fName = s.trim();
                    if (!fName.equals(fltName)) {
                        var existingFilter = filterConfig.findFilterForName(fName);
                        existingFilter.ifPresentOrElse(_ -> {
                            //if a filter already exists we cannot rename...
                            JOptionPane.showMessageDialog(MediathekGui.ui(),
                                    String.format("Filter %s existiert bereits.\nAktion wird abgebrochen", fName),
                                    Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE);
                        }, () -> {
                            // no existing name...
                            Configuration config = ApplicationConfiguration.getConfiguration();
                            config.lock(LockMode.WRITE);
                            var thema = filterConfig.getThema();
                            filterConfig.setThema("");
                            filterConfig.renameCurrentFilter(fName);
                            filterConfig.setThema(thema);
                            config.unlock(LockMode.WRITE);
                            logger.trace("Renamed filter \"{}\" to \"{}\"", fltName, fName);
                        });
                    }
                    else
                        logger.warn("New and old filter name are identical...doing nothing");
                }
                else {
                    JOptionPane.showMessageDialog(MediathekGui.ui(), "Filtername darf nicht leer sein!",
                            Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE);
                    logger.warn("Rename filter text was empty...doing nothing");
                }
            }
        }
    }

    public class FilterDialogComponentListener extends ComponentAdapter {
        @Override
        public void componentResized(ComponentEvent e) {
            storeWindowPosition(e);
        }

        @Override
        public void componentMoved(ComponentEvent e) {
            storeWindowPosition(e);
        }

        @Override
        public void componentShown(ComponentEvent e) {
            storeDialogVisibility();
            filterToggleButton.setSelected(true);
        }

        @Override
        public void componentHidden(ComponentEvent e) {
            storeWindowPosition(e);
            storeDialogVisibility();

            filterToggleButton.setSelected(false);
        }

        private void storeDialogVisibility() {
            config.setProperty(ApplicationConfiguration.FilterDialog.VISIBLE, isVisible());
        }

        private void storeWindowPosition(ComponentEvent e) {
            var component = e.getComponent();

            var dims = component.getSize();
            var loc = component.getLocation();
            try {
                config.lock(LockMode.WRITE);
                config.setProperty(ApplicationConfiguration.FilterDialog.WIDTH, dims.width);
                config.setProperty(ApplicationConfiguration.FilterDialog.HEIGHT, dims.height);
                config.setProperty(ApplicationConfiguration.FilterDialog.X, loc.x);
                config.setProperty(ApplicationConfiguration.FilterDialog.Y, loc.y);
            }
            finally {
                config.unlock(LockMode.WRITE);
            }
        }
    }

    private class FilmeLadenListener extends ListenerFilmeLaden {
        @Override
        public void start(ListenerFilmeLadenEvent event) {
            setEnabled(false);
        }

        @Override
        public void fertig(ListenerFilmeLadenEvent event) {
            updateThemaComboBox();
            setEnabled(true);
        }
    }

    private class FilterSelectionDataListener implements ListDataListener {

        @Override
        public void intervalAdded(ListDataEvent e) {
            restoreConfigSettings();
        }

        @Override
        public void intervalRemoved(ListDataEvent e) {
            restoreConfigSettings();
        }

        @Override
        public void contentsChanged(ListDataEvent e) {
            restoreConfigSettings();
        }
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
        // Generated using JFormDesigner non-commercial license
        createUIComponents();

        var pnlFilterCommon = new JPanel();
        btnSplit = new JideSplitButton();
        var separator2 = new JSeparator();
        var pnlShowOnly = new JPanel();
        cbShowNewOnly = new JCheckBox();
        cbShowBookMarkedOnly = new JCheckBox();
        cbShowOnlyHq = new JCheckBox();
        cbShowSubtitlesOnly = new JCheckBox();
        cbShowOnlyLivestreams = new JCheckBox();
        var separator3 = new JSeparator();
        var pnlDontShow = new JPanel();
        cbShowUnseenOnly = new JCheckBox();
        cbDontShowAbos = new JCheckBox();
        cbDontShowSignLanguage = new JCheckBox();
        cbDontShowTrailers = new JCheckBox();
        cbDontShowAudioVersions = new JCheckBox();
        cbDontShowDuplicates = new JCheckBox();
        var separator4 = new JSeparator();
        var pnlSenderlist = new JPanel();
        label3 = new JLabel();
        var scpSenderList = new JScrollPane();
        senderList = new SenderCheckBoxList();
        var separator5 = new JSeparator();
        var pnlThema = new JPanel();
        label4 = new JLabel();
        jcbThema = new AutoCompletionComboBox2();
        btnResetThema = new JButton();
        var separator6 = new JSeparator();
        var pnlFlimlength = new JPanel();
        label5 = new JLabel();
        lblMinFilmLengthValue = new JLabel();
        var hSpacer1 = new JPanel(null);
        label7 = new JLabel();
        lblMaxFilmLengthValue = new JLabel();
        filmLengthSlider = new FilmLengthSlider();
        var separator7 = new JSeparator();
        var pnlZeitraum = new JPanel();
        label1 = new JLabel();
        spZeitraum = new ZeitraumSpinner();
        label2 = new JLabel();

        //======== this ========
        setType(Window.Type.UTILITY);
        setTitle("Filter"); //NON-NLS
        var contentPane = getContentPane();
        contentPane.setLayout(new MigLayout(
            new LC().fillX().insets("5").hideMode(3), //NON-NLS
            // columns
            new AC()
                .align("left"), //NON-NLS
            // rows
            new AC()
                .gap()
                .shrink(0).align("top").gap("0") //NON-NLS
                .gap("0") //NON-NLS
                .shrink(0).gap("0") //NON-NLS
                .gap("0") //NON-NLS
                .shrink(0).gap()
                .grow().fill().gap()
                .shrink(0).gap()
                .gap()
                .shrink(0).gap()
                .gap()
                .shrink(0).gap()
                ));

        //======== pnlFilterCommon ========
        {
            pnlFilterCommon.setLayout(new MigLayout(
                new LC().fillX().insets("0").hideMode(3), //NON-NLS
                // columns
                new AC()
                    .grow().fill().gap()
                    .fill(),
                // rows
                new AC()
                    .fill()));

            //---- cboxFilterSelection ----
            cboxFilterSelection.setMaximumSize(null);
            cboxFilterSelection.setPreferredSize(null);
            cboxFilterSelection.setMinimumSize(new Dimension(50, 10));
            pnlFilterCommon.add(cboxFilterSelection, new CC().cell(0, 0));

            //======== btnSplit ========
            {
                btnSplit.setAlwaysDropdown(true);
            }
            pnlFilterCommon.add(btnSplit, new CC().cell(1, 0));
        }
        contentPane.add(pnlFilterCommon, new CC().cell(0, 0).growX());
        contentPane.add(separator2, new CC().cell(0, 1).growX());

        //======== pnlShowOnly ========
        {
            pnlShowOnly.setLayout(new VerticalLayout());

            //---- cbShowNewOnly ----
            cbShowNewOnly.setText("Nur neue Filme anzeigen"); //NON-NLS
            pnlShowOnly.add(cbShowNewOnly);

            //---- cbShowBookMarkedOnly ----
            cbShowBookMarkedOnly.setText("Nur gemerkte Filme anzeigen"); //NON-NLS
            pnlShowOnly.add(cbShowBookMarkedOnly);

            //---- cbShowOnlyHq ----
            cbShowOnlyHq.setText("Nur High Quality(HQ) Filme anzeigen"); //NON-NLS
            pnlShowOnly.add(cbShowOnlyHq);

            //---- cbShowSubtitlesOnly ----
            cbShowSubtitlesOnly.setText("Nur Filme mit Untertitel anzeigen"); //NON-NLS
            pnlShowOnly.add(cbShowSubtitlesOnly);

            //---- cbShowOnlyLivestreams ----
            cbShowOnlyLivestreams.setText("Nur Livestreams anzeigen"); //NON-NLS
            pnlShowOnly.add(cbShowOnlyLivestreams);
        }
        contentPane.add(pnlShowOnly, new CC().cell(0, 2).growX());
        contentPane.add(separator3, new CC().cell(0, 3).growX());

        //======== pnlDontShow ========
        {
            pnlDontShow.setLayout(new VerticalLayout());

            //---- cbShowUnseenOnly ----
            cbShowUnseenOnly.setText("Gesehene Filme nicht anzeigen"); //NON-NLS
            pnlDontShow.add(cbShowUnseenOnly);

            //---- cbDontShowAbos ----
            cbDontShowAbos.setText("Abos nicht anzeigen"); //NON-NLS
            pnlDontShow.add(cbDontShowAbos);

            //---- cbDontShowSignLanguage ----
            cbDontShowSignLanguage.setText("Geb\u00e4rdensprache nicht anzeigen"); //NON-NLS
            pnlDontShow.add(cbDontShowSignLanguage);

            //---- cbDontShowTrailers ----
            cbDontShowTrailers.setText("Trailer/Teaser/Vorschau nicht anzeigen"); //NON-NLS
            pnlDontShow.add(cbDontShowTrailers);

            //---- cbDontShowAudioVersions ----
            cbDontShowAudioVersions.setText("H\u00f6rfassungen ausblenden"); //NON-NLS
            pnlDontShow.add(cbDontShowAudioVersions);

            //---- cbDontShowDuplicates ----
            cbDontShowDuplicates.setText("Duplikate nicht anzeigen"); //NON-NLS
            pnlDontShow.add(cbDontShowDuplicates);
        }
        contentPane.add(pnlDontShow, new CC().cell(0, 4).growX());
        contentPane.add(separator4, new CC().cell(0, 5).growX());

        //======== pnlSenderlist ========
        {
            pnlSenderlist.setPreferredSize(new Dimension(258, 220));
            pnlSenderlist.setLayout(new MigLayout(
                new LC().fill().insets("0").hideMode(3), //NON-NLS
                // columns
                new AC()
                    .align("left"), //NON-NLS
                // rows
                new AC()
                    .gap()
                    .grow()));

            //---- label3 ----
            label3.setText("Sender:"); //NON-NLS
            pnlSenderlist.add(label3, new CC().cell(0, 0));

            //======== scpSenderList ========
            {
                scpSenderList.setPreferredSize(null);
                scpSenderList.setMaximumSize(null);

                //---- senderList ----
                senderList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
                senderList.setMaximumSize(null);
                senderList.setMinimumSize(null);
                senderList.setPreferredSize(null);
                scpSenderList.setViewportView(senderList);
            }
            pnlSenderlist.add(scpSenderList, new CC().cell(0, 1).grow().minHeight("50")); //NON-NLS
        }
        contentPane.add(pnlSenderlist, new CC().cell(0, 6).growX());
        contentPane.add(separator5, new CC().cell(0, 7).growX());

        //======== pnlThema ========
        {
            pnlThema.setLayout(new MigLayout(
                new LC().fillX().insets("0").hideMode(3), //NON-NLS
                // columns
                new AC()
                    .align("left").gap() //NON-NLS
                    .grow().fill().gap()
                    .fill(),
                // rows
                new AC()
                    ));

            //---- label4 ----
            label4.setText("Thema:"); //NON-NLS
            pnlThema.add(label4, new CC().cell(0, 0));

            //---- jcbThema ----
            jcbThema.setMinimumSize(new Dimension(50, 10));
            jcbThema.setPreferredSize(null);
            jcbThema.setMaximumSize(null);
            pnlThema.add(jcbThema, new CC().cell(1, 0).growX());
            pnlThema.add(btnResetThema, new CC().cell(2, 0));
        }
        contentPane.add(pnlThema, new CC().cell(0, 8).growX());
        contentPane.add(separator6, new CC().cell(0, 9).growX());

        //======== pnlFlimlength ========
        {
            pnlFlimlength.setLayout(new MigLayout(
                new LC().fill().insets("0").hideMode(3), //NON-NLS
                // columns
                new AC()
                    .fill().gap()
                    .fill().gap()
                    .grow().fill().gap()
                    .fill().gap()
                    .fill(),
                // rows
                new AC()
                    .gap("0") //NON-NLS
                    ));

            //---- label5 ----
            label5.setText("Mindestl\u00e4nge:"); //NON-NLS
            pnlFlimlength.add(label5, new CC().cell(0, 0));

            //---- lblMinFilmLengthValue ----
            lblMinFilmLengthValue.setText("0"); //NON-NLS
            pnlFlimlength.add(lblMinFilmLengthValue, new CC().cell(1, 0));
            pnlFlimlength.add(hSpacer1, new CC().cell(2, 0).growX());

            //---- label7 ----
            label7.setText("Maximall\u00e4nge:"); //NON-NLS
            pnlFlimlength.add(label7, new CC().cell(3, 0));

            //---- lblMaxFilmLengthValue ----
            lblMaxFilmLengthValue.setText("100"); //NON-NLS
            pnlFlimlength.add(lblMaxFilmLengthValue, new CC().cell(4, 0));
            pnlFlimlength.add(filmLengthSlider, new CC().cell(0, 1, 5, 1).growX());
        }
        contentPane.add(pnlFlimlength, new CC().cell(0, 10).growX());
        contentPane.add(separator7, new CC().cell(0, 11).growX());

        //======== pnlZeitraum ========
        {
            pnlZeitraum.setLayout(new MigLayout(
                new LC().fillX().insets("0").hideMode(3), //NON-NLS
                // columns
                new AC()
                    .align("left").gap() //NON-NLS
                    .grow().fill().gap()
                    .fill(),
                // rows
                new AC()
                    ));

            //---- label1 ----
            label1.setText("Zeitraum:"); //NON-NLS
            pnlZeitraum.add(label1, new CC().cell(0, 0));
            pnlZeitraum.add(spZeitraum, new CC().cell(1, 0));

            //---- label2 ----
            label2.setText("Tage"); //NON-NLS
            pnlZeitraum.add(label2, new CC().cell(2, 0));
        }
        contentPane.add(pnlZeitraum, new CC().cell(0, 12).growX());
        pack();
        setLocationRelativeTo(getOwner());
        // JFormDesigner - End of component initialization  //GEN-END:initComponents  @formatter:on
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables  @formatter:off
    // Generated using JFormDesigner non-commercial license
    private FilterSelectionComboBox cboxFilterSelection;
    private JideSplitButton btnSplit;
    private JCheckBox cbShowNewOnly;
    private JCheckBox cbShowBookMarkedOnly;
    private JCheckBox cbShowOnlyHq;
    private JCheckBox cbShowSubtitlesOnly;
    private JCheckBox cbShowOnlyLivestreams;
    private JCheckBox cbShowUnseenOnly;
    private JCheckBox cbDontShowAbos;
    private JCheckBox cbDontShowSignLanguage;
    private JCheckBox cbDontShowTrailers;
    private JCheckBox cbDontShowAudioVersions;
    private JCheckBox cbDontShowDuplicates;
    private JLabel label3;
    public CheckBoxList senderList;
    private JLabel label4;
    private AutoCompletionComboBox2 jcbThema;
    private JButton btnResetThema;
    private JLabel label5;
    private JLabel lblMinFilmLengthValue;
    private JLabel label7;
    private JLabel lblMaxFilmLengthValue;
    private JSlider filmLengthSlider;
    private JLabel label1;
    public ZeitraumSpinner spZeitraum;
    private JLabel label2;
    // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
