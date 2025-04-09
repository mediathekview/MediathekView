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
import ca.odell.glazedlists.swing.GlazedListsSwing;
import com.jidesoft.swing.CheckBoxList;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.gui.messages.ReloadTableDataEvent;
import mediathek.gui.messages.TableModelChangeEvent;
import mediathek.gui.tabs.tab_film.filter.zeitraum.ZeitraumSpinner;
import mediathek.gui.tabs.tab_film.filter_selection.FilterSelectionComboBox;
import mediathek.gui.tabs.tab_film.filter_selection.FilterSelectionComboBoxModel;
import mediathek.mainwindow.MediathekGui;
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
import org.jetbrains.annotations.NotNull;

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

    public SwingFilterDialog(@NotNull Window owner, @NotNull FilterSelectionComboBoxModel model,
                             @NotNull JToggleButton filterToggleButton,
                             @NotNull FilterConfiguration filterConfig) {
        super(owner);
        this.filterSelectionComboBoxModel = model;
        this.filterToggleButton = filterToggleButton;
        this.filterConfig = filterConfig;

        initComponents();

        ToggleVisibilityKeyHandler handler = new ToggleVisibilityKeyHandler(this);
        handler.installHandler(filterToggleButton.getAction());

        setupButtons();
        setupCheckBoxes();
        setupSenderList();
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

    private void setupButtons() {
        btnRenameFilter.setAction(new RenameFilterAction());
        setupDeleteCurrentFilterButton();
        setupResetCurrentFilterButton();
        btnAddNewFilter.setAction(new AddNewFilterAction());
        btnResetThema.setAction(new ResetThemaAction());
    }

    private void setupCheckBoxes() {
        cbShowNewOnly.addActionListener(l -> {
            filterConfig.setShowNewOnly(cbShowNewOnly.isSelected());
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
        cbShowBookMarkedOnly.addActionListener(l -> {
            filterConfig.setShowBookMarkedOnly(cbShowBookMarkedOnly.isSelected());
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
        cbShowOnlyHq.addActionListener(l -> {
            filterConfig.setShowHighQualityOnly(cbShowOnlyHq.isSelected());
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
        cbShowSubtitlesOnly.addActionListener(l -> {
            filterConfig.setShowSubtitlesOnly(cbShowSubtitlesOnly.isSelected());
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
        cbShowOnlyLivestreams.addActionListener(l -> {
            filterConfig.setShowLivestreamsOnly(cbShowOnlyLivestreams.isSelected());
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
        cbShowUnseenOnly.addActionListener(l -> {
            filterConfig.setShowUnseenOnly(cbShowUnseenOnly.isSelected());
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
        cbDontShowAbos.addActionListener(l -> {
            filterConfig.setDontShowAbos(cbDontShowAbos.isSelected());
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
        cbDontShowSignLanguage.addActionListener(l -> {
            filterConfig.setDontShowSignLanguage(cbDontShowSignLanguage.isSelected());
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
        cbDontShowTrailers.addActionListener(l -> {
            filterConfig.setDontShowTrailers(cbDontShowTrailers.isSelected());
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
        cbDontShowAudioVersions.addActionListener(l -> {
            filterConfig.setDontShowAudioVersions(cbDontShowAudioVersions.isSelected());
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
        cbDontShowDuplicates.addActionListener(l -> {
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

        slider.addChangeListener(l -> {
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

        final var blackList = Daten.getInstance().getListeFilmeNachBlackList();
        if (selectedSenders.isEmpty()) {
            finalList.addAll(blackList.getThemenUnprocessed(""));
        } else {
            for (String sender : selectedSenders) {
                finalList.addAll(blackList.getThemenUnprocessed(sender));
            }
        }

        return finalList.parallelStream().distinct()
                .sorted(GermanStringSorter.getInstance()).toList();
    }

    private void updateThemaComboBox() {
        //update the thema list -> updates the combobox automagically
        //use transaction list to minimize updates...
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

    private void setupThemaComboBox() {
        var model = GlazedListsSwing.eventComboBoxModel(new EventListWithEmptyFirstEntry(sourceThemaList));
        jcbThema.setModel(model);
        //otherwise stored filter will not be accepted as entry may not be in list
        var thema = filterConfig.getThema();
        if (!sourceThemaList.contains(thema)) {
            sourceThemaList.add(thema);
        }
        jcbThema.setSelectedItem(thema);
        jcbThema.addActionListener(l -> {
            var sel = (String) jcbThema.getSelectedItem();
            if (sel != null) {
                filterConfig.setThema(sel);
            }
            MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
        });
    }

    private void setupSenderList() {
        //here we show all senders as a filter might be set up for them...
        senderList.setModel(GlazedListsSwing.eventListModel(Daten.getInstance().getAllSendersList()));
        senderList.getCheckBoxListSelectionModel().addListSelectionListener(e -> {
            if (!e.getValueIsAdjusting()) {
                var newSelectedSenderList = ((SenderCheckBoxList) senderList).getSelectedSenders();
                filterConfig.setCheckedChannels(newSelectedSenderList);

                updateThemaComboBox();
                MessageBus.getMessageBus().publish(new ReloadTableDataEvent());
            }
        });

        var contextMenu = new JPopupMenu();
        var menuItem = new JMenuItem("Alle Senderfilter zurücksetzen");
        menuItem.addActionListener(l -> senderList.selectNone());
        contextMenu.add(menuItem);
        senderList.setComponentPopupMenu(contextMenu);

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
        btnDeleteCurrentFilter.setEnabled(filterConfig.getAvailableFilterCount() > 1);
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

        ((SenderCheckBoxList) senderList).restoreFilterConfig(filterConfig);
        ((FilmLengthSlider) filmLengthSlider).restoreFilterConfig(filterConfig);
        spZeitraum.restoreFilterConfig(filterConfig);
    }

    private void setupResetCurrentFilterButton() {
        btnResetCurrentFilter.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/recycle.svg"));
        btnResetCurrentFilter.addActionListener(e -> {
            filterConfig.clearCurrentFilter();
            restoreConfigSettings();
        });
    }

    private void setupDeleteCurrentFilterButton() {
        checkDeleteCurrentFilterButtonState();
        btnDeleteCurrentFilter.setAction(new DeleteCurrentFilterAction());
    }

    private void enableControls(boolean enable) {
        cboxFilterSelection.setEnabled(enable);

        btnRenameFilter.setEnabled(enable);
        btnAddNewFilter.setEnabled(enable);
        btnResetCurrentFilter.setEnabled(enable);

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
                btnDeleteCurrentFilter.setEnabled(false);
            } else {
                checkDeleteCurrentFilterButtonState();
            }
        });
    }

    private void restoreDialogVisibility() {
        final boolean visible = config.getBoolean(ApplicationConfiguration.FilterDialog.VISIBLE, false);
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
        } catch (NoSuchElementException ignored) {
            //do not restore anything
        } finally {
            config.unlock(LockMode.READ);
        }

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

    private class SenderCheckBoxList extends CheckBoxList {
        public List<String> getSelectedSenders() {
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

        public void restoreFilterConfig(@NotNull FilterConfiguration filterConfig) {
            final var checkedSenders = filterConfig.getCheckedChannels();
            final var cblsm = getCheckBoxListSelectionModel();
            final var senderListModel = senderList.getModel();

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
        public AddNewFilterAction() {
            putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/plus.svg"));
            putValue(Action.SHORT_DESCRIPTION, "Neuen Filter anlegen");
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            FilterDTO newFilter = new FilterDTO(UUID.randomUUID(), String.format("Filter %d", filterConfig.getAvailableFilters().size() + 1));
            filterConfig.addNewFilter(newFilter);
            checkDeleteCurrentFilterButtonState();
            filterSelectionComboBoxModel.setSelectedItem(newFilter);
        }
    }

    private class DeleteCurrentFilterAction extends AbstractAction {
        public DeleteCurrentFilterAction() {
            putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/trash-can.svg"));
            putValue(Action.SHORT_DESCRIPTION, "Aktuellen Filter löschen");
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
        public ResetThemaAction() {
            putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/trash-can.svg"));
            putValue(Action.SHORT_DESCRIPTION, "Thema zurücksetzen");
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            filterConfig.setThema("");
            jcbThema.setSelectedIndex(0);
        }
    }

    private class RenameFilterAction extends AbstractAction {
        public RenameFilterAction() {
            putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/pen-to-square.svg"));
            putValue(Action.SHORT_DESCRIPTION, "Filter umbenennen");
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
                        existingFilter.ifPresentOrElse(f -> {
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
                    } else
                        logger.warn("New and old filter name are identical...doing nothing");
                } else {
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
            } finally {
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

    private void createUIComponents() {
        cboxFilterSelection = new FilterSelectionComboBox(filterSelectionComboBoxModel);
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
        // Generated using JFormDesigner non-commercial license
        createUIComponents();

        var panel1 = new JPanel();
        btnRenameFilter = new JButton();
        btnAddNewFilter = new JButton();
        btnDeleteCurrentFilter = new JButton();
        var separator1 = new JSeparator();
        btnResetCurrentFilter = new JButton();
        var separator2 = new JSeparator();
        cbShowNewOnly = new JCheckBox();
        cbShowBookMarkedOnly = new JCheckBox();
        cbShowOnlyHq = new JCheckBox();
        cbShowSubtitlesOnly = new JCheckBox();
        cbShowOnlyLivestreams = new JCheckBox();
        var separator3 = new JSeparator();
        cbShowUnseenOnly = new JCheckBox();
        cbDontShowAbos = new JCheckBox();
        cbDontShowSignLanguage = new JCheckBox();
        cbDontShowTrailers = new JCheckBox();
        cbDontShowAudioVersions = new JCheckBox();
        cbDontShowDuplicates = new JCheckBox();
        var separator4 = new JSeparator();
        label3 = new JLabel();
        var scrollPane1 = new JScrollPane();
        senderList = new SenderCheckBoxList();
        var separator5 = new JSeparator();
        label4 = new JLabel();
        jcbThema = new JComboBox<>();
        btnResetThema = new JButton();
        var separator6 = new JSeparator();
        var panel2 = new JPanel();
        label5 = new JLabel();
        lblMinFilmLengthValue = new JLabel();
        var hSpacer1 = new JPanel(null);
        label7 = new JLabel();
        lblMaxFilmLengthValue = new JLabel();
        filmLengthSlider = new FilmLengthSlider();
        var separator7 = new JSeparator();
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
                .align("left").gap() //NON-NLS
                .grow().fill().gap()
                .fill(),
            // rows
            new AC()
                .gap()
                .shrink(0).align("top").gap("0") //NON-NLS
                .gap("0") //NON-NLS
                .gap("0") //NON-NLS
                .gap("0") //NON-NLS
                .gap("0") //NON-NLS
                .gap("0") //NON-NLS
                .shrink(0).gap("0") //NON-NLS
                .gap("0") //NON-NLS
                .gap("0") //NON-NLS
                .gap("0") //NON-NLS
                .gap("0") //NON-NLS
                .gap("0") //NON-NLS
                .gap("0") //NON-NLS
                .shrink(0).gap()
                .gap()
                .grow().gap()
                .shrink(0).gap()
                .gap()
                .shrink(0).gap()
                .gap()
                .shrink(0).gap()
                ));

        //======== panel1 ========
        {
            panel1.setLayout(new MigLayout(
                new LC().fillX().insets("0").hideMode(3), //NON-NLS
                // columns
                new AC()
                    .grow().fill().gap()
                    .fill().gap()
                    .fill().gap()
                    .align("left").gap() //NON-NLS
                    .fill().gap()
                    .fill(),
                // rows
                new AC()
                    .grow().fill()));
            panel1.add(cboxFilterSelection, new CC().cell(0, 0));
            panel1.add(btnRenameFilter, new CC().cell(1, 0).alignX("center").growX(0)); //NON-NLS
            panel1.add(btnAddNewFilter, new CC().cell(2, 0).alignX("center").growX(0)); //NON-NLS
            panel1.add(btnDeleteCurrentFilter, new CC().cell(3, 0).alignX("center").growX(0)); //NON-NLS

            //---- separator1 ----
            separator1.setOrientation(SwingConstants.VERTICAL);
            panel1.add(separator1, new CC().cell(4, 0));

            //---- btnResetCurrentFilter ----
            btnResetCurrentFilter.setToolTipText("Aktuellen Filter zur\u00fccksetzen"); //NON-NLS
            panel1.add(btnResetCurrentFilter, new CC().cell(5, 0).alignX("center").growX(0)); //NON-NLS
        }
        contentPane.add(panel1, new CC().cell(0, 0, 3, 1).growX());
        contentPane.add(separator2, new CC().cell(0, 1, 3, 1).growX());

        //---- cbShowNewOnly ----
        cbShowNewOnly.setText("Nur neue Filme anzeigen"); //NON-NLS
        contentPane.add(cbShowNewOnly, new CC().cell(0, 2, 3, 1));

        //---- cbShowBookMarkedOnly ----
        cbShowBookMarkedOnly.setText("Nur gemerkte Filme anzeigen"); //NON-NLS
        contentPane.add(cbShowBookMarkedOnly, new CC().cell(0, 3, 3, 1));

        //---- cbShowOnlyHq ----
        cbShowOnlyHq.setText("Nur High Quality(HQ) Filme anzeigen"); //NON-NLS
        contentPane.add(cbShowOnlyHq, new CC().cell(0, 4, 3, 1));

        //---- cbShowSubtitlesOnly ----
        cbShowSubtitlesOnly.setText("Nur Filme mit Untertitel anzeigen"); //NON-NLS
        contentPane.add(cbShowSubtitlesOnly, new CC().cell(0, 5, 3, 1));

        //---- cbShowOnlyLivestreams ----
        cbShowOnlyLivestreams.setText("Nur Livestreams anzeigen"); //NON-NLS
        contentPane.add(cbShowOnlyLivestreams, new CC().cell(0, 6, 3, 1));
        contentPane.add(separator3, new CC().cell(0, 7, 3, 1).growX());

        //---- cbShowUnseenOnly ----
        cbShowUnseenOnly.setText("Gesehene Filme nicht anzeigen"); //NON-NLS
        contentPane.add(cbShowUnseenOnly, new CC().cell(0, 8, 3, 1));

        //---- cbDontShowAbos ----
        cbDontShowAbos.setText("Abos nicht anzeigen"); //NON-NLS
        contentPane.add(cbDontShowAbos, new CC().cell(0, 9, 3, 1));

        //---- cbDontShowSignLanguage ----
        cbDontShowSignLanguage.setText("Geb\u00e4rdensprache nicht anzeigen"); //NON-NLS
        contentPane.add(cbDontShowSignLanguage, new CC().cell(0, 10, 3, 1));

        //---- cbDontShowTrailers ----
        cbDontShowTrailers.setText("Trailer/Teaser/Vorschau nicht anzeigen"); //NON-NLS
        contentPane.add(cbDontShowTrailers, new CC().cell(0, 11, 3, 1));

        //---- cbDontShowAudioVersions ----
        cbDontShowAudioVersions.setText("H\u00f6rfassungen ausblenden"); //NON-NLS
        contentPane.add(cbDontShowAudioVersions, new CC().cell(0, 12, 3, 1));

        //---- cbDontShowDuplicates ----
        cbDontShowDuplicates.setText("Duplikate nicht anzeigen"); //NON-NLS
        contentPane.add(cbDontShowDuplicates, new CC().cell(0, 13, 3, 1));
        contentPane.add(separator4, new CC().cell(0, 14, 3, 1).growX());

        //---- label3 ----
        label3.setText("Sender:"); //NON-NLS
        contentPane.add(label3, new CC().cell(0, 15, 3, 1));

        //======== scrollPane1 ========
        {

            //---- senderList ----
            senderList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
            scrollPane1.setViewportView(senderList);
        }
        contentPane.add(scrollPane1, new CC().cell(0, 16, 3, 1).grow().minHeight("50")); //NON-NLS
        contentPane.add(separator5, new CC().cell(0, 17, 3, 1).growX());

        //---- label4 ----
        label4.setText("Thema:"); //NON-NLS
        contentPane.add(label4, new CC().cell(0, 18));

        //---- jcbThema ----
        jcbThema.setMaximumRowCount(6);
        contentPane.add(jcbThema, new CC().cell(1, 18).growX().maxWidth("300")); //NON-NLS
        contentPane.add(btnResetThema, new CC().cell(2, 18));
        contentPane.add(separator6, new CC().cell(0, 19, 3, 1).growX());

        //======== panel2 ========
        {
            panel2.setLayout(new MigLayout(
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
            panel2.add(label5, new CC().cell(0, 0));

            //---- lblMinFilmLengthValue ----
            lblMinFilmLengthValue.setText("0"); //NON-NLS
            panel2.add(lblMinFilmLengthValue, new CC().cell(1, 0));
            panel2.add(hSpacer1, new CC().cell(2, 0).growX());

            //---- label7 ----
            label7.setText("Maximall\u00e4nge:"); //NON-NLS
            panel2.add(label7, new CC().cell(3, 0));

            //---- lblMaxFilmLengthValue ----
            lblMaxFilmLengthValue.setText("100"); //NON-NLS
            panel2.add(lblMaxFilmLengthValue, new CC().cell(4, 0));
            panel2.add(filmLengthSlider, new CC().cell(0, 1, 5, 1).growX());
        }
        contentPane.add(panel2, new CC().cell(0, 20, 3, 1).growX());
        contentPane.add(separator7, new CC().cell(0, 21, 3, 1).growX());

        //---- label1 ----
        label1.setText("Zeitraum:"); //NON-NLS
        contentPane.add(label1, new CC().cell(0, 22));
        contentPane.add(spZeitraum, new CC().cell(1, 22));

        //---- label2 ----
        label2.setText("Tage"); //NON-NLS
        contentPane.add(label2, new CC().cell(2, 22));
        pack();
        setLocationRelativeTo(getOwner());
        // JFormDesigner - End of component initialization  //GEN-END:initComponents  @formatter:on
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables  @formatter:off
    // Generated using JFormDesigner non-commercial license
    private FilterSelectionComboBox cboxFilterSelection;
    private JButton btnRenameFilter;
    private JButton btnAddNewFilter;
    private JButton btnDeleteCurrentFilter;
    private JButton btnResetCurrentFilter;
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
    private JComboBox<String> jcbThema;
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
