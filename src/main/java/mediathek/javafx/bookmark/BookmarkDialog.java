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

package mediathek.javafx.bookmark;

import ca.odell.glazedlists.GlazedLists;
import ca.odell.glazedlists.ObservableElementList;
import ca.odell.glazedlists.SortedList;
import ca.odell.glazedlists.gui.TableFormat;
import ca.odell.glazedlists.impl.beans.BeanTableFormat;
import ca.odell.glazedlists.swing.DefaultEventSelectionModel;
import ca.odell.glazedlists.swing.GlazedListsSwing;
import ca.odell.glazedlists.swing.TableComparatorChooser;
import mediathek.config.Daten;
import mediathek.controller.history.SeenHistoryController;
import mediathek.gui.tabs.tab_film.FilmDescriptionPanel;
import mediathek.javafx.bookmark.renderer.*;
import mediathek.mainwindow.MediathekGui;
import mediathek.swing.IconUtils;
import mediathek.swing.table.GlazedSortKeysPersister;
import mediathek.swing.table.IconHeaderCellRenderer;
import mediathek.swing.table.TableUtils;
import mediathek.tool.ApplicationConfiguration;
import org.apache.commons.configuration2.sync.LockMode;
import org.kordamp.ikonli.fontawesome6.FontAwesomeRegular;
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid;
import org.kordamp.ikonli.materialdesign2.MaterialDesignE;
import org.kordamp.ikonli.materialdesign2.MaterialDesignN;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;
import java.util.Objects;

public class BookmarkDialog extends JDialog {
    private static final int COLUMN_SEEN = 0;
    private static final int COLUMN_SENDER = 1;
    @SuppressWarnings("unused")
    private static final int COLUMN_THEMA = 2;
    @SuppressWarnings("unused")
    private static final int COLUMN_TITLE = 3;
    private static final int COLUMN_DAUER = 4;
    private static final int COLUMN_SENDEDATUM = 5;
    @SuppressWarnings("unused")
    private static final int COLUMN_AVAILABLE_UNTIL = 6;
    private static final int COLUMN_NORMAL_QUALITY_URL = 7;
    private static final int COLUMN_NOTIZ = 8;
    private static final int COLUMN_HASHCODE = 9;
    private static final int COLUM_BOOKMARK_ADDED_AT = 10;
    private static final String CONFIG_PREFIX = "ui.bookmark-dialog";
    private static final String BOOKMARK_POS_X = CONFIG_PREFIX + ".x";
    private static final String BOOKMARK_POS_Y = CONFIG_PREFIX + ".y";
    private static final String BOOKMARK_WIDTH = CONFIG_PREFIX + ".width";
    private static final String BOOKMARK_HEIGHT = CONFIG_PREFIX + ".height";
    private final FilmDescriptionPanel filmDescriptionPanel = new FilmDescriptionPanel();
    private final JTextArea noteArea = new JTextArea();
    private final JTable table = new JTable();
    private final AddNoteAction addNoteAction = new AddNoteAction();
    private final MarkSeenAction markSeenAction = new MarkSeenAction();
    private final MarkUnseenAction markUnseenAction = new MarkUnseenAction();
    private final RemoveNoteAction removeNoteAction = new RemoveNoteAction();
    private final DeleteBookmarkAction deleteBookmarkAction = new DeleteBookmarkAction();
    private DefaultEventSelectionModel<BookmarkData> selectionModel;
    private BookmarkTableColumnSettingsManager<BookmarkData> tableColumnSettingsManager;
    private GlazedSortKeysPersister<BookmarkData> sortPersister;

    public BookmarkDialog(Frame owner) {
        super(owner);
        setTitle("Merkliste verwalten");
        setModal(false);
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);

        setupToolBar();

        JScrollPane scrollPane = new JScrollPane(table);
        getContentPane().add(scrollPane, BorderLayout.CENTER);

        setupNoteArea();
        getContentPane().add(createTabbedPane(), BorderLayout.SOUTH);

        setupTable();
        TableUtils.fitColumnHeaders(table, 5);

        installListener();

        restoreBounds();
    }

    private void installTableContextMenu() {
        table.addMouseListener(new MouseAdapter() {
            private void showPopup(MouseEvent e) {
                if (!e.isPopupTrigger())
                    return;

                // ensure right‐clicked row is selected
                int row = table.rowAtPoint(e.getPoint());
                if (row != -1 && !table.isRowSelected(row)) {
                    table.getSelectionModel().setSelectionInterval(row, row);
                }

                var selectedItems = selectionModel.getSelected();
                if (selectedItems.isEmpty()) {
                    return;
                }

                JPopupMenu popup = new JPopupMenu();
                JMenuItem menuItem = new NoIconMenuItem(addNoteAction);
                popup.add(menuItem);
                //if (numSelectedItems >= 1) {
                menuItem = new NoIconMenuItem(removeNoteAction);
                popup.add(menuItem);
                popup.addSeparator();
                menuItem = new NoIconMenuItem(markSeenAction);
                popup.add(menuItem);
                menuItem = new NoIconMenuItem(markUnseenAction);
                popup.add(menuItem);
                popup.addSeparator();
                menuItem = new NoIconMenuItem(deleteBookmarkAction);
                popup.add(menuItem);
                //}

                popup.show(e.getComponent(), e.getX(), e.getY());
            }

            @Override
            public void mousePressed(MouseEvent e) {
                showPopup(e);
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                showPopup(e);
            }
        });
    }

    private void installListener() {
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                saveBounds();
                tableColumnSettingsManager.save();
                dispose();
            }

            @Override
            public void windowClosed(WindowEvent e) {
                saveBounds();
            }
        });
    }

    private void saveBounds() {
        var config = ApplicationConfiguration.getConfiguration();
        config.lock(LockMode.WRITE);
        try {
            Rectangle b = getBounds();
            config.setProperty(BOOKMARK_POS_X, b.x);
            config.setProperty(BOOKMARK_POS_Y, b.y);
            config.setProperty(BOOKMARK_WIDTH, b.width);
            config.setProperty(BOOKMARK_HEIGHT, b.height);
        }
        finally {
            config.unlock(LockMode.WRITE);
        }
    }

    private void restoreBounds() {
        var config = ApplicationConfiguration.getConfiguration();
        config.lock(LockMode.READ);
        try {
            int x = config.getInt(BOOKMARK_POS_X, 100);
            int y = config.getInt(BOOKMARK_POS_Y, 100);
            int w = config.getInt(BOOKMARK_WIDTH, 800);
            int h = config.getInt(BOOKMARK_HEIGHT, 600);
            setBounds(x, y, w, h);
        }
        finally {
            config.unlock(LockMode.READ);
        }
    }

    private void setupNoteArea() {
        noteArea.setLineWrap(true);
        noteArea.setWrapStyleWord(true);
        noteArea.setEditable(false);
    }

    private JTabbedPane createTabbedPane() {
        JTabbedPane tabbedPane = new JTabbedPane(JTabbedPane.TOP);
        tabbedPane.addTab("Beschreibung", filmDescriptionPanel);
        JPanel notePanel = new JPanel(new BorderLayout());
        notePanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        notePanel.add(noteArea, BorderLayout.CENTER);
        tabbedPane.addTab("Notizen", notePanel);
        return tabbedPane;
    }

    private TableFormat<BookmarkData> getTableFormat() {
        var propertyNames = new String[]{"seen", "sender", "thema", "title", "dauer", "sendedatum", "AvailableUntil", "NormalQualityUrl", "note", "filmHashCode", "BookmarkAdded"};
        var columnLabels = new String[]{"Gesehen", "Sender", "Thema", "Titel", "Dauer", "Sendedatum", "Verfügbar bis", "URL", "Notiz", "Hash Code", "hinzugefügt am"};
        return new BeanTableFormat<>(BookmarkData.class, propertyNames, columnLabels);
    }

    private void disableSortableColumns(TableComparatorChooser<BookmarkData> comparatorChooser) {
        comparatorChooser.getComparatorsForColumn(COLUMN_NORMAL_QUALITY_URL).clear();
        comparatorChooser.getComparatorsForColumn(COLUMN_HASHCODE).clear();
        comparatorChooser.getComparatorsForColumn(COLUMN_NOTIZ).clear();
        comparatorChooser.getComparatorsForColumn(COLUMN_SEEN).clear();
    }

    private void setupTable() {
        ObservableElementList.Connector<BookmarkData> personConnector = GlazedLists.beanConnector(BookmarkData.class);
        var sourceEventList = Daten.getInstance().getListeBookmarkList().getEventList();
        sourceEventList.getReadWriteLock().readLock().lock();

        ObservableElementList<BookmarkData> observedBookmarks;
        SortedList<BookmarkData> sortedList;
        try {
            observedBookmarks = new ObservableElementList<>(Daten.getInstance().getListeBookmarkList().getEventList(), personConnector);
            sortedList = new SortedList<>(observedBookmarks, new BookmarkAddedAtComparator());
        }
        finally {
            sourceEventList.getReadWriteLock().readLock().unlock();
        }
        var model = GlazedListsSwing.eventTableModelWithThreadProxyList(sortedList, getTableFormat());
        selectionModel = new DefaultEventSelectionModel<>(sortedList);
        selectionModel.addListSelectionListener(l -> {
            if (!l.getValueIsAdjusting()) {
                var numSelections = selectionModel.getSelected().size();
                addNoteAction.setEnabled(numSelections == 1);
                updateInfoTabs();
            }
        });

        table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        table.setModel(model);
        table.setSelectionModel(selectionModel);

        var comparatorChooser = TableComparatorChooser.install(table, sortedList, TableComparatorChooser.MULTIPLE_COLUMN_MOUSE);
        //disable sort for url and hashcode
        disableSortableColumns(comparatorChooser);

        sortPersister = new GlazedSortKeysPersister<>(CONFIG_PREFIX, comparatorChooser);
        sortPersister.restoreSortState();
        comparatorChooser.addSortActionListener(_ -> sortPersister.saveSortState());
        setupCellRenderers();

        tableColumnSettingsManager = new BookmarkTableColumnSettingsManager<>(table, CONFIG_PREFIX, comparatorChooser);
        tableColumnSettingsManager.load();
        tableColumnSettingsManager.installContextMenu();

        installTableContextMenu();
    }

    private void setupCellRenderers() {
        var columnModel = table.getColumnModel();
        //seen column
        var colSeen = columnModel.getColumn(COLUMN_SEEN);
        colSeen.setCellRenderer(new SeenCellRenderer());
        colSeen.setHeaderRenderer(new IconHeaderCellRenderer(IconUtils.of(MaterialDesignE.EYE), "Gesehen"));
        //sender column
        columnModel.getColumn(COLUMN_SENDER).setCellRenderer(new CenteredCellRenderer());
        // dauer column
        columnModel.getColumn(COLUMN_DAUER).setCellRenderer(new FilmLengthCellRenderer());
        //sendedatum column
        columnModel.getColumn(COLUMN_SENDEDATUM).setCellRenderer(new CenteredCellRenderer());
        var colNote = columnModel.getColumn(COLUMN_NOTIZ);
        colNote.setCellRenderer(new NoteCellRenderer());
        colNote.setHeaderRenderer(new IconHeaderCellRenderer(IconUtils.of(MaterialDesignN.NOTE), "Notiz vorhanden"));
        //hinzugefügt am Column
        columnModel.getColumn(COLUM_BOOKMARK_ADDED_AT).setCellRenderer(new AddedAtCellRenderer());
    }

    private void setupToolBar() {
        JToolBar toolBar = new JToolBar();
        toolBar.setFloatable(false);

        JButton addNoteButton = new IconOnlyButton(addNoteAction);
        toolBar.add(addNoteButton);
        JButton removeNoteButton = new IconOnlyButton(removeNoteAction);
        toolBar.add(removeNoteButton);
        toolBar.addSeparator();

        JButton markSeenButton = new IconOnlyButton(markSeenAction);
        toolBar.add(markSeenButton);
        JButton markUnseenButton = new IconOnlyButton(markUnseenAction);
        toolBar.add(markUnseenButton);

        toolBar.addSeparator();
        JButton deleteEntryButton = new IconOnlyButton(deleteBookmarkAction);
        toolBar.add(deleteEntryButton);

        getContentPane().add(toolBar, BorderLayout.NORTH);
    }

    private void updateInfoTabs() {
        var selectedBookmarks = selectionModel.getSelected();
        if (selectedBookmarks.size() == 1) {
            var bookmark = selectedBookmarks.getFirst();
            bookmark.getDatenFilmOptional()
                    .ifPresentOrElse(film -> {
                                filmDescriptionPanel.setCurrentFilm(film);
                                noteArea.setText(Objects.requireNonNullElse(bookmark.getNote(), ""));
                            },
                            () -> {
                                filmDescriptionPanel.setCurrentFilm(null);
                                noteArea.setText("");
                            });
        }
        else {
            filmDescriptionPanel.setCurrentFilm(null);
            noteArea.setText("");
        }
    }

    class MarkSeenAction extends AbstractAction {

        public MarkSeenAction() {
            putValue(Action.NAME, "Als gesehen markieren");
            putValue(Action.SHORT_DESCRIPTION, "Als gesehen markieren");
            putValue(Action.SMALL_ICON, IconUtils.toolbarIcon(MaterialDesignE.EYE));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            var selFilms = selectionModel.getSelected().stream().filter(bookmark -> bookmark.getDatenFilmOptional().isPresent())
                    .map(bookmark -> bookmark.getDatenFilmOptional().get())
                    .toList();
            try (var controller = new SeenHistoryController()) {
                controller.markSeen(selFilms);
            }
        }
    }

    class MarkUnseenAction extends AbstractAction {

        public MarkUnseenAction() {
            putValue(Action.NAME, "Als ungesehen markieren");
            putValue(Action.SHORT_DESCRIPTION, "Als ungesehen markieren");
            putValue(Action.SMALL_ICON, IconUtils.toolbarIcon(MaterialDesignE.EYE_OFF));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            var selFilms = selectionModel.getSelected().stream().filter(bookmark -> bookmark.getDatenFilmOptional().isPresent())
                    .map(bookmark -> bookmark.getDatenFilmOptional().get())
                    .toList();
            try (var controller = new SeenHistoryController()) {
                controller.markUnseen(selFilms);
            }
        }
    }

    class AddNoteAction extends AbstractAction {
        public AddNoteAction() {
            putValue(Action.NAME, "Notiz hinzufügen...");
            putValue(Action.SHORT_DESCRIPTION, "Notiz hinzufügen");
            putValue(Action.SMALL_ICON, IconUtils.toolbarIcon(FontAwesomeRegular.EDIT));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            var bm = selectionModel.getSelected().getFirst();
            BookmarkEditNoteDialog dialog = new BookmarkEditNoteDialog(BookmarkDialog.this, bm.getNote());
            dialog.setVisible(true);
            if (dialog.isOkPressed()) {
                var notizText = dialog.getNotiz();
                if (notizText.isBlank())
                    notizText = null;
                bm.setNote(notizText);
                Daten.getInstance().getListeBookmarkList().saveToFile();
                updateInfoTabs();
            }
        }
    }

    class RemoveNoteAction extends AbstractAction {
        public RemoveNoteAction() {
            putValue(Action.NAME, "Notiz löschen...");
            putValue(Action.SHORT_DESCRIPTION, "Notiz löschen");
            putValue(Action.SMALL_ICON, IconUtils.toolbarIcon(FontAwesomeSolid.ERASER));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            if (!selectionModel.isSelectionEmpty()) {
                selectionModel.getSelected().forEach(bookmark -> bookmark.setNote(null));
                Daten.getInstance().getListeBookmarkList().saveToFile();
                updateInfoTabs();
            }
        }
    }

    class DeleteBookmarkAction extends AbstractAction {
        public DeleteBookmarkAction() {
            putValue(Action.NAME, "Aus Merkliste löschen...");
            putValue(Action.SHORT_DESCRIPTION, "Aus Merkliste löschen");
            putValue(Action.SMALL_ICON, IconUtils.toolbarIcon(FontAwesomeRegular.TRASH_ALT));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            var bookmarkList = Daten.getInstance().getListeBookmarkList();
            var list = new ArrayList<>(selectionModel.getSelected());
            for (var bookmark : list) {
                bookmarkList.removeBookmark(bookmark);
            }

            bookmarkList.saveToFile();
            SwingUtilities.invokeLater(() -> MediathekGui.ui().tabFilme.repaint());
        }
    }

}
