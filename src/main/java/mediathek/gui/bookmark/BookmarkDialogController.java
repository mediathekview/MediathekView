package mediathek.gui.bookmark;

import mediathek.config.Daten;
import mediathek.config.MVColor;
import mediathek.config.MVConfig;
import mediathek.config.StandardLocations;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;
import mediathek.daten.DatenPset;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.gui.dialog.DialogAddMoreDownload;
import mediathek.gui.messages.DownloadListChangedEvent;
import mediathek.gui.tabs.tab_film.GuiFilme;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.MessageBus;
import mediathek.tool.TimerPool;
import mediathek.tool.models.TModelBookmark;
import mediathek.tool.models.TModelFilm;
import org.apache.commons.configuration2.Configuration;
import org.apache.commons.configuration2.sync.LockMode;
import org.apache.logging.log4j.LogManager;
import org.jdesktop.swingx.JXHyperlink;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import java.awt.*;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

public class BookmarkDialogController {

    private static final String[] BTNFILTER_TOOLTIPTEXT = {
            "Nur ungesehene Filme anzeigen",
            "Nur gesehene Filme anzeigen",
            "Alle Filme anzeigen"
    };

    private static final String[] LBLFILTER_MESSAGETEXT = {"", "Ungesehene Filme", "Gesehene Filme"};

    private static final boolean[] LBLSEEN_DISABLE = {false, true, false};

    private final BookmarkDataListSwing listeBookmarkList;

    private final SeenHistoryController history = new SeenHistoryController();

    private BookmarkDialogSwing bookmarkDialogSwing;

    private Color colorExpired;

    private Color colorSeen;

    private Color colorSelected;

    private JMenuItem playItem;

    private JMenuItem loadItem;

    private JMenuItem deleteItem;

    private JMenuItem viewItem;

    private JMenuItem webItem;

    private JMenuItem copyItem;

    private JMenuItem editItem;

    private JPopupMenu cellContextMenu;

    private GuiFilme infotab;

    private double divPosition;

    private boolean listUpdated;

    private ScheduledFuture<?> saveBookmarkTask;

    private int filterState;

    private JButton btnSaveList;

    private JButton btnDeleteEntry;

    private JButton btnMarkViewed;

    private JToggleButton btnShowDetails;

    private JButton btnFilter;

    private JButton btnEditNote;

    private JToolBar toolBar;

    private JTable tbBookmarks;

    private JLabel lblCount;

    private JLabel lblSeen;

    private JLabel lblMessage;

    private JLabel lblFilter;

    private JTextArea taDescription;

    private JSplitPane spSplitPane;

    private JXHyperlink hyperLink;

    public BookmarkDialogController() {
        listeBookmarkList = Daten.getInstance().getListeBookmarkListSwing();
        listUpdated = false;
        bookmarkDialogSwing = new BookmarkDialogSwing(null, true);
    }

    public void initialize() {
        createUI();
        setupTable();
        setupListeners();
        restoreTableState();

        filterState = -1;
        btnFilterAction(null);
        btnShowDetails.setSelected(ApplicationConfiguration.getConfiguration().getBoolean(
                ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".details", true));
        divPosition = ApplicationConfiguration.getConfiguration().getDouble(
                ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".divider", spSplitPane.getDividerLocation());
        btnShowDetailsAction(null);
        updateDescriptionArea();
restoreWindowState();
        bookmarkDialogSwing.setVisible(true);
    }

    private void createUI() {
        toolBar = bookmarkDialogSwing.toolBar;
        btnSaveList = bookmarkDialogSwing.btnSaveList;
        btnDeleteEntry = bookmarkDialogSwing.btnDeleteEntry;
        btnMarkViewed = bookmarkDialogSwing.btnMarkViewed;
        btnShowDetails = bookmarkDialogSwing.btnShowDetails;
        btnFilter = bookmarkDialogSwing.btnFilter;
        btnEditNote = bookmarkDialogSwing.btnEditNote;


        lblCount = bookmarkDialogSwing.lblCount;
        lblSeen = bookmarkDialogSwing.lblSeen;
        lblMessage = bookmarkDialogSwing.lblMessage;
        lblFilter = bookmarkDialogSwing.lblFilter;
        spSplitPane = bookmarkDialogSwing.spSplitPane;

        tbBookmarks = bookmarkDialogSwing.tbBookmarks;
        bookmarkDialogSwing.tbBookmarks.setModel(new TModelBookmark());
        taDescription = bookmarkDialogSwing.taDescription;
        taDescription.setEditable(false);
        taDescription.setLineWrap(true);
        taDescription.setWrapStyleWord(true);
        hyperLink = bookmarkDialogSwing.hyperLink;
    }

    private void setupTable() {
        // Create table model
        DefaultTableModel model = new DefaultTableModel() {
            @Override
            public boolean isCellEditable(int row, int column) {
                return false;
            }
        };

        model.addColumn("Sender");
        model.addColumn("Thema");
        model.addColumn("Titel");
        model.addColumn("Dauer");
        model.addColumn("Sendung");
        model.addColumn("URL");
        model.addColumn("Notiz");
        model.addColumn("Ablauf");

        tbBookmarks.setModel(model);
        tbBookmarks.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);

        // Custom renderer for styling
        tbBookmarks.setDefaultRenderer(Object.class, new DefaultTableCellRenderer() {
            @Override
            public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
                                                           boolean hasFocus, int row, int column) {
                Component c = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
                BookmarkDataSwing data = listeBookmarkList.getList().get(row);

                if (isSelected) {
                    c.setBackground(colorSelected);
                    c.setForeground(Color.WHITE);
                } else if (data.getSeen()) {
                    c.setBackground(colorSeen);
                    c.setForeground(Color.BLACK);
                } else {
                    c.setBackground(table.getBackground());
                    c.setForeground(data.isNotInFilmList() ? colorExpired : Color.BLACK);
                }

                return c;
            }
        });
    }

    private void setupListeners() {
        btnSaveList.addActionListener(e -> btnSaveBookMarkList(e));
        btnDeleteEntry.addActionListener(e -> btnDeleteEntry(e));
        btnMarkViewed.addActionListener(e -> btnMarkEntryAsViewed(e));
        btnShowDetails.addActionListener(e -> btnShowDetailsAction(e));
        btnFilter.addActionListener(e -> btnFilterAction(e));
        btnEditNote.addActionListener(e -> btnEditNote(e));
        hyperLink.addActionListener(e -> hyperLinkSelected(e));

        tbBookmarks.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
            @Override
            public void valueChanged(ListSelectionEvent e) {
                if (!e.getValueIsAdjusting()) {
                    boolean disable = tbBookmarks.getSelectedRow() == -1;
                    btnDeleteEntry.setEnabled(!disable);
                    btnMarkViewed.setEnabled(!disable && !onlyLifeStreamSelected());

                    boolean multipleSelected = tbBookmarks.getSelectedRows().length > 1;
                    btnEditNote.setEnabled(!disable && !multipleSelected);

                    boolean setViewed = isUnSeenSelected();
                    setSeenButtonState(setViewed, multipleSelected);

                    updateDescriptionArea();
                }
            }
        });

        // Context menu
        cellContextMenu = new JPopupMenu();
        playItem = new JMenuItem("Film abspielen");
        loadItem = new JMenuItem("Film aufzeichnen");
        viewItem = new JMenuItem();
        editItem = new JMenuItem("Anmerkungen bearbeiten");
        deleteItem = new JMenuItem();
        webItem = new JMenuItem("Film Webseite öffnen");
        copyItem = new JMenuItem("Zellinhalt in die Ablage kopieren");

        playItem.addActionListener(e -> playAction(getSelectedBookmarkData()));
        loadItem.addActionListener(e -> loadAction(getSelectedBookmarkData()));
        viewItem.addActionListener(e -> btnMarkEntryAsViewed(e));
        editItem.addActionListener(e -> btnEditNote(e));
        deleteItem.addActionListener(e -> btnDeleteEntry(e));
        webItem.addActionListener(e -> hyperLinkSelected(e));
        copyItem.addActionListener(e -> copy2Clipboard(e));

        cellContextMenu.add(playItem);
        cellContextMenu.add(loadItem);
        cellContextMenu.add(viewItem);
        cellContextMenu.addSeparator();
        cellContextMenu.add(editItem);
        cellContextMenu.add(deleteItem);
        cellContextMenu.addSeparator();
        cellContextMenu.add(webItem);
        cellContextMenu.add(copyItem);

        tbBookmarks.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount() == 2) {
                    btnEditNote(null);
                }
            }

            @Override
            public void mousePressed(MouseEvent e) {
                if (SwingUtilities.isRightMouseButton(e)) {
                    int row = tbBookmarks.rowAtPoint(e.getPoint());
                    if (row != -1 && !tbBookmarks.isRowSelected(row)) {
                        tbBookmarks.setRowSelectionInterval(row, row);
                    }

                    if (tbBookmarks.getSelectedRow() != -1) {
                        boolean multipleSelected = tbBookmarks.getSelectedRows().length > 1;
                        playItem.setEnabled(!multipleSelected);
                        editItem.setEnabled(!multipleSelected);
                        loadItem.setEnabled(!multipleSelected);
                        webItem.setEnabled(!multipleSelected && getSelectedBookmarkData().getWebUrl() != null);
                        copyItem.setEnabled(!multipleSelected);

                        deleteItem.setText(String.format("Film%s aus der Merkliste entfernen",
                                (multipleSelected ? "e" : "")));

                        cellContextMenu.show(tbBookmarks, e.getX(), e.getY());
                    }
                }
            }
        });
    }

    private BookmarkDataSwing getSelectedBookmarkData() {
        int row = tbBookmarks.getSelectedRow();
        return row != -1 ? listeBookmarkList.getList().get(row) : null;
    }

    private void btnMarkEntryAsViewed(ActionEvent e) {
        int[] selectedRows = tbBookmarks.getSelectedRows();
        if (selectedRows.length > 0) {
            boolean hasUnSeen = isUnSeenSelected();
            List<BookmarkDataSwing> bookmarks = new ArrayList<>();
            List<DatenFilm> filmlist = new ArrayList<>();

            for (int row : selectedRows) {
                BookmarkDataSwing data = listeBookmarkList.getList().get(row);
                data.setSeen(hasUnSeen);
                bookmarks.add(data);

                DatenFilm film = data.getDatenFilm();
                if (film != null) {
                    filmlist.add(film);
                }
            }

            if (hasUnSeen) {
                history.markSeen(filmlist);
            } else {
                history.markUnseen(filmlist);
            }

            setSeenButtonState(hasUnSeen, selectedRows.length > 1);
            tbBookmarks.repaint();
        }
    }

    private void btnSaveBookMarkList(ActionEvent e) {
        cancelBookmarkSave();
        saveBookMarkList();
    }

    private void btnDeleteEntry(ActionEvent e) {
        int[] selectedRows = tbBookmarks.getSelectedRows();
        if (selectedRows.length > 0) {
            List<BookmarkDataSwing> toRemove = new ArrayList<>();
            for (int row : selectedRows) {
                toRemove.add(listeBookmarkList.getList().get(row));
            }

            listeBookmarkList.deleteEntries(toRemove);
            updateDisplay();
            tbBookmarks.clearSelection();
            if (infotab != null) {
                infotab.repaint();
            }
        }
    }

    private void btnEditNote(ActionEvent e) {

    }

    private void hyperLinkSelected(ActionEvent e) {
        BookmarkDataSwing selected = getSelectedBookmarkData();
        if (selected != null && selected.getWebUrl() != null) {
            try {
                UrlHyperlinkAction.openURL(null, selected.getWebUrl());
            } catch (URISyntaxException ex) {
                LogManager.getLogger(BookmarkDialogController.class).error("Hyperlink Syntax exception", ex);
            }
        }
    }

    private void copy2Clipboard(ActionEvent e) {
        int row = tbBookmarks.getSelectedRow();
        int col = tbBookmarks.getSelectedColumn();
        if (row != -1 && col != -1) {
            String data = tbBookmarks.getValueAt(row, col).toString();
            Toolkit.getDefaultToolkit().getSystemClipboard().setContents(new StringSelection(data), null);
        }
    }

    private void restoreTableState() {
        Configuration config = ApplicationConfiguration.getConfiguration();
        try {
            config.lock(LockMode.READ);
            String colbase = ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".columns.";
            int entries = config.getInt(colbase + "no", 0);

            if (entries > 0) {
                TableColumnModel columnModel = tbBookmarks.getColumnModel();
                for (int i = 1; i <= entries; i++) {
                    String colref = colbase + "col" + i;
                    String colid = config.getString(colref + ".id");
                    int colsize = config.getInt(colref + ".size", 100);
                    boolean colvisible = config.getBoolean(colref + ".visible", true);

                    for (int j = 0; j < columnModel.getColumnCount(); j++) {
                        TableColumn column = columnModel.getColumn(j);
                        if (column.getIdentifier().equals(colid)) {
                            column.setPreferredWidth(colsize);
                            column.setMinWidth(colvisible ? 30 : 0);
                            column.setMaxWidth(colvisible ? Integer.MAX_VALUE : 0);
                            break;
                        }
                    }
                }
            }
        } finally {
            config.unlock(LockMode.READ);
        }
    }

    private void btnShowDetailsAction(ActionEvent e) {
        if (btnShowDetails.isSelected()) {
            spSplitPane.setDividerLocation(divPosition);
        } else {
            divPosition = spSplitPane.getDividerLocation();
            spSplitPane.setDividerLocation(1.0);
        }
    }

    private void btnFilterAction(ActionEvent e) {
        if (++filterState > 2) {
            filterState = 0;
        }

        // In Swing we need to manually filter the data
        updateDisplay();

        btnFilter.setToolTipText(BTNFILTER_TOOLTIPTEXT[filterState]);
        lblFilter.setText(LBLFILTER_MESSAGETEXT[filterState]);
        lblSeen.setEnabled(!LBLSEEN_DISABLE[filterState]);
    }

    public void show() {
        if (!bookmarkDialogSwing.isVisible()) {
            restoreWindowState();
            initSettings();
            refresh();
            bookmarkDialogSwing.setVisible(true);
        } else {
            bookmarkDialogSwing.toFront();
            bookmarkDialogSwing.requestFocus();
        }
    }

    public void setPartner(GuiFilme partner) {
        this.infotab = partner;
    }

    private void refresh() {
        if (bookmarkDialogSwing.isVisible()) {
            updateTableData();
            updateDisplay();
        }
    }

    private void updateTableData() {
        DefaultTableModel model = (DefaultTableModel) tbBookmarks.getModel();
        model.setRowCount(0);

        for (BookmarkDataSwing data : listeBookmarkList.getList()) {
            if (filterState == 1 && data.getSeen()) continue;
            if (filterState == 2 && !data.getSeen()) continue;

            model.addRow(new Object[]{
                    data.getSender(),
                    data.getThema(),
                    data.getTitel(),
                    data.getDauer(),
                    data.getSendDate(),
                    data.getUrl(),
                    data.getNote(),
                    data.getExpiry()
            });
        }
    }

    private void updateDisplay() {
        int filteredCount = 0;
        int totalCount = listeBookmarkList.getNbOfEntries();
        int seenCount = listeBookmarkList.getSeenNbOfEntries();

        for (BookmarkDataSwing data : listeBookmarkList.getList()) {
            if (filterState == 1 && data.getSeen()) continue;
            if (filterState == 2 && !data.getSeen()) continue;
            filteredCount++;
        }

        lblCount.setText(String.format("Einträge: %d / %d", filteredCount, totalCount));
        lblSeen.setText(String.format("Gesehen: %d", seenCount));
        btnSaveList.setEnabled(listUpdated);

        if (listUpdated) {
            cancelBookmarkSave();
            saveBookmarkTask = TimerPool.getTimerPool().schedule(() -> {
                saveBookMarkList();
                saveBookmarkTask = null;
            }, 30, TimeUnit.SECONDS);
        }

        lblMessage.setText("");
    }

    private void saveBookMarkList() {
        if (listUpdated) {
            listeBookmarkList.saveToFile(StandardLocations.getBookmarkFilePath());
            btnSaveList.setEnabled(false);
            lblMessage.setText("Merkliste ist gesichert");
        }
        listUpdated = false;
    }

    private void playAction(BookmarkDataSwing data) {
        Daten.getInstance().getStarterClass().urlMitProgrammStarten(
                Daten.listePset.getPsetAbspielen(), data.getDataAsDatenFilm(), "");
        tbBookmarks.clearSelection();
        tbBookmarks.setRowSelectionInterval(
                listeBookmarkList.getList().indexOf(data),
                listeBookmarkList.getList().indexOf(data));
    }

    private void loadAction(@NotNull BookmarkDataSwing data) {
        Optional<DatenFilm> datenFilm = Optional.ofNullable(data.getDatenFilm());
        final var daten = Daten.getInstance();

        refresh();
        datenFilm.ifPresent(film -> {
            DatenDownload previouslyCreatedDownload = daten.getListeDownloads().getDownloadUrlFilm(film.getUrlNormalQuality());
            if (previouslyCreatedDownload == null) {
                createDownload(film);
            } else {
                int result = JOptionPane.showConfirmDialog(bookmarkDialogSwing,
                        "Ein Download für den Film existiert bereits.\nNochmal anlegen?",
                        "Merkliste", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);

                if (result == JOptionPane.YES_OPTION) {
                    createDownload(film);
                }
            }
        });
    }

    private void createDownload(DatenFilm film) {
        final var daten = Daten.getInstance();
        bookmarkDialogSwing.setVisible(false);

        DatenPset pSet = Daten.listePset.getListeSpeichern().get(0);
        final var ui = MediathekGui.ui();
        DialogAddMoreDownload damd = new DialogAddMoreDownload(ui, pSet);
        damd.setLocationRelativeTo(ui);
        damd.setVisible(true);

        String pfad = damd.getPath();
        boolean info = damd.info;
        boolean subtitle = damd.subtitle;
        if (!damd.cancel) {
            DatenDownload datenDownload = new DatenDownload(pSet, film,
                    DatenDownload.QUELLE_DOWNLOAD, null, "", pfad, "");
            datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI] = Boolean.toString(info);
            datenDownload.arr[DatenDownload.DOWNLOAD_SUBTITLE] = Boolean.toString(subtitle);

            daten.getListeDownloads().addMitNummer(datenDownload);
            MessageBus.getMessageBus().publishAsync(new DownloadListChangedEvent());
            if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN))) {
                datenDownload.startDownload();
            }
        }

        bookmarkDialogSwing.setVisible(true);
        bookmarkDialogSwing.toFront();
    }

    public void saveSettings() {
        Configuration config = ApplicationConfiguration.getConfiguration();
        try {
            config.lock(LockMode.WRITE);
            config.setProperty(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".width", bookmarkDialogSwing.getWidth());
            config.setProperty(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".height", bookmarkDialogSwing.getHeight());
            config.setProperty(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".location.x", bookmarkDialogSwing.getX());
            config.setProperty(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".location.y", bookmarkDialogSwing.getY());

            String colbase = ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".columns";
            config.setProperty(colbase + ".no", tbBookmarks.getColumnCount());

            for (int i = 0; i < tbBookmarks.getColumnCount(); i++) {
                TableColumn column = tbBookmarks.getColumnModel().getColumn(i);
                String colref = colbase + ".col" + (i + 1);
                config.setProperty(colref + ".id", column.getIdentifier());
                config.setProperty(colref + ".size", column.getWidth());
                config.setProperty(colref + ".visible", column.getWidth() > 0);
            }

            config.setProperty(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".details", btnShowDetails.isSelected());
            config.setProperty(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".divider",
                    btnShowDetails.isSelected() ? spSplitPane.getDividerLocation() : divPosition);
        } catch (Exception e) {
            LogManager.getLogger(ApplicationConfiguration.class).error("Save Config exception: ", e);
        } finally {
            config.unlock(LockMode.WRITE);
        }
    }

    private void restoreWindowState() {
        Configuration config = ApplicationConfiguration.getConfiguration();
        try {
            config.lock(LockMode.READ);
            bookmarkDialogSwing.setSize(
                    config.getInt(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".width", 800),
                    config.getInt(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".height", 600));
            bookmarkDialogSwing.setLocation(
                    config.getInt(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".location.x", 0),
                    config.getInt(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".location.y", 0));
        } finally {
            config.unlock(LockMode.READ);
        }
    }

    private void updateDescriptionArea() {
        int selectedRow = tbBookmarks.getSelectedRow();
        if (selectedRow != -1 && tbBookmarks.getSelectedRowCount() == 1) {
            BookmarkDataSwing data = listeBookmarkList.getList().get(selectedRow);
            taDescription.setText(data.getExtendedDescription());

            String url = data.getWebUrl();
            hyperLink.setVisible(url != null && !url.isEmpty());
            if (url != null) {
                hyperLink.setToolTipText(url);
            }
        } else {
            taDescription.setText("");
            hyperLink.setVisible(false);
        }
    }

    private void setSeenButtonState(boolean setViewed, boolean multipleSelected) {
        String text = String.format("Film%s als %sgesehen markieren",
                (multipleSelected ? "e" : ""), (setViewed ? "" : "un"));
        btnMarkViewed.setText(text);
        viewItem.setText(text);
    }

    private boolean isUnSeenSelected() {
        for (int row : tbBookmarks.getSelectedRows()) {
            BookmarkDataSwing data = listeBookmarkList.getList().get(row);
            if (!data.getSeen() && !data.isLiveStream()) {
                return true;
            }
        }
        return false;
    }

    private boolean onlyLifeStreamSelected() {
        for (int row : tbBookmarks.getSelectedRows()) {
            BookmarkDataSwing data = listeBookmarkList.getList().get(row);
            if (!data.isLiveStream()) {
                return false;
            }
        }
        return true;
    }

    private void initSettings() {
        colorSeen = MVColor.FILM_HISTORY.color;
        colorSelected = MVColor.getNewColor();
        colorExpired = MVColor.DOWNLOAD_FEHLER.color;
    }

    private void cancelBookmarkSave() {
        if (saveBookmarkTask != null) {
            saveBookmarkTask.cancel(false);
            saveBookmarkTask = null;
        }
    }
}