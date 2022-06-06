package mediathek.tool.table;

import com.sun.jna.platform.win32.VersionHelpers;
import mediathek.config.MVConfig;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.RowSorter.SortKey;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableColumnModel;
import java.awt.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public abstract class MVTable extends JTable {
    private static final String FELDTRENNER = "|";
    private static final String SORT_ASCENDING = "ASCENDING";
    private static final String SORT_DESCENDING = "DESCENDING";
    private static final Logger logger = LogManager.getLogger();
    protected final int[] breite;
    protected final int[] reihe;
    public boolean useSmallSenderIcons;
    protected final int maxSpalten;
    protected int[] selRows;
    protected int selRow = -1;
    protected final boolean[] spaltenAnzeigen;
    protected MVConfig.Configs nrDatenSystem;
    protected MVConfig.Configs iconAnzeigenStr;
    protected MVConfig.Configs iconKleinStr;
    protected List<? extends RowSorter.SortKey> listeSortKeys;
    /**
     * This is the UI provided default font used for calculating the size area
     */
    private Font defaultFont = UIManager.getDefaults().getFont("Table.font");
    private boolean showSenderIcon;
    private boolean lineBreak = true;

    public MVTable(int maxColumns, boolean @NotNull [] visibleColumStore) {
        maxSpalten = maxColumns;
        spaltenAnzeigen = visibleColumStore;
        // make all columns visible by default in column store
        Arrays.fill(spaltenAnzeigen, true);

        setAutoCreateRowSorter(true);
        setAutoResizeMode(JTable.AUTO_RESIZE_OFF);

        setupTableType();

        getTableHeader().addMouseListener(new WidthAdjuster(this));


        breite = new int[maxSpalten];
        Arrays.fill(breite,-1);

        reihe = new int[maxSpalten];
        Arrays.fill(reihe, -1);

        if (iconAnzeigenStr != null) {
            showSenderIcon = Boolean.parseBoolean(MVConfig.get(iconAnzeigenStr));
        }
        if (iconKleinStr != null) {
            useSmallSenderIcons = Boolean.parseBoolean(MVConfig.get(iconKleinStr));
        }

        loadDefaultFontSize();
        calculateRowHeight();

        applyWindowsSevenTableEffects();
    }

    /**
     * Show JTable grid lines on Windows 7 only.
     */
    private void applyWindowsSevenTableEffects() {
        // Windows 7 used to have grid lines therefore simulate behaviour
        if (SystemUtils.IS_OS_WINDOWS && !VersionHelpers.IsWindows8OrGreater()) {
            setShowHorizontalLines(true);
            setShowVerticalLines(true);
        }
    }

    /**
     * Load font size from settings and replace default font.
     */
    protected void loadDefaultFontSize() {
        //unused here
    }

    /**
     * Store default font size in settings
     */
    protected void saveDefaultFontSize() {
        //unused here
    }

    public Font getDefaultFont() { return defaultFont;}

    public void setDefaultFont(Font newFont) {
        defaultFont = newFont;
        saveDefaultFontSize();
    }

    private SortKey sortKeyLesen(String s, String strSortOrder) {
        SortKey sk;

        try {
            final int column = Integer.parseInt(s);
            SortOrder order = switch (strSortOrder) {
                case SORT_ASCENDING -> SortOrder.ASCENDING;
                case SORT_DESCENDING -> SortOrder.DESCENDING;
                default -> throw new IndexOutOfBoundsException("UNDEFINED SORT KEY");
            };
            sk = new SortKey(column,order);
        } catch (Exception ex) {
            return null;
        }
        return sk;
    }

    public boolean showSenderIcons() {
        return showSenderIcon;
    }

    public void setShowIcon(boolean newVal) {
        showSenderIcon = newVal;
    }

    /**
     * Setup table specific stuff here.
     */
    protected abstract void setupTableType();

    public boolean isLineBreak() { return lineBreak;}

    public void setLineBreak(boolean lb) {
        lineBreak = lb;
    }

    public void invertSelection() {
        final ListSelectionModel mdl = getSelectionModel();
        final int[] selected = getSelectedRows();
        mdl.setValueIsAdjusting(true);
        mdl.setSelectionInterval(0, getRowCount() - 1);
        for (int i : selected) {
            mdl.removeSelectionInterval(i, i);
        }
        mdl.setValueIsAdjusting(false);
    }

    /**
     * Return a fictious size of a multi-line text area.
     * @return The fictious size of a multi-line label.
     */
    private int getSizeArea() {
        final int sizeArea;
        var fm = getFontMetrics(getDefaultFont());
        final var height = fm.getHeight();

        if (lineBreak) {
            sizeArea = 4 * height;
        }
        else {
            sizeArea = height;
        }

        return sizeArea;
    }

    /**
     * Calculate the row height in a table based on icon display,etc.
     */
    public void calculateRowHeight() {
        var sizeArea = getSizeArea();
        var fm = getFontMetrics(getDefaultFont());

        var height = fm.getHeight() + 5; // add some extra spacing for the height

        // check some minimum height requirements
        if (showSenderIcon) {
            if (useSmallSenderIcons) {
                // small icons
                if (height < 18)
                    height = 20;
            }
            else {
                //large icons
                if (height < 30)
                    height = 36;
            }
        }

        setRowHeight(Math.max(height, sizeArea));
    }

    public void initTabelle() {
        // Tabelle das erste Mal initialisieren,
        // mit den gespeicherten Daten oder
        // den Standardwerten
        // erst die Breite, dann die Reihenfolge
        try {
            if (nrDatenSystem == null) {
                // wird nur für eingerichtete Tabellen gemacht
                return;
            }
            String b = "", r = "", s = "", upDown = "";
            boolean ok = false;
            if (!MVConfig.get(nrDatenSystem).isEmpty()) {
                ok = true;
                int f1, f2, f3;
                //String d = Daten.system[nrDatenSystem];
                final String strNrDatenSystem = MVConfig.get(nrDatenSystem);

                if ((f1 = strNrDatenSystem.indexOf(FELDTRENNER)) != -1) {
                    b = strNrDatenSystem.substring(0, f1);
                    if ((f2 = strNrDatenSystem.indexOf(FELDTRENNER, f1 + 1)) != -1) {
                        r = strNrDatenSystem.substring(f1 + 1, f2);
                    }
                    if ((f3 = strNrDatenSystem.indexOf(FELDTRENNER, f2 + 1)) != -1) {
                        s = strNrDatenSystem.substring(f2 + 1, f3);
                        upDown = strNrDatenSystem.substring(f3 + 1);
                    }
                }
                if (!arrLesen(b, breite)) {
                    ok = false;
                }
                if (!arrLesen(r, reihe)) {
                    ok = false;
                }

                SortKey sk = sortKeyLesen(s, upDown);
                if (sk != null) {
                    final ArrayList<SortKey> listSortKeys_ = new ArrayList<>();
                    listSortKeys_.add(sk);
                    this.getRowSorter().setSortKeys(listSortKeys_);
                }
            }
            if (ok) {
                setSpaltenEinAus(breite);
                setSpalten();
                calculateRowHeight();
            } else {
                resetTabelle();
            }
        } catch (Exception ignored) {
        }
    }

    private boolean isColumnVisible(int index) {
        return spaltenAnzeigen[index];
    }

    protected void setSpaltenEinAus(int[] nr) {
        for (int i = 0; i < spaltenAnzeigen.length; ++i) {
            spaltenAnzeigen[i] = nr[i] > 0;
        }
    }

    public void fireTableDataChanged(boolean setSpalten) {
        if (setSpalten) {
            getSelected();
        }
        var model = (AbstractTableModel)getModel();
        model.fireTableDataChanged();
        if (setSpalten) {
            setSelected();
        }
    }

    public void scrollToSelection() {
        final int rowCount = getRowCount();

        if (rowCount > 0) {
            int i = getSelectedRow();
            if (i == -1) {
                i = 0;
                getSelectionModel().setSelectionInterval(0, 0);
            }
            if (i >= rowCount) {
                i = rowCount - 1;
            }

            scrollToIndexDelegate(i);
        }
    }

    protected void scrollToIndexDelegate(int index) {
        scrollRectToVisible(getCellRect(index, 0, true));
    }

    public void getSelected() {
        // Einstellungen der Tabelle merken
        selRow = getSelectedRow();
        selRows = getSelectedRows();
    }

    protected void setSelected() {
        if (selRows != null) {
            if (selRows.length > 0) {
                selectionModel.setValueIsAdjusting(true);
                for (int selectedRow : selRows) {
                    if (selectedRow < getRowCount()) {
                        addRowSelectionInterval(selectedRow, selectedRow);
                    }
                }
                selectionModel.setValueIsAdjusting(false);
            }
        }
    }

    protected void changeColumnWidth2() {
        final TableColumnModel model = getColumnModel();
        for (int i = 0; i < breite.length && i < getColumnCount(); ++i) {
            final int colIndex = convertColumnIndexToView(i);
            var column = model.getColumn(colIndex);
            if (breite[i] == 0) {
                column.setMinWidth(0);
                column.setPreferredWidth(0);
                column.setMaxWidth(0);
            } else {
                column.setMinWidth(10);
                column.setMaxWidth(3000);
                column.setPreferredWidth(breite[i]);
            }
        }
    }

    protected void changeColumnWidth() {
        for (int i = 0; i < breite.length && i < getColumnCount(); ++i) {
            if (!isColumnVisible(i)) {
                // geänderte Ansicht der Spalten abfragen
                breite[i] = 0;
            } else if (breite[i] == 0) {
                breite[i] = 100; // damit sie auch zu sehen ist :)
            }
        }
    }

    public void spaltenEinAus() {
        getSpalten(); // die aktuelle Breite holen
        changeColumnWidth();
        changeColumnWidth2();

        validate();
    }

    public void getSpalten() {
        // Einstellungen der Tabelle merken
        getSelected();

        var columnCount = getModel().getColumnCount();

        for (int i = 0; i < reihe.length && i < columnCount; ++i) {
            reihe[i] = convertColumnIndexToModel(i);
        }

        for (int i = 0; i < breite.length && i < columnCount; ++i) {
            breite[i] = getColumnModel().getColumn(convertColumnIndexToView(i)).getWidth();
        }
        if (this.getRowSorter() != null) {
            listeSortKeys = getRowSorter().getSortKeys();
        } else {
            listeSortKeys = null;
        }
    }

    public void setSpalten() {
        // gemerkte Einstellungen der Tabelle wieder setzen
        try {
            changeColumnWidth();

            final TableColumnModel model = getColumnModel();
            changeColumnWidth2();

            for (int i = 0; i < reihe.length && i < getColumnCount(); ++i) {
                model.moveColumn(convertColumnIndexToView(reihe[i]), i);
            }

            if (listeSortKeys != null) {
                if (!listeSortKeys.isEmpty()) {
                    getRowSorter().setSortKeys(listeSortKeys);
                }
            }

            setSelected();

            validate();
        } catch (Exception ex) {
            logger.error("setSpalten", ex);
        }
    }

    /**
     * Perform common reset steps for all subclasses.
     */
    public void resetTabelle() {
        listeSortKeys = null;

        getRowSorter().setSortKeys(null); // empty sort keys
        setRowSorter(null);
        setAutoCreateRowSorter(true);
        spaltenAusschalten();
        setSpaltenEinAus(breite);
        setSpalten();
        calculateRowHeight();
    }

    protected abstract void spaltenAusschalten();

    public void tabelleNachDatenSchreiben() {
        // Tabellendaten ind die Daten.system schreiben
        // erst die Breite, dann die Reihenfolge
        String b, r, s = "", upDown = "";
        int[] reihe_ = new int[maxSpalten];
        int[] breite_ = new int[maxSpalten];
        for (int i = 0; i < reihe_.length && i < getModel().getColumnCount(); ++i) {
            reihe_[i] = convertColumnIndexToModel(i);
        }

        final TableColumnModel model = getColumnModel();
        for (int i = 0; i < breite_.length && i < getModel().getColumnCount(); ++i) {
            breite_[i] = model.getColumn(convertColumnIndexToView(i)).getWidth();
        }

        b = Integer.toString(breite_[0]);
        r = Integer.toString(reihe_[0]);
        for (int i = 1; i < breite.length; i++) {
            b = b + ',' + breite_[i];
            r = r + ',' + reihe_[i];
        }

        listeSortKeys = this.getRowSorter().getSortKeys();
        if (listeSortKeys != null) {
            if (!listeSortKeys.isEmpty()) {
                SortKey sk = listeSortKeys.get(0);
                s = String.valueOf(sk.getColumn());
                upDown = sk.getSortOrder() == SortOrder.ASCENDING ? SORT_ASCENDING : SORT_DESCENDING;
            }
        }

        MVConfig.add(nrDatenSystem, b + FELDTRENNER + r + FELDTRENNER + s + FELDTRENNER + upDown);
        if (iconAnzeigenStr != null) {
            MVConfig.add(iconAnzeigenStr, String.valueOf(showSenderIcon));
        }
        if (iconKleinStr != null) {
            MVConfig.add(iconKleinStr, String.valueOf(useSmallSenderIcons));
        }
    }

    private boolean arrLesen(String s, int[] arr) {
        String sub;
        if (maxSpalten != countNumberOfColumns(s)) {
            // dann hat sich die Anzahl der Spalten der Tabelle geändert: Versionswechsel
            return false;
        } else {
            for (int i = 0; i < maxSpalten; i++) {
                if (!s.isEmpty()) {
                    if (s.contains(",")) {
                        sub = s.substring(0, s.indexOf(','));
                        s = s.replaceFirst(sub + ',', "");
                    } else {
                        sub = s;
                        s = "";
                    }
                    try {
                        arr[i] = Integer.parseInt(sub);
                    } catch (Exception ex) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    /**
     * Count the number of saved columns within the string.
     * Counts the number of comma separated entries.
     * @param s The string to be processed.
     * @return The number of columns included.
     */
    private int countNumberOfColumns(String s) {
        // add plus one to satisfy saved data requirements...
        return StringUtils.countMatches(s,',') + 1;
    }
}
