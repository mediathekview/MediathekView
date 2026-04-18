package mediathek.tool.table;

import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.gui.messages.FontSizeChangedEvent;
import mediathek.tool.MessageBus;
import net.engio.mbassy.listener.Handler;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jspecify.annotations.NonNull;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableColumnModel;
import java.awt.*;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

public abstract class MVTable extends JTable {
    private static final Logger logger = LogManager.getLogger();
    protected final int[] breite;
    protected final int[] reihe;
    protected final int maxSpalten;
    protected final ColumnVisibilityStore spaltenAnzeigen;
    protected final Optional<MVConfig.Configs> showIconsConfigKey;
    protected final Optional<MVConfig.Configs> smallSenderIconConfigKey;
    /**
     * unmodified JTable used to calculate the row height. Reference only.
     */
    private final JTable probe = new JTable();
    protected boolean useSmallSenderIcons;
    protected List<? extends RowSorter.SortKey> listeSortKeys;
    private int[] selRows = {};
    private boolean showSenderIcon;
    private boolean lineBreak = true;
    protected MVTable(int maxColumns, @NonNull ColumnVisibilityStore visibleColumnStore,
                   @NonNull Optional<MVConfig.Configs> showIconsConfigKey,
                   @NonNull Optional<MVConfig.Configs> smallSenderIconConfigKey) {
        maxSpalten = maxColumns;
        this.showIconsConfigKey = showIconsConfigKey;
        this.smallSenderIconConfigKey = smallSenderIconConfigKey;
        spaltenAnzeigen = visibleColumnStore;
        // make all columns visible by default in column store
        spaltenAnzeigen.fill(true);

        setAutoCreateRowSorter(true);
        setAutoResizeMode(JTable.AUTO_RESIZE_OFF);

        breite = new int[maxSpalten];
        Arrays.fill(breite,-1);

        reihe = new int[maxSpalten];
        Arrays.fill(reihe, -1);

        showIconsConfigKey.ifPresent( key -> showSenderIcon = Boolean.parseBoolean(MVConfig.get(key)));
        smallSenderIconConfigKey.ifPresent(key -> useSmallSenderIcons = Boolean.parseBoolean(MVConfig.get(key)));

        calculateRowHeight();

        MessageBus.getMessageBus().subscribe(this);
    }

    public boolean getUseSmallSenderIcons() {
        return useSmallSenderIcons;
    }

    public void setUseSmallSenderIcons(boolean useSmallSenderIcons) {
        this.useSmallSenderIcons = useSmallSenderIcons;
    }

    protected Color defaultRowBackground(int row) {
        if ((row % 2) != 0) {
            var alternateRowColor = UIManager.getColor("Table.alternateRowColor");
            if (alternateRowColor != null) {
                return alternateRowColor;
            }
        }

        var background = getBackground();
        if (!(background instanceof UIResource)) {
            return background;
        }

        var tableBackground = UIManager.getColor("Table.background");
        return tableBackground != null ? tableBackground : background;
    }

    @Handler
    private void handleFontSizeChanged(FontSizeChangedEvent e) {
        SwingUtilities.invokeLater(this::calculateRowHeight);
    }

    public boolean showSenderIcons() {
        return showSenderIcon;
    }

    public void setShowIcon(boolean newVal) {
        showSenderIcon = newVal;
    }

    public boolean isLineBreak() { return lineBreak;}

    public void setLineBreak(boolean lb) {
        lineBreak = lb;
    }

    /**
     * Return a fictious size of a multi-line text area.
     * @return The fictious size of a multi-line label.
     */
    private int getSizeArea() {
        int lineHeight = getFontMetrics(getFont()).getHeight();
        return lineBreak ? lineHeight * 3 : lineHeight;
    }

    /**
     * Calculate the row height in a table based on icon display,etc.
     */
    public void calculateRowHeight() {
        int minimumHeight = Konstanten.TABLE_DEFAULT_ROW_HEIGHT;

        if (showSenderIcon) {
            minimumHeight = useSmallSenderIcons
                    ? Math.max(Konstanten.TABLE_DEFAULT_ROW_HEIGHT, probe.getRowHeight())
                    : Math.max(Konstanten.TABLE_DEFAULT_LARGE_ICON_ROW_HEIGHT, probe.getRowHeight());
        }

        setRowHeight(Math.max(minimumHeight, getSizeArea()));
    }

    private boolean isColumnVisible(int index) {
        return spaltenAnzeigen.isVisible(index);
    }

    protected void setSpaltenEinAus(int[] nr) {
        for (int i = 0; i < spaltenAnzeigen.length(); ++i) {
            spaltenAnzeigen.setVisible(i, nr[i] > 0);
        }
    }

    public void fireTableDataChanged(boolean setSpalten) {
        if (setSpalten) {
            saveSelectedTableRows();
        }
        var model = (AbstractTableModel)getModel();
        model.fireTableDataChanged();
        if (setSpalten) {
            restoreSelectedTableRows();
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

    protected void saveSelectedTableRows() {
        // Einstellungen der Tabelle merken
        selRows = getSelectedRows();
    }

    protected void restoreSelectedTableRows() {
        if (selRows.length > 0) {
            final int visibleRow;
            if (selRows.length == 1) {
                final var selectedRow = selRows[0];
                selectionModel.setSelectionInterval(selectedRow, selectedRow);
                visibleRow = selectedRow;
            } else {
                selectionModel.setValueIsAdjusting(true);
                for (int selectedRow : selRows) {
                    if (selectedRow < getRowCount()) {
                        addRowSelectionInterval(selectedRow, selectedRow);
                    }
                }
                selectionModel.setValueIsAdjusting(false);
                visibleRow = selRows[0];
            }
            scrollToIndexDelegate(visibleRow);
            requestFocusInWindow();
        }
    }

    protected void changeTableModelColumnWidths() {
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

    protected void changeInternalColumnWidths() {
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
        changeInternalColumnWidths();
        changeTableModelColumnWidths();

        validate();
    }

    public void getSpalten() {
        // Einstellungen der Tabelle merken
        saveSelectedTableRows();

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
            changeInternalColumnWidths();

            final TableColumnModel model = getColumnModel();
            changeTableModelColumnWidths();

            for (int i = 0; i < reihe.length && i < getColumnCount(); ++i) {
                model.moveColumn(convertColumnIndexToView(reihe[i]), i);
            }

            if (listeSortKeys != null) {
                if (!listeSortKeys.isEmpty()) {
                    getRowSorter().setSortKeys(listeSortKeys);
                }
            }

            restoreSelectedTableRows();

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

        if (getRowSorter() != null) {
            getRowSorter().setSortKeys(null); // empty sort keys
        }
        setRowSorter(null);
        setAutoCreateRowSorter(true);
        spaltenAusschalten();
        setSpaltenEinAus(breite);
        setSpalten();
        calculateRowHeight();
    }

    protected abstract void spaltenAusschalten();

    /**
     * Write table display preferences to config.
     */
    public void writeTableConfigurationData() {
        showIconsConfigKey.ifPresent(key -> MVConfig.add(key, String.valueOf(showSenderIcon)));
        smallSenderIconConfigKey.ifPresent(key -> MVConfig.add(key, String.valueOf(useSmallSenderIcons)));
    }
}
