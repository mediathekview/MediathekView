package mediathek.tool.table;

import mediathek.config.MVColor;
import mediathek.config.MVConfig;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.gui.tabs.tab_film.GuiFilme;
import mediathek.tool.FilmSize;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.util.*;
import java.util.List;

public class MVFilmTable extends PersistentColumnConfigurationTable {
    private static final Logger logger = LogManager.getLogger();
    private MyRowSorter<TableModel> sorter;
    private List<DatenFilm.FilmIdentity> selectedFilmIdentities = List.of();
    private int selectionAnchorRow = -1;

    public MVFilmTable() {
        super(DatenFilm.MAX_ELEM, ColumnVisibilityStore.of(GuiFilme.VISIBLE_COLUMNS),
                Optional.of(MVConfig.Configs.SYSTEM_TAB_FILME_ICON_ANZEIGEN),
                Optional.of(MVConfig.Configs.SYSTEM_TAB_FILME_ICON_KLEIN),
                MVConfig.Configs.SYSTEM_EIGENSCHAFTEN_TABELLE_FILME);

        setAutoCreateRowSorter(false);
        addPropertyChangeListener("model", evt -> {
            //we need to setup sorter later as the model is invalid at ctor point...
            var model = (TableModel) evt.getNewValue();
            if (sorter == null) {
                sorter = new MyRowSorter<>(model);
                sorter.setModel(model);
                setRowSorter(sorter);
            }
            else
                sorter.setModel(model);
        });
    }

    protected static Color blend(Collection<Color> colors) {
        if (colors == null || colors.isEmpty()) {
            return null;
        }

        int a = 0;
        int r = 0;
        int g = 0;
        int b = 0;

        for (Color color : colors) {
            a += color.getAlpha();
            r += color.getRed();
            g += color.getGreen();
            b += color.getBlue();
        }

        int size = colors.size();
        return new Color(r / size, g / size, b / size, a / size);
    }

    @Override
    public Component prepareRenderer(TableCellRenderer renderer, int row, int column) {
        var component = super.prepareRenderer(renderer, row, column);
        if (isRowSelected(row)) {
            return component;
        }

        decorateUnselectedRow(component, row);
        return component;
    }

    private void decorateUnselectedRow(Component component, int viewRow) {
        var film = filmAtViewRow(viewRow);
        component.setBackground(backgroundForRow(viewRow, film));
        component.setForeground(foregroundFor(film));
    }

    private DatenFilm filmAtViewRow(int viewRow) {
        return (DatenFilm) getModel().getValueAt(convertRowIndexToModel(viewRow), DatenFilm.FILM_REF);
    }

    private Color foregroundFor(@NotNull DatenFilm film) {
        return film.isNew() ? MVColor.NEW_COLOR.getColor() : getForeground();
    }

    private Color backgroundForRow(int viewRow, @NotNull DatenFilm film) {
        var backgrounds = new ArrayList<Color>(4);
        backgrounds.add(defaultRowBackground(viewRow));

        if (SeenHistoryController.hasBeenSeenFromSharedCache(film)) {
            backgrounds.add(MVColor.FILM_HISTORY.getColor());
        }
        if (film.isBookmarked()) {
            backgrounds.add(MVColor.FILM_BOOKMARKED.getColor());
        }
        if (film.isDuplicate()) {
            backgrounds.add(MVColor.FILM_DUPLICATE.getColor());
        }

        return backgrounds.size() == 1 ? backgrounds.getFirst() : blend(backgrounds);
    }

    @Override
    public String getToolTipText(MouseEvent e) {
        var point = e.getPoint();
        int viewColumn = columnAtPoint(point);
        int viewRow = rowAtPoint(point);

        if (!isTitleColumn(viewColumn)) {
            return super.getToolTipText(e);
        }

        try {
            return isTitleTruncatedAt(viewRow, viewColumn) ? filmAtViewRow(viewRow).getTitle() : null;
        } catch (RuntimeException _) {
            // catch null pointer exception if mouse is over an empty line
            return null;
        }
    }

    private boolean isTitleColumn(int viewColumn) {
        return viewColumn >= 0 && convertColumnIndexToModel(viewColumn) == DatenFilm.FILM_TITEL;
    }

    private boolean isTitleTruncatedAt(int viewRow, int viewColumn) {
        var component = prepareRenderer(getCellRenderer(viewRow, viewColumn), viewRow, viewColumn);
        var bounds = getCellRect(viewRow, viewColumn, false);
        return component.getPreferredSize().width > bounds.width;
    }

    private void resetFilmeTab(int i) {
        reihe[i] = i;
        breite[i] = defaultColumnWidth(i);
    }

    private int defaultColumnWidth(int column) {
        return switch (column) {
            case DatenFilm.FILM_NR -> 75;
            case DatenFilm.FILM_TITEL -> 300;
            case DatenFilm.FILM_DATUM, DatenFilm.FILM_ZEIT, DatenFilm.FILM_SENDER, DatenFilm.FILM_GROESSE,
                 DatenFilm.FILM_DAUER, DatenFilm.FILM_GEO -> 100;
            case DatenFilm.FILM_URL -> 500;
            case DatenFilm.FILM_ABSPIELEN, DatenFilm.FILM_AUFZEICHNEN, DatenFilm.FILM_MERKEN -> 20;
            case DatenFilm.FILM_HD, DatenFilm.FILM_UT -> 50;
            default -> 200;
        };
    }

    @Override
    public void resetTabelle() {
        for (int i = 0; i < maxSpalten; ++i) {
            resetFilmeTab(i);
        }

        getRowSorter().setSortKeys(null); // empty sort keys
        spaltenAusschalten();
        setSpaltenEinAus(breite);
        setSpalten();
        calculateRowHeight();
    }

    @Override
    protected void spaltenAusschalten() {
        // do nothing here
    }

    @Override
    public void getSpalten() {
        saveSelectedTableRows();

        int columnCount = getModel().getColumnCount();
        for (int i = 0; i < reihe.length && i < columnCount; ++i) {
            reihe[i] = convertColumnIndexToModel(i);
        }

        for (int i = 0; i < breite.length && i < columnCount; ++i) {
            breite[i] = getColumnModel().getColumn(convertColumnIndexToView(i)).getWidth();
        }

        var rowSorter = getRowSorter();
        listeSortKeys = rowSorter != null ? rowSorter.getSortKeys() : null;
    }

    @Override
    protected void saveSelectedTableRows() {
        super.saveSelectedTableRows();

        var selectedRows = getSelectedRows();
        selectionAnchorRow = selectedRows.length > 0 ? selectedRows[0] : -1;
        if (selectedRows.length == 0) {
            selectedFilmIdentities = List.of();
            return;
        }

        var identities = new ArrayList<DatenFilm.FilmIdentity>(selectedRows.length);
        for (int selectedRow : selectedRows) {
            if (selectedRow >= 0 && selectedRow < getRowCount()) {
                identities.add(filmAtViewRow(selectedRow).getFilmIdentity());
            }
        }
        selectedFilmIdentities = List.copyOf(identities);
    }

    @Override
    protected void restoreSelectedTableRows() {
        if (selectedFilmIdentities.isEmpty()) {
            super.restoreSelectedTableRows();
            return;
        }

        clearSelection();

        int firstVisibleRow = -1;
        selectionModel.setValueIsAdjusting(true);
        try {
            for (int viewRow = 0; viewRow < getRowCount(); viewRow++) {
                var filmIdentity = filmAtViewRow(viewRow).getFilmIdentity();
                if (selectedFilmIdentities.contains(filmIdentity)) {
                    addRowSelectionInterval(viewRow, viewRow);
                    if (firstVisibleRow == -1) {
                        firstVisibleRow = viewRow;
                    }
                }
            }
        } finally {
            selectionModel.setValueIsAdjusting(false);
        }

        if (firstVisibleRow != -1) {
            scrollToIndexDelegate(firstVisibleRow);
            requestFocus();
            return;
        }

        if (getRowCount() > 0 && selectionAnchorRow >= 0) {
            int fallbackRow = Math.min(selectionAnchorRow, getRowCount() - 1);
            selectionModel.setSelectionInterval(fallbackRow, fallbackRow);
            scrollToIndexDelegate(fallbackRow);
            requestFocus();
        }
    }

    private void reorderColumns() {
        final TableColumnModel model = getColumnModel();
        var numCols = getColumnCount();
        for (int i = 0; i < reihe.length && i < numCols; ++i) {
            //move only when there are changes...
            if (reihe[i] != i)
                model.moveColumn(convertColumnIndexToView(reihe[i]), i);
        }
    }

    private void restoreSortKeys() {
        if (listeSortKeys != null) {
            var rowSorter = getRowSorter();
            var tblSortKeys = rowSorter.getSortKeys();
            if (!(listeSortKeys == tblSortKeys)) {
                if (!listeSortKeys.isEmpty()) {
                    rowSorter.setSortKeys(listeSortKeys);
                }
            }
        }
    }

    /**
     * Setzt die gemerkte Position der Spalten in der Tabelle wieder.
     * Ziemlich ineffizient!
     */
    @Override
    public void setSpalten() {
        try {
            changeInternalColumnWidths();
            changeTableModelColumnWidths();
            reorderColumns();
            restoreSortKeys();
            restoreSelectedTableRows();
            validate();
        } catch (Exception ex) {
            logger.error("setSpalten", ex);
        }
    }

    static class MyRowSorter<M extends TableModel> extends TableRowSorter<M> {
        public MyRowSorter(M model) {
            super(model);
        }

        @Override
        public void setModel(M model) {
            super.setModel(model);
            configureSortableColumns();
            configureComparators();
        }

        private void configureSortableColumns() {
            setSortable(DatenFilm.FILM_ABSPIELEN, false);
            setSortable(DatenFilm.FILM_AUFZEICHNEN, false);
            setSortable(DatenFilm.FILM_GEO, false);
            setSortable(DatenFilm.FILM_MERKEN, false);
        }

        private void configureComparators() {
            setComparator(DatenFilm.FILM_GROESSE, (Comparator<FilmSize>) FilmSize::compareTo);
            setComparator(DatenFilm.FILM_SENDER, (Comparator<String>) String::compareTo);
            setComparator(DatenFilm.FILM_ZEIT, (Comparator<String>) String::compareTo);
            setComparator(DatenFilm.FILM_URL, (Comparator<String>) String::compareTo);
            setComparator(DatenFilm.FILM_DAUER, Comparator.naturalOrder());
        }

        @Override
        public void setSortKeys(List<? extends SortKey> sortKeys) {
            super.setSortKeys(sortKeys == null ? null : sortKeys.stream().limit(1).toList());
        }
    }
}
