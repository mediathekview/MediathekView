package com.explodingpixels.widgets;

import java.awt.Component;
import java.awt.Graphics;

import javax.swing.CellRendererPane;
import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.border.AbstractBorder;
import javax.swing.border.Border;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;

public class TableHeaderUtils {

    private static final CellRendererPane CELL_RENDER_PANE = new CellRendererPane();

    public static final int NO_COLUMN = -1;

    private static final String PRESSED_COLUMN_KEY = "EPJTableHeader.pressedColumn";
    private static final String SELECTED_COLUMN_KEY = "EPJTableHeader.selectedColumn";
    private static final String SORT_DIRECTION_KEY = "EPJTableHeader.sortDirection";

    private TableHeaderUtils() {
        // no constructor - utility class.
    }

    /**
     * Creates a component that paints the table's header background.
     *
     * @param table the {@link JTable} to create the corner component for.
     * @return a {@link JComponent} that paints the given table's table header background.
     */
    public static JComponent createCornerComponent(final JTable table) {
        return new JComponent() {
            @Override
            protected void paintComponent(Graphics g) {
                paintHeader(g, table, 0, getWidth());
            }
        };
    }

    /**
     * Installs a custom {@link Border} on the given table's {@link JTableHeader} that paints any
     * blank area to the right of the last column header with the {@code JTableHeader}'s background.
     *
     * @param table the {@link JTable} from which to get the {@code JTableHeader} to paint the
     *              empty column header space for.
     */
    public static void makeHeaderFillEmptySpace(JTable table) {
        table.getTableHeader().setBorder(createTableHeaderEmptyColumnPainter(table));
    }

    /**
     * Paints the given JTable's table default header background at given
     * x for the given width.
     *
     * @param graphics the {@link Graphics} to paint into.
     * @param table    the table that the header belongs to.
     * @param x        the x coordinate of the table header.
     * @param width    the width of the table header.
     */
    public static void paintHeader(Graphics graphics, JTable table, int x, int width) {
        TableCellRenderer renderer = table.getTableHeader().getDefaultRenderer();
        Component component = renderer.getTableCellRendererComponent(
                table, "", false, false, -1, table.getColumnCount());

        component.setBounds(0, 0, width, table.getTableHeader().getHeight());

        ((JComponent) component).setOpaque(false);
        CELL_RENDER_PANE.paintComponent(graphics, component, null, x, 0,
                width, table.getTableHeader().getHeight(), true);
    }

    /**
     * Creates a {@link Border} that paints any empty space to the right of the last column header
     * in the given {@link JTable}'s {@link JTableHeader}.
     */
    private static Border createTableHeaderEmptyColumnPainter(final JTable table) {
        return new AbstractBorder() {
            @Override
            public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
                // if this JTableHeader is parented in a JViewport, then paint the table header
                // background to the right of the last column if neccessary.
                JViewport viewport = (JViewport) table.getParent();
                if (viewport != null && table.getWidth() < viewport.getWidth()) {
                    int startX = table.getWidth();
                    int emptyColumnWidth = viewport.getWidth() - table.getWidth();
                    paintHeader(g, table, startX, emptyColumnWidth);
                }
            }
        };
    }

    // Support for making column headers selected and sorted. /////////////////////////////////////

    /**
     * Get's the pressed column header for the given {@link JTableHeader}.
     *
     * @param tableHeader the {@code JTableHeader} to determine the pressed column header for.
     * @return the column model index of the pressed column header, or {@link #NO_COLUMN} if no
     *         column's header has been pressed.
     * @see #isColumnPressed(javax.swing.table.JTableHeader, int)
     */
    public static int getPressedColumn(JTableHeader tableHeader) {
        Object pressedColumnValue = tableHeader.getClientProperty(PRESSED_COLUMN_KEY);
        return pressedColumnValue != null && pressedColumnValue instanceof Integer
                ? ((Integer) pressedColumnValue) : NO_COLUMN;
    }

    /**
     * {@code true} if the given column model index is pressed in the given {@link JTableHeader}.
     *
     * @param tableHeader      the {@code JTableHeader} to use when determining if the given column is
     *                         pressed.
     * @param columnModelIndex the column model index to determine the pressed state of.
     * @return {@code true} if the given column is pressed.
     * @see #getPressedColumn(javax.swing.table.JTableHeader)
     * @see #setPressedColumn(javax.swing.table.JTableHeader, int)
     */
    public static boolean isColumnPressed(JTableHeader tableHeader, int columnModelIndex) {
        int pressedColumn = getPressedColumn(tableHeader);
        return pressedColumn >= 0 && pressedColumn == columnModelIndex;
    }

    /**
     * Sets the given column for the given {@link JTableHeader} as pressed. Calling this method
     * installs a hint, which can be used by renders to draw the correct column pressed state.
     * Renders can use {@link #isColumnPressed(javax.swing.table.JTableHeader, int)} to determine
     * what state to draw the header for the given index in.
     * header state.
     *
     * @param tableHeader      the {@code JTableHeader} to set the pressed state for.
     * @param columnModelIndex the column model index of the pressed column.
     */
    public static void setPressedColumn(JTableHeader tableHeader, int columnModelIndex) {
        tableHeader.putClientProperty(PRESSED_COLUMN_KEY, columnModelIndex);
    }

    /**
     * Get's the selected column header for the given {@link JTableHeader}.
     *
     * @param tableHeader the {@code JTableHeader} to determine the selected column header for.
     * @return the column model index of the selected column header, or {@link #NO_COLUMN} if no
     *         column's header has been pressed.
     * @see #isColumnSelected(javax.swing.table.JTableHeader, int)
     */
    public static int getSelectedColumn(JTableHeader tableHeader) {
        Object selectedColumnValue = tableHeader.getClientProperty(SELECTED_COLUMN_KEY);
        return selectedColumnValue != null && selectedColumnValue instanceof Integer
                ? ((Integer) selectedColumnValue) : NO_COLUMN;
    }

    /**
     * {@code true} if the given column model index is selected in the given {@link JTableHeader}.
     * <p/>
     * Note that there is no direct way to set wheter a column is selected or not. Selection is a
     * by-product of set the sort direction on a column.
     *
     * @param tableHeader      the {@code JTableHeader} to use when determining if the given column is
     *                         selected.
     * @param columnModelIndex the column model index to determine the selected state of.
     * @return {@code true} if the given column is selected.
     * @see #getSelectedColumn(javax.swing.table.JTableHeader)
     * @see #setSortDirection(javax.swing.table.JTableHeader, int, com.explodingpixels.widgets.TableUtils.SortDirection)
     */
    public static boolean isColumnSelected(JTableHeader tableHeader, int columnModelIndex) {
        int selectedColumn = getSelectedColumn(tableHeader);
        return selectedColumn >= 0 && selectedColumn == columnModelIndex;
    }

    /**
     * Get's the pressed column header for the given {@link JTableHeader}.
     *
     * @param tableHeader      the {@code JTableHeader} containing the column to determine the sort
     *                         direction for.
     * @param columnModelIndex the column model index to determine the sort direction for.
     * @return the {@link com.explodingpixels.widgets.TableUtils.SortDirection} of the given
     *         column index.
     * @see #setSortDirection(javax.swing.table.JTableHeader, int, com.explodingpixels.widgets.TableUtils.SortDirection)
     * @see #toggleSortDirection(javax.swing.table.JTableHeader, int)
     */
    public static TableUtils.SortDirection getSortDirection(JTableHeader tableHeader, int columnModelIndex) {
        Object sortDirection = tableHeader.getClientProperty(SORT_DIRECTION_KEY);
        boolean isColumnSelected = isColumnSelected(tableHeader, columnModelIndex);
        return sortDirection != null && sortDirection instanceof String && isColumnSelected
                ? TableUtils.SortDirection.find((String) sortDirection) : TableUtils.SortDirection.NONE;
    }

    /**
     * Toggles the sort direction of the given column model index in the given {@link JTableHeader}.
     * This also makes the given column selected. If the given column has no sort order set
     * ({@link com.explodingpixels.widgets.TableUtils.SortDirection#NONE}) then the new sort order
     * will be {@link com.explodingpixels.widgets.TableUtils.SortDirection#ASCENDING}. Otherwise
     * the new sort order will be {@link com.explodingpixels.widgets.TableUtils.SortDirection#DESCENDING}
     *
     * @param tableHeader      the {@code JTableHeader} of the column to toggle the sort order for.
     * @param columnModelIndex the column model index to toggle the sort order for.
     * @return the new sort order of the column.
     */
    public static TableUtils.SortDirection toggleSortDirection(JTableHeader tableHeader, int columnModelIndex) {
        // grab the previously selected column as well as the sort direction for the given column, if either exist.
        int oldSelectedColumn = getSelectedColumn(tableHeader);
        TableUtils.SortDirection oldSortDirection = getSortDirection(tableHeader, columnModelIndex);

        // if the old selected column is different from the given column, or the old sort direction is set to none,
        //    then the new sort direction is "ascending".
        // else the new sort direction is "descending.
        TableUtils.SortDirection newSortDirection;
        if (oldSelectedColumn != columnModelIndex || oldSortDirection == TableUtils.SortDirection.NONE) {
            newSortDirection = TableUtils.SortDirection.ASCENDING;
        } else {
            newSortDirection =
                    oldSortDirection == TableUtils.SortDirection.ASCENDING
                            ? TableUtils.SortDirection.DESCENDING : TableUtils.SortDirection.ASCENDING;
        }

        // set the new sort direction on the given column model index.
        setSortDirection(tableHeader, columnModelIndex, newSortDirection);

        return newSortDirection;
    }

    private static void setSortDirection(JTableHeader tableHeader, int columnModelIndex,
                                         TableUtils.SortDirection sortDirection) {
        tableHeader.putClientProperty(SELECTED_COLUMN_KEY, columnModelIndex);
        tableHeader.putClientProperty(SORT_DIRECTION_KEY, sortDirection.getValue());
    }

}
