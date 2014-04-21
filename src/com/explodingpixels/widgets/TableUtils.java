package com.explodingpixels.widgets;

import java.awt.Cursor;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.JTable;
import javax.swing.table.TableColumnModel;

/**
 * A collection of utility methods to be used with {@link JTable}.
 */
public class TableUtils {

    private TableUtils() {
        // no constructor - utility class.
    }

    // Sort support. //////////////////////////////////////////////////////////////////////////////

    /**
     * An enumeration representing the sort order of a table column.
     */
    public enum SortDirection {
        NONE(""), ASCENDING("ascending"), DESCENDING("descending");

        private final String fValue;

        SortDirection(String value) {
            fValue = value;
        }

        String getValue() {
            return fValue;
        }

        static SortDirection find(String value) {
            for (SortDirection sortDirection : values()) {
                if (sortDirection.getValue().equals(value)) {
                    return sortDirection;
                }
            }
            throw new IllegalArgumentException("No sort direction found for " + value);
        }
    }

    /**
     * An interface that will be notified when sorting of a {@link JTable} should occur.
     *
     * @see TableUtils#makeSortable(javax.swing.JTable, com.explodingpixels.widgets.TableUtils.SortDelegate)
     */
    public interface SortDelegate {
        /**
         * Called when a table should sort its' rows based on the given column.
         *
         * @param columnModelIndex the column model index to base the sorting on.
         * @param sortDirection    the direction to sort the table's rows in.
         */
        void sort(int columnModelIndex, SortDirection sortDirection);
    }

    /**
     * Installs a listener on the given {@link JTable}'s {@link javax.swing.table.JTableHeader},
     * which will notify the given {@link SortDelegate} when the user clicks the header
     * and thus wishes to sort. The listener will also call
     * {@link com.explodingpixels.widgets.TableHeaderUtils#toggleSortDirection(javax.swing.table.JTableHeader, int)}
     * and {@link TableHeaderUtils#setPressedColumn(javax.swing.table.JTableHeader, int)}
     * which will install hints for header renders to render the column headers in the
     * appropriate state.
     *
     * @param table        the table so install the {@code SortDelegate} on.
     * @param sortDelegate the delegate to notify when sorting should be performed.
     */
    public static void makeSortable(JTable table, SortDelegate sortDelegate) {
        validateSortDelegate(sortDelegate);
        MouseListener mouseListener = new ColumnHeaderMouseListener(table, sortDelegate);
        table.getTableHeader().addMouseListener(mouseListener);
    }

    private static void validateSortDelegate(SortDelegate sortDelegate) {
        if (sortDelegate == null) {
            throw new IllegalArgumentException("The given SortDelegate cannot be null.");
        }
    }

    private static class ColumnHeaderMouseListener extends MouseAdapter {

        private final JTable fTable;
        private final TableUtils.SortDelegate fSortDelegate;
        private boolean fMouseEventIsPerformingPopupTrigger = false;

        private ColumnHeaderMouseListener(JTable fTable, TableUtils.SortDelegate fSortDelegate) {
            this.fTable = fTable;
            this.fSortDelegate = fSortDelegate;
        }

        public void mouseClicked(MouseEvent mouseEvent) {
            if (shouldProcessMouseClicked()) {
                final TableColumnModel columnModel = fTable.getColumnModel();
                int columnViewIndex = columnModel.getColumnIndexAtX(mouseEvent.getX());
                int columnModelIndex = fTable.convertColumnIndexToModel(columnViewIndex);
                TableUtils.SortDirection sortDirection =
                        TableHeaderUtils.toggleSortDirection(fTable.getTableHeader(), columnModelIndex);
                fSortDelegate.sort(columnModelIndex, sortDirection);

                fTable.getTableHeader().repaint();
            }
        }

        private boolean shouldProcessMouseClicked() {
            return !fMouseEventIsPerformingPopupTrigger && isNotResizeCursor();
        }

        private boolean isNotResizeCursor() {
            return fTable.getTableHeader().getCursor() != Cursor.getPredefinedCursor(Cursor.E_RESIZE_CURSOR);
        }

        public void mousePressed(MouseEvent mouseEvent) {
            fMouseEventIsPerformingPopupTrigger = mouseEvent.isPopupTrigger();

            if (isNotResizeCursor()) {
                final TableColumnModel columnModel = fTable.getColumnModel();
                int viewColumnIndex = columnModel.getColumnIndexAtX(mouseEvent.getX());
                int columnModelIndex = fTable.convertColumnIndexToModel(viewColumnIndex);

                TableHeaderUtils.setPressedColumn(fTable.getTableHeader(), columnModelIndex);
                fTable.getTableHeader().repaint();
            }
        }

        @Override
        public void mouseReleased(MouseEvent e) {
            TableHeaderUtils.setPressedColumn(fTable.getTableHeader(), TableHeaderUtils.NO_COLUMN);
            fTable.getTableHeader().repaint();
        }
    }
}
