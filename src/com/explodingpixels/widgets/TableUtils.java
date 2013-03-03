package com.explodingpixels.widgets;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.BorderFactory;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.table.TableColumnModel;

/**
 * A collection of utility methods to be used with {@link JTable}.
 */
public class TableUtils {

    private TableUtils() {
        // no constructor - utility class.
    }

    /**
     * Add's striping to the background of the given {@link JTable}. The actual striping is
     * installed on the containing {@link JScrollPane}'s {@link JViewport}, so if this table is not
     * added to a {@code JScrollPane}, then no stripes will be painted. This method can be called
     * before the given table is added to a scroll pane, though, as a {@link PropertyChangeListener}
     * will be installed to handle "ancestor" changes.
     *
     * @param table      the table to paint row stripes for.
     * @param stipeColor the color of the stripes to paint.
     */
    public static void makeStriped(JTable table, Color stipeColor) {
        table.addPropertyChangeListener("ancestor",
                createAncestorPropertyChangeListener(table, stipeColor));
        // install a listener to cause the whole table to repaint when a column is resized. we do
        // this because the extended grid lines may need to be repainted. this could be cleaned up,
        // but for now, it works fine.
        for (int i = 0; i < table.getColumnModel().getColumnCount(); i++) {
            table.getColumnModel().getColumn(i).addPropertyChangeListener(
                    createAncestorPropertyChangeListener(table, stipeColor)
            );
        }
    }

    private static PropertyChangeListener createAncestorPropertyChangeListener(
            final JTable table, final Color stipeColor) {
        return new PropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                // indicate that the parent of the JTable has changed.
                parentDidChange(table, stipeColor);
            }
        };
    }

    private static void parentDidChange(JTable table, Color stipeColor) {
        // if the parent of the table is an instance of JViewport, and that JViewport's parent is
        // a JScrollpane, then install the custom BugFixedViewportLayout.
        if (table.getParent() instanceof JViewport
                && table.getParent().getParent() instanceof JScrollPane) {

            JScrollPane scrollPane = (JScrollPane) table.getParent().getParent();
            scrollPane.setViewportBorder(
                    new StripedViewportBorder(scrollPane.getViewport(), table, stipeColor));
            scrollPane.getViewport().setOpaque(false);

            scrollPane.setCorner(JScrollPane.UPPER_RIGHT_CORNER,
                    TableHeaderUtils.createCornerComponent(table));
            scrollPane.setBorder(BorderFactory.createEmptyBorder());
        }
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
