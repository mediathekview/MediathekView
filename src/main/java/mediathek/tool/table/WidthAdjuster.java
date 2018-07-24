package mediathek.tool.table;

import javax.swing.*;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.List;

class WidthAdjuster extends MouseAdapter {

    private static final int EPSILON = 5;   //boundary sensitivity
    private final JTable table;
    private final JTableHeader tableHeader;
    private final Cursor EAST = Cursor.getPredefinedCursor(Cursor.E_RESIZE_CURSOR);
    private final Cursor WEST = Cursor.getPredefinedCursor(Cursor.W_RESIZE_CURSOR);
    private List<? extends RowSorter.SortKey> listeSortKeys = null;

    protected WidthAdjuster(final JTable table) {
        this.table = table;
        tableHeader = table.getTableHeader();
    }

    @Override
    public void mousePressed(final MouseEvent evt) {
        if (evt.getClickCount() == 1) {
            if (table.getRowSorter() != null) {
                listeSortKeys = table.getRowSorter().getSortKeys();
            } else {
                listeSortKeys = null;
            }
        }
        if (evt.getClickCount() > 1 && usingResizeCursor()) {
            resize(getLeftColumn(evt.getPoint()));
        }
    }

    private boolean usingResizeCursor() {
        final Cursor cursor = tableHeader.getCursor();
        return cursor.equals(EAST) || cursor.equals(WEST);
    }

    //if near the boundary, will choose left column
    private int getLeftColumn(final Point pt) {
        pt.x -= EPSILON;
        return tableHeader.columnAtPoint(pt);
    }

    private void resize(final int col) {

        final TableColumnModel tcm = table.getColumnModel();
        final TableColumn tc = tcm.getColumn(col);
        TableCellRenderer tcr = tc.getHeaderRenderer();

        if (tcr == null) {
            tcr = tableHeader.getDefaultRenderer();
        }

        Object obj = tc.getHeaderValue();
        Component comp = tcr.getTableCellRendererComponent(table, obj, false, false, 0, 0);
        int maxWidth = comp.getPreferredSize().width;

        for (int i = 0, ub = table.getRowCount(); i != ub; ++i) {
            tcr = table.getCellRenderer(i, col);
            obj = table.getValueAt(i, col);
            comp = tcr.getTableCellRendererComponent(table, obj, false, false, i, col);
            final int w = comp.getPreferredSize().width;
            if (w > maxWidth) {
                maxWidth = w;
            }
        }

        maxWidth += 10; //and room to grow...
        tc.setPreferredWidth(maxWidth); //remembers the value
        tc.setWidth(maxWidth);          //forces layout, repaint

        if (listeSortKeys != null) {
            if (!listeSortKeys.isEmpty()) {
                table.getRowSorter().setSortKeys(listeSortKeys);
            }
        }
    }

}
