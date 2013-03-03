package com.explodingpixels.macwidgets;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.border.Border;
import javax.swing.table.TableCellRenderer;

import com.explodingpixels.macwidgets.plaf.EmphasizedLabelUI;
import com.explodingpixels.painter.MacWidgetsPainter;
import com.explodingpixels.widgets.TableHeaderUtils;
import com.explodingpixels.widgets.TableUtils;
import com.explodingpixels.widgets.WindowUtils;

/**
 * A table header renderer for an iTunes style table. Note that this class specifically extends
 * {@link JLabel} in order to be compatible with Glazed Lists. Glazed Lists looks for a label in the
 * header renderer in order to install the sort icon, if necessary.
 */
public class ITunesTableHeaderRenderer extends JLabel implements TableCellRenderer {

    private JTable fTable;
    private int fColumnModelIndexBeingPainted = -1;

    //private static Color HIGHLIGHT_BORDER_COLOR = new Color(255, 255, 255, 77);
    private static Color BORDER_COLOR = new Color(0, 0, 0, 51);
    private static Color UNFOCUSED_FONT_COLOR = new Color(0x8f8f8f);

    private static Border BORDER = BorderFactory.createCompoundBorder(
            BorderFactory.createMatteBorder(0, 0, 1, 0, MacColorUtils.LEOPARD_BORDER_COLOR),
            BorderFactory.createCompoundBorder(
                    BorderFactory.createMatteBorder(0, 0, 0, 1, BORDER_COLOR),
                    BorderFactory.createEmptyBorder(1, 5, 0, 5)));

    private static final int SORT_ICON_INDENT_PIXELS = 6;
    private static final Color SORT_ICON_COLOR = new Color(0, 0, 0, 175);

    public ITunesTableHeaderRenderer(JTable table) {
        fTable = table;
        init();
    }

    private void init() {
        MacWidgetFactory.makeEmphasizedLabel(this,
                EmphasizedLabelUI.DEFAULT_FOCUSED_FONT_COLOR, UNFOCUSED_FONT_COLOR,
                EmphasizedLabelUI.DEFAULT_EMPHASIS_COLOR);
        setBorder(BORDER);

        // TODO table column re-ordering is not supported at this time.
        fTable.getTableHeader().setReorderingAllowed(false);
    }

    public void setSortDelegate(TableUtils.SortDelegate sortDelegate) {
        TableUtils.makeSortable(fTable, sortDelegate);
    }

    public Component getTableCellRendererComponent(
            JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {

        // if the given value is an Icon, then use that. otherwise, use the string version of the
        // given value.
        if (value instanceof Icon) {
            setIcon((Icon) value);
            setText("");
            setHorizontalAlignment(SwingConstants.CENTER);
        } else {
            setIcon(null);
            setText(value.toString());
            setFont(fTable.getTableHeader().getFont());
            setHorizontalAlignment(SwingConstants.LEFT);
        }

        // keep the index of the column we're rendering. if the column isn't a value in the model,
        // then use -1 as the index.
        fColumnModelIndexBeingPainted = 0 <= column && column < fTable.getColumnCount()
                ? fTable.convertColumnIndexToModel(column) : -1;

        return this;
    }

    @Override
    protected void paintComponent(Graphics g) {
        Graphics2D graphics2d = (Graphics2D) g.create();
        MacWidgetsPainter<Component> painter = getBackgroundPainter();
        painter.paint(graphics2d, this, getWidth(), getHeight());

        super.paintComponent(g);

        paintSortIndicatorIfNecessary(graphics2d);
        graphics2d.dispose();
    }

    private void paintSortIndicatorIfNecessary(Graphics2D graphics2d) {
        TableUtils.SortDirection sortDirection = getColumnBeingPaintedSortDirection();
        if (sortDirection != TableUtils.SortDirection.NONE) {
            paintSortIndicator(graphics2d, sortDirection);
        }
    }

    private void paintSortIndicator(Graphics2D graphics2d,
                                    TableUtils.SortDirection sortDirection) {
        Shape sortShape = sortDirection == TableUtils.SortDirection.ASCENDING
                ? createSortAscendingShape() : createSortDescendingShape();

        int x = getWidth() - sortShape.getBounds().width - SORT_ICON_INDENT_PIXELS;
        int y = getHeight() / 2 - sortShape.getBounds().height / 2;

        graphics2d.setRenderingHint(
                RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        graphics2d.translate(x, y);
        graphics2d.setColor(SORT_ICON_COLOR);
        graphics2d.fill(sortShape);
    }

    // Utility methods. ///////////////////////////////////////////////////////////////////////////

    private boolean isColumnBeingPaintedPressed() {
        return TableHeaderUtils.isColumnPressed(fTable.getTableHeader(), fColumnModelIndexBeingPainted);
    }

    private boolean isColumnBeingPaintedSelected() {
        return TableHeaderUtils.isColumnSelected(fTable.getTableHeader(), fColumnModelIndexBeingPainted);
    }

    private TableUtils.SortDirection getColumnBeingPaintedSortDirection() {
        return TableHeaderUtils.getSortDirection(fTable.getTableHeader(), fColumnModelIndexBeingPainted);
    }

    private MacWidgetsPainter<Component> getBackgroundPainter() {
        MacWidgetsPainter<Component> retVal;

        boolean windowHasFocus = WindowUtils.isParentWindowFocused(fTable);
        boolean isColumnSelected = isColumnBeingPaintedSelected();
        boolean isColumnPressed = isColumnBeingPaintedPressed();

        // TODO cleanup this logic.
        if (!fTable.isEnabled()) {
            retVal = MacPainterFactory.createIAppUnpressedUnselectedHeaderPainter();
        } else if (windowHasFocus && isColumnPressed && isColumnSelected) {
            retVal = MacPainterFactory.createIAppPressedSelectedHeaderPainter();
        } else if (windowHasFocus && isColumnPressed) {
            retVal = MacPainterFactory.createIAppPressedUnselectedHeaderPainter();
        } else if (windowHasFocus && isColumnSelected) {
            retVal = MacPainterFactory.createIAppUnpressedSelectedHeaderPainter();
        } else {
            retVal = MacPainterFactory.createIAppUnpressedUnselectedHeaderPainter();
        }
        return retVal;
    }

    private static GeneralPath createSortAscendingShape() {
        float width = 7;
        float height = 6;
        GeneralPath path = new GeneralPath();
        path.moveTo(width / 2.0f, 0.0f);
        path.lineTo(width, height);
        path.lineTo(0.0f, height);
        path.lineTo(width / 2.0f, 0.0f);
        return path;
    }

    private static GeneralPath createSortDescendingShape() {
        GeneralPath path = createSortAscendingShape();
        double centerX = path.getBounds2D().getWidth() / 2.0;
        double centerY = path.getBounds2D().getHeight() / 2.0;
        path.transform(AffineTransform.getRotateInstance(Math.PI, centerX, centerY));
        return path;
    }
}
