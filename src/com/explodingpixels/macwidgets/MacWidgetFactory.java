package com.explodingpixels.macwidgets;

import java.awt.Color;
import java.awt.Dimension;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.plaf.basic.BasicSplitPaneUI;
import javax.swing.table.TableModel;

import com.explodingpixels.macwidgets.plaf.EmphasizedLabelUI;
import com.explodingpixels.macwidgets.plaf.ITunesTableUI;

/**
 * A factory for creating various types of Mac style widgets. See each method's javadoc for detailed
 * descriptions of the components, as well as screen shots and links to specific sections in Apples
 * Human Interface Guidelines.
 */
public class MacWidgetFactory {

    /**
     * Creates an iTunes style table like the one listed below:
     * <br/>
     * <img src="../../../../graphics/iTunesTable.png">
     * <br/>
     * Sorting indicators will be rendered if a
     * {@link com.explodingpixels.widgets.TableUtils.SortDelegate} is installed on the associated
     * {@link JTable} via the
     * {@link com.explodingpixels.widgets.TableUtils#makeSortable(javax.swing.JTable, com.explodingpixels.widgets.TableUtils.SortDelegate)}
     * method.
     * <p/>
     * To wrap this table in an iApp style {@link JScrollPane}, see
     * {@link IAppWidgetFactory#createScrollPane(java.awt.Component)};
     */
    public static JTable createITunesTable(TableModel tableModel) {
        JTable table = new JTable(tableModel);
        table.setUI(new ITunesTableUI());
        return table;
    }

    public static ComponentBottomBar createComponentStatusBar() {
        return new ComponentBottomBar();
    }

    /**
     * Creates a transparent spacer of the given width and height. If you don't care about a
     * particular dimension, that is, you only want a horiztonal spacer, than simply provide zero
     * for the value your not interested in.
     *
     * @param width  the width of the spacer - zero if the width doesn't matter.
     * @param height the height of the spacer - zero if the height doesn't matter.
     * @return a transparent spacer of the given size.
     */
    public static JComponent createSpacer(int width, int height) {
        JLabel label = new JLabel();
        label.setOpaque(false);
        label.setPreferredSize(new Dimension(width, height));
        return label;
    }

    public static JLabel createEmphasizedLabel(String text) {
        return makeEmphasizedLabel(new JLabel(text));
    }

    public static JLabel makeEmphasizedLabel(JLabel label) {
        label.setUI(new EmphasizedLabelUI());
        return label;
    }

    public static JLabel makeEmphasizedLabel(JLabel label, Color focusedColor, Color unfocusedColor,
                                             Color emphasisColor) {
        label.setUI(new EmphasizedLabelUI(focusedColor, unfocusedColor, emphasisColor));
        return label;
    }

    public static JSplitPane createSplitPaneForSourceList(SourceList sourceList, JComponent component) {
        JSplitPane splitPane = new JSplitPane(
                JSplitPane.HORIZONTAL_SPLIT, sourceList.getComponent(), component);
        splitPane.setContinuousLayout(true);
        splitPane.setDividerSize(1);
        ((BasicSplitPaneUI) splitPane.getUI()).getDivider().setBorder(
                BorderFactory.createMatteBorder(0, 1, 0, 0, new Color(0xa5a5a5)));
        splitPane.setBorder(BorderFactory.createEmptyBorder());
        return splitPane;
    }

    public static JScrollPane createSourceListScrollPane(JComponent content) {
        return makeSourceListScrollPane(new JScrollPane(content));
    }

    public static JScrollPane makeSourceListScrollPane(JScrollPane scrollPane) {
        scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPane.setBorder(BorderFactory.createEmptyBorder());
        return scrollPane;
    }

}
