package com.explodingpixels.macwidgets;

import java.awt.Component;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JScrollPane;

import com.explodingpixels.macwidgets.plaf.IAppScrollBarUI;
import com.explodingpixels.widgets.ImageBasedJComponent;

/**
 * A factory for iApp style widgets.
 */
public class IAppWidgetFactory {

    private IAppWidgetFactory() {
        // utility class - no constructor needed.
    }

    /**
     * Creates an iApp style {@link JScrollPane}, with vertical and horizontal scrollbars shown as
     * needed. The increment/decrement buttons will be placed together or separatebased on the value
     * of {@link IAppScrollBarUI#areButtonsSeparate()}.
     *
     * @param view the view to wrap inside the {@code JScrollPane}.
     * @return an iApp style {@code JScrollPane}.
     * @see #makeIAppScrollPane
     */
    public static JScrollPane createScrollPane(Component view) {
        return createScrollPane(view, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    }

    /**
     * Creates an iApp style {@link JScrollPane} using the given scroll bar policies. The
     * increment/decrement buttons will be placed together or separatebased on the value of
     * {@link IAppScrollBarUI#areButtonsSeparate()}.
     *
     * @param view                      the view to wrap inside the {@code JScrollPane}.
     * @param verticalScrollBarPolicy   the vertical scroll bar policy.
     * @param horizontalScrollBarPolicy the horizontal scroll bar policy.
     * @return an iApp style {@code JScrollPane} using the given scroll bar policies.
     * @see #makeIAppScrollPane
     */
    public static JScrollPane createScrollPane(Component view, int verticalScrollBarPolicy,
                                               int horizontalScrollBarPolicy) {
        JScrollPane retVal = new JScrollPane(view, verticalScrollBarPolicy, horizontalScrollBarPolicy);
        makeIAppScrollPane(retVal);
        return retVal;
    }

    /**
     * Makes the given {@link JScrollPane} an iApp style scroll pane that looks like this:
     * <br>
     * <img src="../../../../graphics/iAppScrollbars.png">
     *
     * @param scrollPane the {@code JScrollPane} to make an iApp style scroll pane.
     * @return an iApp style scroll pane.
     */
    public static JScrollPane makeIAppScrollPane(JScrollPane scrollPane) {
        scrollPane.setBorder(BorderFactory.createEmptyBorder());
        installUIDelegates(scrollPane);
        scrollPane.setCorner(JScrollPane.LOWER_RIGHT_CORNER, createScrollPaneCorner());
        scrollPane.setCorner(JScrollPane.LOWER_LEFT_CORNER, createScrollPaneCornerLowerLeft());
        // TODO listen for scrollBar.setUI calls in order to reinstall UI delegates.
        return scrollPane;
    }

    /**
     * Sets the default "buttons separate" status for scroll bars. The default value is
     * {@code false}, meaning that the buttons will be placed together at the right or bottom of the
     * scroll bar. A value of {@code true} means that the buttons will be placed at opposite ends of
     * the scroll bar.
     *
     * @param buttonsApart the "buttons apart" status.
     */
    public static void setIAppScrollBarButtonsSeparate(boolean buttonsApart) {
        IAppScrollBarUI.setButtonsSeparate(buttonsApart);
    }

    // ScrollBarUI creation methods ///////////////////////////////////////////////////////////////

    private static void installUIDelegates(JScrollPane scrollPane) {
        scrollPane.getVerticalScrollBar().setUI(new IAppScrollBarUI());
        scrollPane.getHorizontalScrollBar().setUI(new IAppScrollBarUI());
    }

    /**
     * Creates an iApp style scrollpane corner.
     *
     * @return returns a {@link JComponent} that represents the scroll pane corner.
     */
    public static JComponent createScrollPaneCorner() {
        return new ImageBasedJComponent(new ImageIcon(IAppWidgetFactory.class.getResource(
                "/com/explodingpixels/macwidgets/images/iapp_scrollpane_corner.png")).getImage());
    }
    
    /**
     * Creates an iApp style scrollpane corner.
     *
     * @return returns a {@link JComponent} that represents the scroll pane corner.
     */
    public static JComponent createScrollPaneCornerLowerLeft() {
        return new ImageBasedJComponent(new ImageIcon(IAppWidgetFactory.class.getResource(
                "/com/explodingpixels/macwidgets/images/iapp_scrollpane_corner_left.png")).getImage());
    }
}
