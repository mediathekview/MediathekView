package com.explodingpixels.macwidgets;

import java.awt.Cursor;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JSplitPane;
import javax.swing.SwingUtilities;

import com.explodingpixels.widgets.PopdownButton;
import com.explodingpixels.widgets.PopupMenuCustomizer;

/**
 * <p>
 * A bar that can contain buttons and pop-down buttons that act on a given {@link SourceList}. This
 * control bar is displayed at the base of the {@code SourceList}. The control bar also has a
 * draggable widget that can control the divider location of a given {@link JSplitPane}.
 * </p>
 * <p>
 * Heres how to create and install an empty {@code SourceListControlBar}:
 * <pre>
 * SourceList sourceList = DSourceListITunes.createSourceList();
 * SourceListControlBar controlBar = new SourceListControlBar();
 * sourceList.installSourceListControlBar(controlBar);
 * </pre>
 * The above code creates a control bar that looks like this:
 * <br><br>
 * <img src="../../../../graphics/SourceListControlBar-empty.png">
 * </p>
 * <p>
 * The following code adds two push buttons and a drop-down button to the control bar:
 * <pre>
 * controlBar.createAndAddPopdownButton(MacIcons.GEAR,
 *         new PopupMenuCustomizer() {
 *             public void customizePopup(JPopupMenu popup) {
 *                 popup.removeAll();
 *                 popup.add(new JMenuItem("Item One"));
 *                 popup.add(new JMenuItem("Item Two"));
 *                 popup.add(new JMenuItem("Item Three"));
 *             }
 *         });
 * </pre>
 * The above code creates a control bar that looks like this:
 * <br><br>
 * <img src="../../../../graphics/SourceListControlBar-buttons.png">
 * </p>
 */
public class SourceListControlBar {

    private ComponentBottomBar fComponentBottomBar = new ComponentBottomBar();

    /**
     * Creates a {@code SourceListControlBar}.
     */
    public SourceListControlBar() {
        init();
    }

    private void init() {
    }

    /**
     * Connects the draggable widget in this {@code SourceListControlBar} to the divider of the
     * given {@link JSplitPane}. Thus when the user drags the {@code SourceListControlBar} draggable
     * widget, the given {@code JSplitPane}s divider location will be adjusted.
     *
     * @param splitPane the {@code JSplitPane} to connect the draggable widget to.
     */
    public void installDraggableWidgetOnSplitPane(JSplitPane splitPane) {
        fComponentBottomBar.installDraggableWidgetOnSplitPane(splitPane);
    }

    /**
     * Gets the user interface component representing this {@code SourceListControlBar}. The
     * returned {@link JComponent} should be added to a container that will be displayed.
     *
     * @return the user interface component representing this {@code SourceListControlBar}.
     */
    public JComponent getComponent() {
        return fComponentBottomBar.getComponent();
    }

    private void addComponent(JComponent component) {
        fComponentBottomBar.addComponentToLeftWithBorder(component);
    }

    /**
     * Add a new pop-down style button. The given {@link PopupMenuCustomizer} will be called just
     * prior to each showing of the menu.
     *
     * @param icon                the icon to use in the pop-down menu.
     * @param popupMenuCustomizer the {@code PopupMenuCustomizer} to be called just prior to showing
     *                            the menu.
     */
    public void createAndAddPopdownButton(Icon icon, PopupMenuCustomizer popupMenuCustomizer) {
        PopdownButton button = MacButtonFactory.createGradientPopdownButton(
                icon, popupMenuCustomizer);
        initSourceListButton(button.getComponent());
        addComponent(button.getComponent());
    }

    /**
     * Adds a new button with the given icon. The given {@link ActionListener} will be called when
     * the button is pressed.
     *
     * @param icon           the icon to use for the button.
     * @param actionListener the {@code ActionListener} to call when the button is pressed.
     */
    public void createAndAddButton(Icon icon, ActionListener actionListener) {
        JComponent button = MacButtonFactory.createGradientButton(icon, actionListener);
        initSourceListButton(button);
        addComponent(button);
    }

    private static void initSourceListButton(JComponent component) {
        component.setBorder(BorderFactory.createEmptyBorder());
    }

    /**
     * Hides the resize handle.
     */
    public void hideResizeHandle() {
        fComponentBottomBar.hideResizeHandle();
    }
}
