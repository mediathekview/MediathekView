package com.explodingpixels.widgets.plaf;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.MouseMotionListener;

import javax.swing.ButtonGroup;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComboBox;
import javax.swing.JList;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.MenuElement;
import javax.swing.MenuSelectionManager;
import javax.swing.SwingUtilities;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.plaf.basic.ComboPopup;

/**
 * An implementation of {@link ComboPopup} that uses actual {@link JMenuItem}s rather than a
 * {@link JList} to display it's contents.
 */
public class EPComboPopup implements ComboPopup {

    private final JComboBox fComboBox;
    private JPopupMenu fPopupMenu = new JPopupMenu();
    private Font fFont;
    private ComboBoxVerticalCenterProvider fComboBoxVerticalCenterProvider =
            new DefaultVerticalCenterProvider();

    private static final int LEFT_SHIFT = 5;

    public EPComboPopup(JComboBox comboBox) {
        fComboBox = comboBox;
        fFont = comboBox.getFont();
        fPopupMenu.addPopupMenuListener(createPopupMenuListener());
    }

    /**
     * Creates a {@link PopupMenuListener} on the underlying {@link JPopupMenu} which forwards along the popup event
     * notification to the associated {@link JComboBox}.
     */
    private PopupMenuListener createPopupMenuListener() {
        return new PopupMenuListener() {
            public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
                fComboBox.firePopupMenuWillBecomeVisible();
            }

            public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
                fComboBox.firePopupMenuWillBecomeInvisible();
            }

            public void popupMenuCanceled(PopupMenuEvent e) {
                fComboBox.firePopupMenuCanceled();
            }
        };
    }

    public void setFont(Font font) {
        fFont = font;
    }

    public void setVerticalComponentCenterProvider(
            ComboBoxVerticalCenterProvider comboBoxVerticalCenterProvider) {
        if (comboBoxVerticalCenterProvider == null) {
            throw new IllegalArgumentException("The given CompnonentCenterProvider cannot be null.");
        }
        fComboBoxVerticalCenterProvider = comboBoxVerticalCenterProvider;
    }

    private void togglePopup() {
        if (isVisible()) {
            hide();
        } else {
            show();
        }
    }

    private void clearAndFillMenu() {
        fPopupMenu.removeAll();

        ButtonGroup buttonGroup = new ButtonGroup();

        // add the given items to the popup menu.
        for (int i = 0; i < fComboBox.getModel().getSize(); i++) {
            Object item = fComboBox.getModel().getElementAt(i);
            JMenuItem menuItem = new JCheckBoxMenuItem(item.toString());
            menuItem.setFont(fFont);
            menuItem.addActionListener(createMenuItemListener(item));
            buttonGroup.add(menuItem);
            fPopupMenu.add(menuItem);

            // if the current item is selected, make the menu item reflect that.
            if (item.equals(fComboBox.getModel().getSelectedItem())) {
                menuItem.setSelected(true);
                fPopupMenu.setSelected(menuItem);
            }
        }

        fPopupMenu.pack();
        int popupWidth = fComboBox.getWidth() + 5;
        // adjust the width to be slightly wider than the associated combo box.
        fPopupMenu.setSize(popupWidth, fPopupMenu.getHeight());
    }

    private Point placePopupOnScreen() {
        // grab the right most location of the button.
        int buttonRightX = fComboBox.getWidth();

        // figure out how the height of a menu item.
        Insets insets = fPopupMenu.getInsets();

        // calculate the x and y value at which to place the popup menu.
        // by default, this will place the selected menu item in the
        // popup item directly over the button.
        int x = buttonRightX - fPopupMenu.getPreferredSize().width - LEFT_SHIFT;
        int selectedItemIndex = fPopupMenu.getSelectionModel().getSelectedIndex();
        int componentCenter = fComboBoxVerticalCenterProvider.provideCenter(fComboBox);
        int menuItemHeight = fPopupMenu.getComponent(selectedItemIndex).getPreferredSize().height;
        int menuItemCenter = insets.top + (selectedItemIndex * menuItemHeight) + menuItemHeight / 2;
        int y = componentCenter - menuItemCenter;

        // do a cursory check to make sure we're not placing the popup
        // off the bottom of the screen. note that Java on Mac won't
        // let the popup show up off screen no matter where you place it.
        Dimension size = Toolkit.getDefaultToolkit().getScreenSize();
        Point bottomOfMenuOnScreen = new Point(0, y + fPopupMenu.getPreferredSize().height);
        SwingUtilities.convertPointToScreen(bottomOfMenuOnScreen, fComboBox);
        if (bottomOfMenuOnScreen.y > size.height) {
            y = fComboBox.getHeight() - fPopupMenu.getPreferredSize().height;
        }

        return new Point(x, y);
    }

    private ActionListener createMenuItemListener(final Object comboBoxItem) {
        return new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                fComboBox.setSelectedItem(comboBoxItem);
            }
        };
    }

    private void forceCorrectPopupSelectionIfNeccessary() {
        if (fComboBox.getSelectedIndex() >= 0) {
            forceCorrectPopupSelection();
        }
    }

    private void forceCorrectPopupSelection() {
        assert fPopupMenu.isShowing() : "The popup must be showing for this method to work properly.";

        // force the correct item to be shown as selected. this is a
        // work around for Java bug 4740942, which has been fixed by
        // Sun, but not by Apple.
        int index = fPopupMenu.getSelectionModel().getSelectedIndex();
        MenuElement[] menuPath = new MenuElement[2];
        menuPath[0] = fPopupMenu;
        menuPath[1] = fPopupMenu.getSubElements()[index];
        MenuSelectionManager.defaultManager().setSelectedPath(menuPath);
    }

    // ComboPopup implementation. /////////////////////////////////////////////////////////////////

    public void show() {
        clearAndFillMenu();
        // if there are combo box items, then show the popup menu.
        if (fComboBox.getModel().getSize() > 0) {
//        Point popupLocation = placePopupOnScreen();
            Rectangle popupBounds = calculateInitialPopupBounds();

//        fPopupMenu.show(fComboBox, popupLocation.x, popupLocation.y);
            fPopupMenu.show(fComboBox, popupBounds.x, popupBounds.y);
            forceCorrectPopupSelectionIfNeccessary();
        }
    }

    public void hide() {
        fPopupMenu.setVisible(false);
    }

    public boolean isVisible() {
        return fPopupMenu.isVisible();
    }

    /**
     * This method is not implemented and would throw an {@link UnsupportedOperationException} if
     * {@link javax.swing.plaf.basic.BasicComboBoxUI} didn't call it. Thus, this method should not
     * be used, as it always returns null.
     *
     * @return null.
     */
    public JList getList() {
        return null;
    }

    public MouseListener getMouseListener() {
        return new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent e) {
                if (fComboBox.isEnabled()) {
                    togglePopup();
                }
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                MenuSelectionManager.defaultManager().processMouseEvent(e);
            }
        };
    }

    public MouseMotionListener getMouseMotionListener() {
        return new MouseMotionAdapter() {
            @Override
            public void mouseDragged(MouseEvent e) {
                MenuSelectionManager.defaultManager().processMouseEvent(e);
            }
        };
    }

    public KeyListener getKeyListener() {
        return null;
    }

    public void uninstallingUI() {
        // TODO implement, if necessary.
    }

    // An interface to allow a third-party to provide the center of a given compoennt. ////////////

    public interface ComboBoxVerticalCenterProvider {
        int provideCenter(JComboBox comboBox);
    }

    // A default implementation of ComboBoxVerticalCenterProvider. ////////////////////////////////

    private static class DefaultVerticalCenterProvider implements ComboBoxVerticalCenterProvider {
        public int provideCenter(JComboBox comboBox) {
            return comboBox.getHeight() / 2;
        }
    }

    // Utility methods. ///////////////////////////////////////////////////////////////////////////

    private Rectangle calculateInitialPopupBounds() {
        // grab the right most location of the button.
        int comboBoxRightEdge = fComboBox.getWidth();

        // figure out how the height of a menu item.
        Insets insets = fPopupMenu.getInsets();

        // calculate the x and y value at which to place the popup menu. by default, this will place
        // the selected menu item in the popup item directly over the button.
        int x = comboBoxRightEdge - fPopupMenu.getPreferredSize().width - LEFT_SHIFT;
        int selectedItemIndex = fPopupMenu.getSelectionModel().getSelectedIndex();
        int componentCenter = fComboBoxVerticalCenterProvider.provideCenter(fComboBox);
        int menuItemHeight = fPopupMenu.getSelectionModel().getSelectedIndex() >= 0
                ? fPopupMenu.getComponent(selectedItemIndex).getPreferredSize().height
                : 0;
        int menuItemCenter = insets.top + (selectedItemIndex * menuItemHeight) + menuItemHeight / 2;
        int y = componentCenter - menuItemCenter;

        // TODO this method doesn't robustly handle multiple monitors.

        Rectangle bounds = new Rectangle(new Point(x, y), fPopupMenu.getPreferredSize());

        Dimension preferredSize = fPopupMenu.getPreferredSize();

        // do a cursory check to make sure we're not placing the popup
        // off the bottom of the screen. note that Java on Mac won't
        // let the popup show up off screen no matter where you place it.
        Dimension size = Toolkit.getDefaultToolkit().getScreenSize();
        Point bottomOfMenuOnScreen = new Point(0, y + preferredSize.height);
        SwingUtilities.convertPointToScreen(bottomOfMenuOnScreen, fComboBox);
        if (bottomOfMenuOnScreen.y > size.height) {
            y = fComboBox.getHeight() - preferredSize.height;
        }

        Point position = new Point(x, y);

        return bounds;
    }
}
