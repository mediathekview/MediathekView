package com.explodingpixels.widgets;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.MouseMotionListener;

import javax.swing.AbstractButton;
import javax.swing.Icon;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JPopupMenu;
import javax.swing.MenuSelectionManager;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import com.explodingpixels.painter.MacWidgetsPainter;
import com.explodingpixels.swingx.EPToggleButton;

public class PopdownButton {

    private final EPToggleButton fButton;

    private PopupMenuCustomizer fPopupMenuCustomizer;

    private JPopupMenu fPopupMenu = new JPopupMenu();

    public PopdownButton(Icon icon, PopupMenuCustomizer popupMenuCustomizer) {

        if (popupMenuCustomizer == null) {
            throw new IllegalArgumentException("The list of items to add to" +
                    "the popup menu cannot be null.");
        }

        fButton = new EPToggleButton(icon);
        fButton.setPressedIcon(icon);

        fPopupMenuCustomizer = popupMenuCustomizer;

        init();
    }

    private void init() {

        fButton.addMouseListener(createToggleButtonMouseListener());
        fButton.addMouseMotionListener(createToggleButtonMouseMotionListener());
        fPopupMenu.addPopupMenuListener(createPopupMenuListener());

        // this is a trick to get hold of the client property which prevents
        // closing of the popup when the button is pressed.
        JComboBox box = new JComboBox();
        Object preventHide = box.getClientProperty("doNotCancelPopup");
        fButton.putClientProperty("doNotCancelPopup", preventHide);
    }

    public void setBackgroundPainter(MacWidgetsPainter<AbstractButton> painter) {
        fButton.setBackgroundPainter(painter);
    }

    public JComponent getComponent() {
        return fButton;
    }

    public void setPressedIcon(Icon pressedIcon) {
        fButton.setPressedIcon(pressedIcon);
    }

    private void showPopupMenu() {
        fPopupMenu.pack();
        fPopupMenu.show(fButton, 0, fButton.getHeight());
    }

    private void hidePopupMenu() {
        fPopupMenu.setVisible(false);
        fButton.setSelected(false);
    }

    private MouseListener createToggleButtonMouseListener() {
        return new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                // if the button is not in the selected state, then hide the
                // popup menu. note that showing the popup menu is always
                // handled by the mousePressed method.
                if (!fButton.isSelected()) {
                    hidePopupMenu();
                }
            }

            @Override
            public void mousePressed(MouseEvent e) {
                // allways show the menu on a mouse pressed. the mouse clicked
                // event will take care of dismissing the popup menu if the
                // button has gone into an unselected state.
                showPopupMenu();
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                MenuSelectionManager manager = MenuSelectionManager.defaultManager();
                manager.processMouseEvent(e);
            }
        };
    }

    private MouseMotionListener createToggleButtonMouseMotionListener() {
        return new MouseMotionAdapter() {
            @Override
            public void mouseDragged(MouseEvent e) {
                MenuSelectionManager.defaultManager().processMouseEvent(e);
            }
        };
    }

    private PopupMenuListener createPopupMenuListener() {
        return new PopupMenuListener() {

            public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
                fPopupMenuCustomizer.customizePopup(fPopupMenu);
            }

            public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
                // make sure the button accuratley reflects the visibility of
                // the popup menu.
                fButton.setSelected(false);
            }

            public void popupMenuCanceled(PopupMenuEvent e) {
                // no implementation.
            }
        };
    }
}
