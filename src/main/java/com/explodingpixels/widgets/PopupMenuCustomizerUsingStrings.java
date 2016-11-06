package com.explodingpixels.widgets;

import java.awt.event.ActionListener;
import java.util.Arrays;
import java.util.List;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

public class PopupMenuCustomizerUsingStrings implements PopupMenuCustomizer {

    private final List<String> fMenuItemStrings;

    private final ActionListener fListener;

    public PopupMenuCustomizerUsingStrings(ActionListener listener,
                             String... menuItemStrings) {
        this(listener, Arrays.asList(menuItemStrings));
    }

    public PopupMenuCustomizerUsingStrings(ActionListener listener,
                             List<String> menuItemStrings) {
        fMenuItemStrings = menuItemStrings;
        fListener = listener;
    }

    public void customizePopup(JPopupMenu popup) {
        popup.removeAll();
        for (String menuString : fMenuItemStrings) {
            JMenuItem menuItem = new JMenuItem(menuString);
            menuItem.addActionListener(fListener);
            popup.add(menuItem);
        }

    }

}

