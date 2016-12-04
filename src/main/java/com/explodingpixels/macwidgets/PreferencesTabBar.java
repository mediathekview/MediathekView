package com.explodingpixels.macwidgets;

import com.explodingpixels.macwidgets.plaf.PreferencesTabBarButtonUI;
import com.explodingpixels.widgets.WindowUtils;

import javax.swing.*;
import javax.swing.border.Border;
import java.awt.*;
import java.awt.event.ActionListener;
import java.util.Enumeration;

@SuppressWarnings("serial")
public class PreferencesTabBar {
    private final TriAreaComponent fPreferencesTabBar = new TriAreaComponent();

    private final ButtonGroup fButtonGroup = new ButtonGroup();

    public PreferencesTabBar() {
        Border b = BorderFactory.createEmptyBorder(0, 4, 0, 4);
        fPreferencesTabBar.getComponent().setBorder(b);
        UnifiedToolBar.installUnifiedToolBarBorder(fPreferencesTabBar.getComponent());
        fPreferencesTabBar.setBackgroundPainter(MacPainterFactory.createTexturedWindowWorkaroundPainter());
        WindowUtils.installJComponentRepainterOnWindowFocusChanged(fPreferencesTabBar.getComponent());
    }

    public void addTab(String title, Icon icon, ActionListener listener) {
        AbstractButton button = new JToggleButton(title, icon) {
            @Override
            public void updateUI() {
                setUI(new PreferencesTabBarButtonUI());
            }
        };
        fButtonGroup.add(button);
        button.addActionListener(listener);

        fPreferencesTabBar.addComponentToLeft(button);
    }

    public void showTab(String title) {
        getButton(title).doClick();
    }

    public void installWindowDraggerOnWindow(Window window) {
        fPreferencesTabBar.installWindowDraggerOnWindow(window);
    }

    public JComponent getComponent() {
        return fPreferencesTabBar.getComponent();
    }

    private AbstractButton getButton(String title) {
        AbstractButton retVal = null;

        Enumeration<AbstractButton> buttons = fButtonGroup.getElements();
        while (buttons.hasMoreElements()) {
            AbstractButton button = buttons.nextElement();
            if (button.getText().equals(title)) {
                retVal = button;
                break;
            }
        }

        checkButtonFound(retVal);

        return retVal;
    }

    private static void checkButtonFound(AbstractButton button) {
        if (button == null) {
            throw new IllegalArgumentException(
                    "The given button title does not represent a preferences tab.");
        }
    }
}
