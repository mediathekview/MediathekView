/*
 * $Id: MacOSXPopupLocationFix.java 4019 2011-05-11 16:52:30Z kschaefe $
 *
 * Copyright 2004 Sun Microsystems, Inc., 4150 Network Circle,
 * Santa Clara, California 95054, U.S.A. All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
package org.jdesktop.swingx.autocomplete.workarounds;

import javax.swing.*;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import java.awt.*;

/**
 * Fix a problem where the JComboBox's popup obscures its editor in the Mac OS X
 * Aqua look and feel.
 *
 * <p>Installing this fix will resolve the problem for Aqua without having
 * side-effects for other look-and-feels. It also supports dynamically changed
 * look and feels.
 *
 * @see <a href="https://glazedlists.dev.java.net/issues/show_bug.cgi?id=332">Glazed Lists bug entry</a>
 * @see <a href="https://swingx.dev.java.net/issues/show_bug.cgi?id=360">SwingX bug entry</a>
 *
 * @author <a href="mailto:jesse@swank.ca">Jesse Wilson</a>
 */
public final class MacOSXPopupLocationFix {
    
    /** the components being fixed */
    private final JComboBox comboBox;
    private final JPopupMenu popupMenu;
    
    /** the listener provides callbacks as necessary */
    private final Listener listener = new Listener();
    
    /**
     * Private constructor so users use the more action-oriented
     * {@link #install} method.
     */
    private MacOSXPopupLocationFix(JComboBox comboBox) {
        this.comboBox = comboBox;
        this.popupMenu = (JPopupMenu)comboBox.getUI().getAccessibleChild(comboBox, 0);
        
        popupMenu.addPopupMenuListener(listener);
    }
    
    /**
     * Install the fix for the specified combo box.
     */
    public static MacOSXPopupLocationFix install(JComboBox comboBox) {
        if(comboBox == null) throw new IllegalArgumentException();
        return new MacOSXPopupLocationFix(comboBox);
    }
    
    /**
     * Uninstall the fix. Usually this is unnecessary since letting the combo
     * box go out of scope is sufficient.
     */
    public void uninstall() {
        popupMenu.removePopupMenuListener(listener);
    }
    
    /**
     * Reposition the popup immediately before it is shown.
     */
    private class Listener implements PopupMenuListener {
        @Override
        public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
            final JComponent popupComponent = (JComponent) e.getSource();
            fixPopupLocation(popupComponent);
        }
        @Override
        public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
            // do nothing
        }
        @Override
        public void popupMenuCanceled(PopupMenuEvent e) {
            // do nothing
        }
    }
    
    /**
     * Do the adjustment on the specified popupComponent immediately before
     * it is displayed.
     */
    private void fixPopupLocation(JComponent popupComponent) {
        // we only need to fix Apple's aqua look and feel
        if(popupComponent.getClass().getName().indexOf("apple.laf") != 0) {
            return;
        }
        
        // put the popup right under the combo box so it looks like a
        // normal Aqua combo box
        Point comboLocationOnScreen = comboBox.getLocationOnScreen();
        int comboHeight = comboBox.getHeight();
        int popupY = comboLocationOnScreen.y + comboHeight;
        
        // ...unless the popup overflows the screen, in which case we put it
        // above the combobox
        Rectangle screenBounds = new ScreenGeometry(comboBox).getScreenBounds();
        int popupHeight = popupComponent.getPreferredSize().height;
        if(comboLocationOnScreen.y + comboHeight + popupHeight > screenBounds.x + screenBounds.height) {
            popupY = comboLocationOnScreen.y - popupHeight;
        }
        
        popupComponent.setLocation(comboLocationOnScreen.x, popupY);
    }
    
    /**
     * Figure out the dimensions of our screen.
     *
     * <p>This code is inspired by similar in
     * <code>JPopupMenu.adjustPopupLocationToFitScreen()</code>.
     *
     * @author <a href="mailto:jesse@swank.ca">Jesse Wilson</a>
     */
    private final static class ScreenGeometry {
        
        final GraphicsConfiguration graphicsConfiguration;
        final boolean aqua;
        
        public ScreenGeometry(JComponent component) {
            this.aqua = UIManager.getLookAndFeel().getName().indexOf("Aqua") != -1;
            this.graphicsConfiguration = graphicsConfigurationForComponent(component);
        }
        
        /**
         * Get the best graphics configuration for the specified point and component.
         */
        private GraphicsConfiguration graphicsConfigurationForComponent(Component component) {
            Point point = component.getLocationOnScreen();
            
            // try to find the graphics configuration for our point of interest
            GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
            GraphicsDevice[] gd = ge.getScreenDevices();
            for(int i = 0; i < gd.length; i++) {
                if(gd[i].getType() != GraphicsDevice.TYPE_RASTER_SCREEN) continue;
                GraphicsConfiguration defaultGraphicsConfiguration = gd[i].getDefaultConfiguration();
                if(!defaultGraphicsConfiguration.getBounds().contains(point)) continue;
                return defaultGraphicsConfiguration;
            }
            
            // we couldn't find a graphics configuration, use the component's
            return component.getGraphicsConfiguration();
        }
        
        /**
         * Get the bounds of where we can put a popup.
         */
        public Rectangle getScreenBounds() {
            Rectangle screenSize = getScreenSize();
            Insets screenInsets = getScreenInsets();
            
            return new Rectangle(
                    screenSize.x + screenInsets.left,
                    screenSize.y + screenInsets.top,
                    screenSize.width - screenInsets.left - screenInsets.right,
                    screenSize.height - screenInsets.top - screenInsets.bottom
                    );
        }
        
        /**
         * Get the bounds of the screen currently displaying the component.
         */
        public Rectangle getScreenSize() {
            // get the screen bounds and insets via the graphics configuration
            if(graphicsConfiguration != null) {
                return graphicsConfiguration.getBounds();
            }
            
            // just use the toolkit bounds, it's less awesome but sufficient
            return new Rectangle(Toolkit.getDefaultToolkit().getScreenSize());
        }
        
        /**
         * Fetch the screen insets, the off limits areas around the screen such
         * as menu bar, dock or start bar.
         */
        public Insets getScreenInsets() {
            Insets screenInsets;
            if(graphicsConfiguration != null) {
                screenInsets = Toolkit.getDefaultToolkit().getScreenInsets(graphicsConfiguration);
            } else {
                screenInsets = new Insets(0, 0, 0, 0);
            }
            
            // tweak the insets for aqua, they're reported incorrectly there
            if(aqua) {
                int aquaBottomInsets = 21; // unreported insets, shown in screenshot, https://glazedlists.dev.java.net/issues/show_bug.cgi?id=332
                int aquaTopInsets = 22; // for Apple menu bar, found via debugger
                
                screenInsets.bottom = Math.max(screenInsets.bottom, aquaBottomInsets);
                screenInsets.top = Math.max(screenInsets.top, aquaTopInsets);
            }
            
            return screenInsets;
        }
    }
}