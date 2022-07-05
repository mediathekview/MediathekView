/*
 * $Id: WindowsClassicStatusBarUI.java 3472 2009-08-27 13:12:42Z kleopatra $
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

package org.jdesktop.swingx.plaf.windows;

import org.jdesktop.swingx.JXStatusBar;
import org.jdesktop.swingx.plaf.basic.BasicStatusBarUI;

import javax.swing.*;
import javax.swing.border.BevelBorder;
import javax.swing.border.Border;
import javax.swing.plaf.BorderUIResource;
import javax.swing.plaf.ComponentUI;
import java.awt.*;

/**
 *
 * @author rbair
 */
public class WindowsClassicStatusBarUI extends BasicStatusBarUI {
    /** Creates a new instance of BasicStatusBarUI */
    public WindowsClassicStatusBarUI() {
    }
    
    /**
     * Returns an instance of the UI delegate for the specified component.
     * Each subclass must provide its own static <code>createUI</code>
     * method that returns an instance of that UI delegate subclass.
     * If the UI delegate subclass is stateless, it may return an instance
     * that is shared by multiple components.  If the UI delegate is
     * stateful, then it should return a new instance per component.
     * The default implementation of this method throws an error, as it
     * should never be invoked.
     */
    public static ComponentUI createUI(JComponent c) {
        return new WindowsClassicStatusBarUI();
    }
    
    @Override protected void paintBackground(Graphics2D g, JXStatusBar bar) {        
        g.setColor(bar.getBackground());
        g.fillRect(0, 0, bar.getWidth(), bar.getHeight());
        
        //paint an inset border around each component. This suggests that
        //there is an extra border around the status bar...!
        Border b = BorderFactory.createBevelBorder(BevelBorder.LOWERED, 
                Color.WHITE, bar.getBackground(), bar.getBackground(), Color.GRAY);
        Insets insets = new Insets(0, 0, 0, 0);
        for (Component c : bar.getComponents()) {
            getSeparatorInsets(insets);
            int x = c.getX() - insets.right;
            int y = c.getY() - 2;
            int w = c.getWidth() + insets.left + insets.right;
            int h = c.getHeight() + 4;
            b.paintBorder(c, g, x, y, w, h);
        }
    }
    
    @Override protected void paintSeparator(Graphics2D g, JXStatusBar bar, int x, int y, int w, int h) {
        //paint nothing, since paintBackground handles this
    }

    @Override protected int getSeparatorWidth() {
        return 11;
    }

    @Override protected BorderUIResource createBorder() {
        return new BorderUIResource(BorderFactory.createEmptyBorder(4, 5, 3, 22));
    }
}
