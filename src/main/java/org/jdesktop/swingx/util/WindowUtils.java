/*
 * $Id: WindowUtils.java 4084 2011-11-15 18:53:49Z kschaefe $
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

package org.jdesktop.swingx.util;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;

import static java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment;

/**
 * Encapsulates various utilities for windows (ie: <code>Frame</code> and
 * <code>Dialog</code> objects and descendants, in particular).
 *
 * @author Richard Bair
 */
public final class WindowUtils {
    /**
     * Hide the constructor - don't wan't anybody creating an instance of this
     */
    private WindowUtils() {
    }
    
    private static GraphicsConfiguration getDefaultGraphicsConfiguration() {
        return getLocalGraphicsEnvironment().getDefaultScreenDevice().getDefaultConfiguration();
    }

    private static boolean isUnowned(Window window) {
        return window.getOwner() == null || (window instanceof JDialog && JOptionPane.getRootFrame().equals(window.getOwner()));
    }
    
    private static Rectangle getUsableDeviceBounds(GraphicsConfiguration gc) {
        Rectangle bounds = gc.getBounds();
        Insets insets = Toolkit.getDefaultToolkit().getScreenInsets(gc);
        
        bounds.x += insets.left;
        bounds.y += insets.top;
        bounds.width -= (insets.left + insets.right);
        bounds.height -= (insets.top + insets.bottom);
        
        return bounds;
    }
    
    /**
     * <p>
     * Returns the <code>Point</code> at which a window should be placed to
     * center that window on the screen.
     * </p>
     * <p>
     * Some thought was taken as to whether to implement a method such as this,
     * or to simply make a method that, given a window, will center it.  It was
     * decided that it is better to not alter an object within a method.
     * </p>
     *
     * @param window The window to calculate the center point for.  This object
     *               can not be null.
     *
     * @return the <code>Point</code> at which the window should be placed to
     *         center that window on the screen.
     */
    public static Point getPointForCentering(Window window) {
        Window w = window.isShowing() || isUnowned(window) ? window : window.getOwner();
        GraphicsConfiguration gc = w.getGraphicsConfiguration();
        
        Rectangle usableBounds = getUsableDeviceBounds(gc);
        int screenWidth = usableBounds.width;
        int screenHeight = usableBounds.height;
        int width = window.getWidth();
        int height = window.getHeight();
        
        return new Point(((screenWidth - width) / 2) + usableBounds.x,
                ((screenHeight - height) / 2) + usableBounds.y);
    }
    
    /**
     * <p/>
     * Returns the <code>Point</code> at which a window should be placed to
     * center that window on the given desktop.
     * </p>
     * <p/>
     * Some thought was taken as to whether to implement a method such as this,
     * or to simply make a method that, given a window, will center it.  It was
     * decided that it is better to not alter an object within a method.
     * </p>
     *
     * @param window  The window (JInternalFrame) to calculate the center point
     *                for.  This object can not be null.
     *
     * @return the <code>Point</code> at which the window should be placed to
     *         center that window on the given desktop
     */
    public static Point getPointForCentering(JInternalFrame window) {
        Window w = SwingUtilities.getWindowAncestor(window);
        GraphicsConfiguration gc = w == null ? getDefaultGraphicsConfiguration()
                : w.getGraphicsConfiguration();
        
        Rectangle usableBounds = getUsableDeviceBounds(gc);
        int screenWidth = usableBounds.width;
        int screenHeight = usableBounds.height;
        int width = window.getWidth();
        int height = window.getHeight();
        
        return new Point(((screenWidth - width) / 2) + usableBounds.x,
                ((screenHeight - height) / 2) + usableBounds.y);
    }

    /**
     * <p/>
     * Returns the <code>Point</code> at which a window should be placed in
     * order to be staggered slightly from another &quot;origin&quot; window to
     * ensure that the title areas of both windows remain visible to the user.
     * </p>
     *
     * @param originWindow Window from which the staggered location will be calculated
     *
     * @return location staggered from the upper left location of the origin
     *         window
     */
    public static Point getPointForStaggering(Window originWindow) {
        Point origin = originWindow.getLocation();
        Insets insets = originWindow.getInsets();
        origin.x += insets.top;
        origin.y += insets.top;
        return origin;
    }

    public static Window findWindow(Component c) {
        if (c == null) {
            return JOptionPane.getRootFrame();
        } else if (c instanceof Window) {
            return (Window) c;
        } else {
            return findWindow(c.getParent());
        }
    }

    public static List<Component> getAllComponents(final Container c) {
        Component[] comps = c.getComponents();
        List<Component> compList = new ArrayList<Component>();
        for (Component comp : comps) {
            compList.add(comp);
            if (comp instanceof Container) {
                compList.addAll(getAllComponents((Container) comp));
            }
        }
        return compList;
    }
}
