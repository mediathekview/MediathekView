/*
 * $Id: TreeRolloverProducer.java 4190 2012-06-27 19:01:06Z kschaefe $
 *
 * Copyright 2007 Sun Microsystems, Inc., 4150 Network Circle,
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
 *
 */
package org.jdesktop.swingx.rollover;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseEvent;

/**
 * Tree-specific implementation of RolloverProducer.
 * <p>
 * This implementation assumes a "hit" for rollover if the mouse is anywhere in
 * the total width of the tree. Additionally, a pressed to the right (but
 * outside of the label bounds) is re-dispatched as a pressed just inside the
 * label bounds. This is a first go for #166-swingx.
 * <p>
 * 
 * PENDING JW: bidi-compliance of pressed?
 * 
 * @author Jeanette Winzenburg
 */
public class TreeRolloverProducer extends RolloverProducer {

    @Override
    public void mousePressed(MouseEvent e) {
        JTree tree = (JTree) e.getComponent();
        Point mousePoint = e.getPoint();
        int labelRow = tree.getRowForLocation(mousePoint.x, mousePoint.y);
        // default selection
        if (labelRow >= 0)
            return;
        int row = tree.getClosestRowForLocation(mousePoint.x, mousePoint.y);
        Rectangle bounds = tree.getRowBounds(row);
        if (bounds == null) {
            row = -1;
        } else {
            if ((bounds.y + bounds.height < mousePoint.y)
                    || bounds.x > mousePoint.x) {
                row = -1;
            }
        }
        // no hit
        if (row < 0)
            return;
        tree.dispatchEvent(new MouseEvent(tree, e.getID(), e.getWhen(), e
                .getModifiers(), bounds.x + bounds.width - 2, mousePoint.y, e
                .getClickCount(), e.isPopupTrigger(), e.getButton()));
    }

    @Override
    protected void updateRolloverPoint(JComponent component, Point mousePoint) {
        JTree tree = (JTree) component;
        int row = tree.getClosestRowForLocation(mousePoint.x, mousePoint.y);
        Rectangle bounds = tree.getRowBounds(row);
        if (bounds == null) {
            row = -1;
        } else {
            if ((bounds.y + bounds.height < mousePoint.y)
                    || bounds.x > mousePoint.x) {
                row = -1;
            }
        }
        int col = row < 0 ? -1 : 0;
        rollover.x = col;
        rollover.y = row;
    }

}
