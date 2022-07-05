/*
 * $Id: TreeRolloverController.java 3100 2008-10-14 22:33:10Z rah003 $
 *
 * Copyright 2008 Sun Microsystems, Inc., 4150 Network Circle,
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
package org.jdesktop.swingx.rollover;

import javax.swing.*;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreePath;
import java.awt.*;


/**
     * listens to rollover properties. 
     * Repaints effected component regions.
     * Updates link cursor.
     * 
     * @author Jeanette Winzenburg
     */
    public class TreeRolloverController<T extends JTree>  extends RolloverController<T> {
    
        private Cursor oldCursor;
        
//    -------------------------------------JTree rollover
        
        @Override
        protected void rollover(Point oldLocation, Point newLocation) {
            // JW: conditional repaint not working?
//            component.repaint();
            if (oldLocation != null) {
                Rectangle r = component.getRowBounds(oldLocation.y);
                if (r != null) {
                    r.x = 0;
                    r.width = component.getWidth();
                    component.repaint(r);
                }
            }
            if (newLocation != null) {
                Rectangle r = component.getRowBounds(newLocation.y);
                if (r != null) {
                    r.x = 0;
                    r.width = component.getWidth();
                    component.repaint(r);
                }
            }
            setRolloverCursor(newLocation);
        }


        private void setRolloverCursor(Point location) {
            if (hasRollover(location)) {
                if (oldCursor == null) {
                    oldCursor = component.getCursor();
                    component.setCursor(Cursor
                            .getPredefinedCursor(Cursor.HAND_CURSOR));
                }
            } else {
                if (oldCursor != null) {
                    component.setCursor(oldCursor);
                    oldCursor = null;
                }
            }

        }


        @Override
        protected RolloverRenderer getRolloverRenderer(Point location, boolean prepare) {
            TreeCellRenderer renderer = component.getCellRenderer();
            RolloverRenderer rollover = renderer instanceof RolloverRenderer 
                ? (RolloverRenderer) renderer : null;
            if ((rollover != null) && !rollover.isEnabled()) {
                rollover = null;
            }
            if ((rollover != null) && prepare) {
                TreePath path = component.getPathForRow(location.y);
                Object element = path != null ? path.getLastPathComponent() : null;
                renderer.getTreeCellRendererComponent(component, element, false, 
                        false, false, 
                        location.y, false);
            }
            return rollover;
        }


        @Override
        protected Point getFocusedCell() {
            // TODO Auto-generated method stub
            return null;
        }

    }