/*
 * $Id: DefaultXTreeCellRenderer.java 3304 2009-03-20 15:11:25Z kleopatra $
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
package org.jdesktop.swingx.tree;

import org.jdesktop.swingx.SwingXUtilities;

import javax.swing.*;
import javax.swing.tree.DefaultTreeCellRenderer;

/**
 * Quick fix for #1061-swingx (which actually is a core issue): 
 * tree icons lost on toggle laf. Updates colors as well -
 * but beware: this is incomplete as some of super's properties are private!
 * 
 * Will not do more because in the longer run (as soon as we've fixed the editor issues) 
 * the JXTree's default renderer will be changed to SwingX DefaultTreeRenderer.
 * 
 * @author Jeanette Winzenburg
 */
public class DefaultXTreeCellRenderer extends DefaultTreeCellRenderer {

    /**
     * {@inheritDoc} <p>
     * 
     * Overridden to update icons and colors.
     */
    @Override
    public void updateUI() {
        super.updateUI();
        updateIcons();
        updateColors();
    }

    /**
     * 
     */
    protected void updateColors() {
        if (SwingXUtilities.isUIInstallable(getTextSelectionColor())) {
            setTextSelectionColor(UIManager.getColor("Tree.selectionForeground"));
        }
        if (SwingXUtilities.isUIInstallable(getTextNonSelectionColor())) {
            setTextNonSelectionColor(UIManager.getColor("Tree.textForeground"));
        }
        if (SwingXUtilities.isUIInstallable(getBackgroundSelectionColor())) {
            setBackgroundSelectionColor(UIManager.getColor("Tree.selectionBackground"));
        }
        if (SwingXUtilities.isUIInstallable(getBackgroundNonSelectionColor())) {
            setBackgroundNonSelectionColor(UIManager.getColor("Tree.textBackground"));
        }
        if (SwingXUtilities.isUIInstallable(getBorderSelectionColor())) {
            setBorderSelectionColor(UIManager.getColor("Tree.selectionBorderColor"));
        }
//        Object value = UIManager.get("Tree.drawsFocusBorderAroundIcon");
//        drawsFocusBorderAroundIcon = (value != null && ((Boolean)value).
//                                      booleanValue());
//        value = UIManager.get("Tree.drawDashedFocusIndicator");
//        drawDashedFocusIndicator = (value != null && ((Boolean)value).
//                                    booleanValue());
    }

    /**
     * 
     */
    protected void updateIcons() {
        if (SwingXUtilities.isUIInstallable(getLeafIcon())) {
            setLeafIcon(UIManager.getIcon("Tree.leafIcon"));
        }
        if (SwingXUtilities.isUIInstallable(getClosedIcon())) {
            setClosedIcon(UIManager.getIcon("Tree.closedIcon"));
        }
        if (SwingXUtilities.isUIInstallable(getOpenIcon())) {
            setOpenIcon(UIManager.getIcon("Tree.openIcon"));
        }

    }
    
    

}
