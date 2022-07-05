/*
 * $Id: BusyLabelAddon.java 2565 2008-01-03 19:08:32Z rah003 $
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
package org.jdesktop.swingx.plaf;

import org.jdesktop.swingx.JXList;

import javax.swing.*;
import javax.swing.border.AbstractBorder;
import javax.swing.border.Border;
import javax.swing.plaf.UIResource;

/**
 * Addon for <code>JXList</code>.
 * <p>
 * 
 * Install a custom ui to support sorting/filtering in JXList.
 */
public class XListAddon extends AbstractComponentAddon {

    public XListAddon() {
        super("JXList");
    }

    @Override
    protected void addBasicDefaults(LookAndFeelAddons addon,
            DefaultsList defaults) {
        defaults.add(JXList.uiClassID,
                "org.jdesktop.swingx.plaf.basic.core.BasicXListUI");
        if (isGTK()) {
            replaceListTableBorders(addon, defaults);
        }
    }

    @Override
    protected void addNimbusDefaults(LookAndFeelAddons addon,
            DefaultsList defaults) {
        defaults.add(JXList.uiClassID,
                "org.jdesktop.swingx.plaf.synth.SynthXListUI");
    }
    

    private void replaceListTableBorders(LookAndFeelAddons addon,
            DefaultsList defaults) {
        replaceBorder(defaults, "List.", "focusCellHighlightBorder");
        replaceBorder(defaults, "List.", "focusSelectedCellHighlightBorder");
        replaceBorder(defaults, "List.", "noFocusBorder");
    }



    /**
     * @param defaults
     * @param componentPrefix
     * @param borderKey
     */
    private void replaceBorder(DefaultsList defaults, String componentPrefix,
            String borderKey) {
        String key = componentPrefix + borderKey;
        Border border = UIManager.getBorder(componentPrefix + borderKey);
        if (border instanceof AbstractBorder && border instanceof UIResource
                && border.getClass().getName().contains("ListTable")) {
            border = new SafeBorder((AbstractBorder) border);
            // PENDING JW: this is fishy ... adding to lookAndFeelDefaults is taken
            UIManager.getLookAndFeelDefaults().put(key, border);
            // adding to defaults is not
//            defaults.add(key, border);
            
        }
    }



    /**
     * 
     * @return true if the LF is GTK.
     */
    private boolean isGTK() {
        return "GTK".equals(UIManager.getLookAndFeel().getID());
    }


}
