/*
 * $Id$
 *
 * Copyright 2009 Sun Microsystems, Inc., 4150 Network Circle,
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
package org.jdesktop.swingx.plaf;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.BorderUIResource;
import javax.swing.plaf.metal.MetalBorders;

/**
 * Addon for JXTableHeader.
 * 
 * Implemented to hack around core issue ??: Metal header renderer appears squeezed.
 * 
 * @author Jeanette Winzenburg
 */
public class TableHeaderAddon extends AbstractComponentAddon {

    /**
     * @param name
     */
    public TableHeaderAddon() {
        super("JXTableHeader");
    }

    @Override
    protected void addMetalDefaults(LookAndFeelAddons addon,
            DefaultsList defaults) {
        super.addMetalDefaults(addon, defaults);
        String key = "TableHeader.cellBorder";
        Border border = UIManager.getBorder(key);
        if (border instanceof MetalBorders.TableHeaderBorder) {
            border = new BorderUIResource.CompoundBorderUIResource(border, 
                    BorderFactory.createEmptyBorder());
            // PENDING JW: this is fishy ... adding to lookAndFeelDefaults is taken
            UIManager.getLookAndFeelDefaults().put(key, border);
            // adding to defaults is not
//            defaults.add(key, border);
        }
        
    }
    
    

}
