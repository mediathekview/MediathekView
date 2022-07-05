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

import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import java.awt.*;

/**
 * Collection of helpers. Could move to LookAndFeelAddon?
 * 
 * @author Jeanette Winzenburg
 */
public class LookAndFeelUtils {

    /**
     * Returns the ui that is of type <code>klass</code>, or null if
     * one can not be found.
     */
    public static Object getUIOfType(ComponentUI ui, Class<?> klass) {
        if (klass.isInstance(ui)) {
            return ui;
        }
        return null;
    }
    
    /**
     * Returns a Font based on the param which is not of type UIResource. 
     * 
     * @param font the base font
     * @return a font not of type UIResource, may be null.
     */
    public static Font getAsNotUIResource(Font font) {
        if (!(font instanceof UIResource)) return font;
        // PENDING JW: correct way to create another font instance?
       return font.deriveFont(font.getAttributes());
    }
    
    /**
     * Returns a Color based on the param which is not of type UIResource. 
     * 
     * @param color the base color
     * @return a color not of type UIResource, may be null.
     */
    public static Color getAsNotUIResource(Color color) {
        if (!(color instanceof UIResource)) return color;
        // PENDING JW: correct way to create another color instance?
        float[] rgb = color.getRGBComponents(null);
        return new Color(rgb[0], rgb[1], rgb[2], rgb[3]);
    }
    

}
