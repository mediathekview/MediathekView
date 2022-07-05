/*
 * $Id: ResetDTCRColorHighlighter.java 3100 2008-10-14 22:33:10Z rah003 $
 *
 * Copyright 2006 Sun Microsystems, Inc., 4150 Network Circle,
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
package org.jdesktop.swingx.decorator;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.*;


/**
 * This is a hack around DefaultTableCellRenderer color "memory", 
 * see Issue #258-swingx.<p>
 * 
 * The issue is that the default has internal color management 
 * which is different from other types of renderers. The
 * consequence of the internal color handling is that there's
 * a color memory which must be reset somehow. The "old" hack around
 * reset the xxColors of all types of renderers to the adapter's
 * target XXColors, introducing #178-swingx (Highlighgters must not
 * change any colors except those for which their color properties are
 * explicitly set).<p>
 * 
 * This hack limits the interference to renderers of type 
 * DefaultTableCellRenderer, applying a hacking highlighter which
 *  resets the renderers XXColors to a previously "memorized" 
 *  color. Note that setting the color to null didn't have the desired
 *  effect.<p>
 * 
 * PENDING: extend ColorHighlighter
 */

public class ResetDTCRColorHighlighter extends ColorHighlighter {

    public ResetDTCRColorHighlighter() {
        super(null, null);
    }

    /**
     * applies the memory hack for renderers of type DefaultTableCellRenderer,
     * does nothing for other types.
     * @param renderer the component to highlight
     * @param adapter the renderee's component state.
     */
    @Override
    public Component highlight(Component renderer, ComponentAdapter adapter) {
        //JW
        // table renderers have different state memory as list/tree renderers
        // without the null they don't unstamp!
        // but... null has adversory effect on JXList f.i. - selection
        // color is changed. This is related to #178-swingx: 
        // highlighter background computation is weird.
        // 
       if (renderer instanceof DefaultTableCellRenderer) {
            return super.highlight(renderer, adapter);
        } 
        return renderer;
    }

    @Override
    protected void applyBackground(Component renderer, ComponentAdapter adapter) {
        if (!adapter.isSelected()) {
            Object colorMemory = ((JComponent) renderer).getClientProperty("rendererColorMemory.background");
            if (colorMemory instanceof ColorMemory) {
                renderer.setBackground(((ColorMemory) colorMemory).color);
            } else {
                ((JComponent) renderer).putClientProperty("rendererColorMemory.background", new ColorMemory(renderer.getBackground()));
            }
        }
    }

    @Override
    protected void applyForeground(Component renderer, ComponentAdapter adapter) {
        if (!adapter.isSelected()) {
            Object colorMemory = ((JComponent) renderer).getClientProperty("rendererColorMemory.foreground");
            if (colorMemory instanceof ColorMemory) {
                renderer.setForeground(((ColorMemory) colorMemory).color);
            } else {
                ((JComponent) renderer).putClientProperty("rendererColorMemory.foreground", new ColorMemory(renderer.getForeground()));
            }
        }
    }
    
    private static class ColorMemory {
        public ColorMemory(Color color) {
            this.color = color;
        }

        Color color;
    }


    
}
