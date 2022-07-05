/*
 * $Id: ShadingColorHighlighter.java 3100 2008-10-14 22:33:10Z rah003 $
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

import java.awt.*;

/**
 * Experimental replacement of HierarchicalColumnHighligher.
 * Darkens the component's background.
 * 
 * @author Jeanette Winzenburg
 */
public class ShadingColorHighlighter extends ColorHighlighter {

    /**
     * Instantiates a Highlighter with null colors using the default 
     * HighlightPredicate.
     * 
     */
    public ShadingColorHighlighter() {
        this(null);
    }

    /**
     * Instantiates a Highlighter with null colors using the specified 
     * HighlightPredicate.
     * 
     * @param predicate the HighlightPredicate to use.
     */
    public ShadingColorHighlighter(HighlightPredicate predicate) {
        super(predicate, null, null);
    }

    /**
     * Applies a suitable background for the renderer component within the
     * specified adapter. <p>
     * 
     * This implementation applies its a darkened background to an unselected
     * adapter. Does nothing for selected cells.
     *
     * @param renderer the cell renderer component that is to be decorated
     * @param adapter the ComponentAdapter for this decorate operation
     */
    @Override
    protected void applyBackground(Component renderer, ComponentAdapter adapter) {
        if (adapter.isSelected())
            return;
        // PENDING JW: really? That would be applying a absolute color, instead
        // of shading whatever the renderer has.
        Color background = getBackground();
        if (background == null) {
            background = renderer.getBackground();
        }
        // Change to the following
//        Color background = renderer.getBackground();
        if (background != null) {
            renderer.setBackground(computeBackgroundSeed(background));
        }
    }

    protected Color computeBackgroundSeed(Color seed) {
        return new Color(Math.max((int) (seed.getRed() * 0.95), 0), Math.max(
                (int) (seed.getGreen() * 0.95), 0), Math.max((int) (seed
                .getBlue() * 0.95), 0));
    }

}
