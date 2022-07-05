/*
 * $Id: BackgroundPaintable.java 4188 2012-06-27 14:21:10Z kschaefe $
 *
 * Copyright 2010 Sun Microsystems, Inc., 4150 Network Circle,
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
package org.jdesktop.swingx;

import org.jdesktop.swingx.painter.Painter;

/**
 * An interface to define the common methods that are required for defining a background painter.
 * 
 * @author kschaefer
 */
@SuppressWarnings("rawtypes")
public interface BackgroundPaintable {
    /**
     * Returns the current background painter.
     * 
     * @return the current painter
     * @see #setBackgroundPainter(Painter)
     * @see #isPaintBorderInsets()
     */
    Painter getBackgroundPainter();
    
    /**
     * Sets the new background painter.
     * 
     * @param painter the new background painter; may be {@code null}
     */
    void setBackgroundPainter(Painter painter);
    
    /**
     * Determines whether this component paints its background paint underneath the border.
     * 
     * @return {@code true} to paint under the border; {@code false} otherwise
     */
    boolean isPaintBorderInsets();
    
    /**
     * 
     * @param paintBorderInsets
     */
    void setPaintBorderInsets(boolean paintBorderInsets);
}
