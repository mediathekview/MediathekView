/*
 * $Id: Painters.java 3590 2010-01-12 19:19:45Z kschaefe $
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
package org.jdesktop.swingx.painter;

import java.awt.*;

/**
 * A collection of static painters.  These painters do not store state and are safe to reuse.
 * @author kschaefer
 */
public final class Painters {
    public static final Painter<Object> EMPTY_PAINTER = new Painter<Object>() {
        @Override
        public void paint(Graphics2D g, Object object, int width, int height) {
            //does nothing
        }
    };
    
    private Painters() {
        //prevent instantiation
    }
}
