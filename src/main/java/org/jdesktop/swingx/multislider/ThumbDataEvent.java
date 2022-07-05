/*
 * $Id: ThumbDataEvent.java 3475 2009-08-28 08:30:47Z kleopatra $
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
package org.jdesktop.swingx.multislider;

import java.util.EventObject;

/**
 *
 * @author jm158417
 */
public class ThumbDataEvent extends EventObject {
    private int type, index;
    private Thumb<?> thumb;

    /** Creates a new instance of ThumbDataEvent */
    public ThumbDataEvent(Object source, int type, int index, Thumb<?> thumb) {
        super(source);
        this.type = type;
        this.thumb = thumb;
        this.index = index;
    }

    public int getType() {
        return type;
    }
    
    public int getIndex() {
        return index;
    }

    public Thumb<?> getThumb() {
        return thumb;
    }
    
    @Override
    public String toString() {
        return this.getClass().getName() + " : " + type + " " + index + " " + thumb;
    }
}