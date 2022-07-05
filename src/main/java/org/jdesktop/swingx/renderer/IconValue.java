/*
 * $Id: IconValue.java 3298 2009-03-11 13:51:25Z kleopatra $
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
package org.jdesktop.swingx.renderer;

import org.jdesktop.swingx.icon.EmptyIcon;

import javax.swing.*;
import java.io.Serializable;

/**
 * A simple converter to return a Icon representation of an Object.
 * <p>
 * 
 * This class is intended to be the "small coin" to configure/format icon cell
 * content of concrete subclasses of <code>ComponentProvider</code>.
 * <p>
 * 
 * 
 * NOTE: this is experimental, most probably will change. A (near) future
 * version with change the signature of the getIcon method to
 * 
 * <pre><code>
 * Icon getIcon(Object value, IconType type);
 * </code></pre>
 * 
 * That will allow a more fine-grained control of custom icons in tree rendering.
 * 
 * @author Jeanette Winzenburg
 */
public interface IconValue extends Serializable {
    
    /**
     * The cell type the icon is used for.
     */
    public enum IconType {
        
        LEAF,
        
        OPEN_FOLDER,
        
        CLOSED_FOLDER
        
    }
    
    /**
     * A marker icon used to indicate a null. 
     * 
     */
    public final static Icon NULL_ICON = new EmptyIcon();
    
    
    /**
     * Returns a icon representation of the given value.
     * 
     * @param value the object to present as Icon
     * @return a Icon representation of the given value, 
     *  may be null if none available.
     *  
     */
    Icon getIcon(Object value);

}
