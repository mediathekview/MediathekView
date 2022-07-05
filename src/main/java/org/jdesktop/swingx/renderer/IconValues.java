/*
 * $Id: IconValues.java 3927 2011-02-22 16:34:11Z kleopatra $
 *
 * Copyright 2008 Sun Microsystems, Inc., 4150 Network Circle,
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

import javax.swing.*;
import javax.swing.filechooser.FileSystemView;
import java.io.File;

/**
 * A collection of common {@code IconValue} implementations.
 * 
 * @author Karl George Schaefer
 * @author Jeanette Winzenburg
 */
public final class IconValues {
    /**
     * Always NULL_ICON. This is useful to indicate that we really want
     * no icon instead of f.i. a default provided by the CellContext. 
     */
    @SuppressWarnings("serial")
    public static final IconValue NONE = new IconValue() {
    
        @Override
        public Icon getIcon(Object value) {
            return IconValue.NULL_ICON;
        }
        
    };

    /**
     * Returns the value as Icon if possible or null.
     */
    @SuppressWarnings("serial")
    public static final IconValue ICON = new IconValue() {
    
        @Override
        public Icon getIcon(Object value) {
            if (value instanceof Icon) {
                return (Icon) value;
            }
            return null;
        }
    };
    
    /**
     * An {@code IconValue} that presents the current L&F icon for a given file.
     * If the value passed to {@code FILE_ICON} is not a {@link File}, this has
     * the same effect as {@link IconValues#NONE}.
     */
    @SuppressWarnings("serial")
    public static final IconValue FILE_ICON = new IconValue() {
        @Override
        public Icon getIcon(Object value) {
            if (value instanceof File) {
                FileSystemView fsv = FileSystemView.getFileSystemView();

                return fsv.getSystemIcon((File) value);
            }

            return IconValues.NONE.getIcon(value);
        }
    };
    
    private IconValues() {
        // does nothing
    }
}
