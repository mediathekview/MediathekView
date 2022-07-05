/*
 * $Id: MappedValues.java 3882 2010-11-05 09:24:37Z kleopatra $
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
 */
package org.jdesktop.swingx.renderer;

import javax.swing.*;
import javax.swing.plaf.UIResource;

/**
 * A collection of common {@code MappedValue} implementations.
 *
 * @author kschaefer
 */
public final class MappedValues {
    /**
     * A {@code MappedValue} that returns either a {@code String} or {@code Icon}, but not both.
     */
    @SuppressWarnings("serial")
    public static final MappedValue STRING_OR_ICON_ONLY = new MappedValue(new StringValue() {
        @Override
        public String getString(Object value) {
            if (value instanceof Icon) {
                return StringValues.EMPTY.getString(value);
            }
            
            return StringValues.TO_STRING.getString(value);
        }
    }, IconValues.ICON);
    
    /**
     * MappedValue wrapper of type UIResource to tag LAF installed converters.
     * 
     * @author (Jeanette Winzenburg, Berlin
     */
    public static class MappedValueUIResource extends MappedValue implements UIResource {
        
        public MappedValueUIResource(MappedValue delegate) {
            this(delegate, delegate, delegate);
        }

        /**
         * @param stringDelegate
         * @param iconDelegate
         * @param booleanDelegate
         */
        public MappedValueUIResource(StringValue stringDelegate,
                IconValue iconDelegate, BooleanValue booleanDelegate) {
            super(stringDelegate, iconDelegate, booleanDelegate);
        }

        /**
         * @param stringDelegate
         * @param iconDelegate
         */
        public MappedValueUIResource(StringValue stringDelegate,
                IconValue iconDelegate) {
            super(stringDelegate, iconDelegate);
        }
        
    }

    private MappedValues() {
        //prevent instantiation
    }
}
