/*
 * $Id: DefaultsList.java 4047 2011-07-19 18:51:12Z kschaefe $
 * 
 * Copyright 2007 Sun Microsystems, Inc., 4150 Network Circle,
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
package org.jdesktop.swingx.plaf;

import org.jdesktop.swingx.painter.Painter;
import org.jdesktop.swingx.util.Contract;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;

/**
 * A specialty "list" for working with UI defaults. Requires adds to be done
 * using key/value pairs. The purpose of this list is to enforce additions as
 * pairs.
 * 
 * @author Karl George Schaefer
 */
@SuppressWarnings("nls")
public final class DefaultsList {
    private List<Object> delegate;

    /**
     * Creates a {@code DefaultsList}.
     */
    public DefaultsList() {
        delegate = new ArrayList<Object>();
    }

    /**
     * Adds a key/value pair to the defaults list. This implementation defers to
     * {@link #add(Object, Object, boolean)} with {@code enableChecking} set to
     * {@code true}.
     * 
     * @param key
     *                the key that will be used to query {@code UIDefaults}
     * @param value
     *                the value associated with the key
     * @throws NullPointerException
     *                 if {@code key} is {@code null}
     * @throws IllegalArgumentException
     *                 if {@code value} is a type that should be a
     *                 {@code UIResource} but is not. For instance, passing in a
     *                 {@code Border} that is not a {@code UIResource} will
     *                 cause an exception. This checking must be enabled.
     */
    public void add(Object key, Object value) {
        add(key, value, true);
    }

    /**
     * Adds a key/value pair to the defaults list. A pair with a {@code null}
     * value is treated specially. A {@code null}-value pair is never added to
     * the list and, furthermore, if a key/value pair exists in this list with
     * the same key as the newly added one, it is removed.
     * 
     * @param key
     *                the key that will be used to query {@code UIDefaults}
     * @param value
     *                the value associated with the key
     * @param enableChecking
     *                if {@code true} then the value is checked to ensure that
     *                it is a {@code UIResource}, if appropriate
     * @throws NullPointerException
     *                 if {@code key} is {@code null}
     * @throws IllegalArgumentException
     *                 if {@code value} is a type that should be a
     *                 {@code UIResource} but is not. For instance, passing in a
     *                 {@code Border} that is not a {@code UIResource} will
     *                 cause an exception. This checking must be enabled.
     */
    public void add(Object key, Object value, boolean enableChecking) {
        if (enableChecking) {
            asUIResource(value, value + " must be a UIResource");
        }
        
        if (value == null && delegate.contains(key)) {
            int i = delegate.indexOf(key);
            
            delegate.remove(i + 1);
            delegate.remove(i);
        } else if (value != null) {
            delegate.add(Contract.asNotNull(key, "key cannot be null"));
            delegate.add(value);
        }
    }
    
    //TODO move to Contract?
    private static <T> T asUIResource(T value, String message) {
        if (!(value instanceof UIResource)) {
            boolean shouldThrow = false;
            
            shouldThrow |= value instanceof ActionMap;
            shouldThrow |= value instanceof Border;
            shouldThrow |= value instanceof Color;
            shouldThrow |= value instanceof Dimension;
            shouldThrow |= value instanceof Font;
            shouldThrow |= value instanceof Icon;
            shouldThrow |= value instanceof InputMap;
            shouldThrow |= value instanceof Insets;
            shouldThrow |= value instanceof Painter<?>;
            //FIXME how to handle UIResource testing
//            shouldThrow |= value instanceof StringValue;
            
            if (shouldThrow) {
                throw new IllegalArgumentException(message);
            }
        }
        
        return value;
    }
    
    /**
     * Gets a copy of this list as an array.
     * 
     * @return an array containing all of the key/value pairs added to this list
     */
    public Object[] toArray() {
        return delegate.toArray();
    }
}
