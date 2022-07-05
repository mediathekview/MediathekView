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
package org.jdesktop.swingx.sort;

import org.jdesktop.swingx.renderer.StringValue;
import org.jdesktop.swingx.renderer.StringValues;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 * A writable implemenation of StringValueProvider. Typically, this is created and
 * maintained by a collection view and then passed over to interested parties. It is
 * modeled/implemented after the default renderer maintenance in a JTable.<p>
 * 
 * PENDING JW: for safety - better not implement but return a provider. We probably don't want 
 * readers to frickle around here?.
 * 
 * @author Jeanette Winzenburg
 */
public final class StringValueRegistry implements StringValueProvider {

    @SuppressWarnings("unused")
    private static final Logger LOG = Logger
            .getLogger(StringValueRegistry.class.getName());
    
    private Map<Class<?>, StringValue> perClass;
    private HashMap<Integer, StringValue> perColumn;
    private HashMap<Integer, Class<?>> classPerColumn;
    
    /**
     * {@inheritDoc} <p>
     */
    @Override
    public StringValue getStringValue(int row, int column) {
        StringValue sv = getPerColumnMap().get(column);
        if (sv == null) {
            sv = getStringValueByClass(getClass(row, column));
        }
        if (sv == null) {
            sv = getStringValueByClass(Object.class);
        }
        return sv != null ? sv : StringValues.TO_STRING;
    }

//-------------------- manage     
    /**
     * Sets a StringValue to use for the given column. If the converter is null,
     * the mapping is removed.
     * 
     * @param sv the StringValue to use for the given column.
     * @param column the column index in model coordinates.
     * 
     */
    public void setStringValue(StringValue sv, int column) {
        // PENDING really remove mapping if sv null
        getPerColumnMap().put(column, sv);
    }

    /**
     * Removes all per-column mappings of StringValues.
     * 
     */
    public void clearColumnStringValues() {
        getPerColumnMap().clear();
    }
    
    /**
     * Sets the StringValue to use for the given class. If the converter is null,
     * the mapping is removed.
     * 
     * @param sv the StringValue to use for the given column.
     * @param clazz the class 
     */
    public void setStringValue(StringValue sv, Class<?> clazz) {
        // PENDING really remove mapping if sv null
        getPerClassMap().put(clazz, sv);
    }
    
    /**
     * Returns the StringValue registered for the given class. <p>
     * 
     * <b>This is temporarily exposed for testing only - do not use, it will
     * be removed very soon!</b>
     * 
     * @param clazz the class to find the registered StringValue for
     * @return the StringValue registered for the class, or null if not directly
     *   registered.
     */
    public StringValue getStringValue(Class<?> clazz) {
        return getPerClassMap().get(clazz);
    }
    /**
     * Sets the column class.
     * 
     * @param clazz 
     * @param column index in model coordinates
     */
    public void setColumnClass(Class<?> clazz, int column) {
        getColumnClassMap().put(column, clazz);
    }
    
    /**
     * @param classPerColumn
     */
    public void setColumnClasses(Map<Integer, Class<?>> classPerColumn) {
        this.classPerColumn = classPerColumn != null ? 
                new HashMap<Integer, Class<?>>(classPerColumn) : null;
    }

    /**
     * 
     * @param clazz
     * @return
     */
    private StringValue getStringValueByClass(Class<?> clazz) {
        if (clazz == null) return null;
        StringValue sv = getPerClassMap().get(clazz);
        if (sv != null) return sv;
        return getStringValueByClass(clazz.getSuperclass());
    }

    /**
     * Returns the Class of the column.
     * 
     * @param row
     * @param column
     * @return
     */
    private Class<?> getClass(int row, int column) {
        Class<?> clazz = getColumnClassMap().get(column);
        return clazz != null ? clazz : Object.class;
    }

    /**
     * Returns the Map which stores the per-column Class, lazily 
     * creates one if null.
     * 
     * @return the per-column storage map of Class
     */
     private Map<Integer, Class<?>> getColumnClassMap() {
         if (classPerColumn == null) {
             classPerColumn = new HashMap<Integer, Class<?>>();
         }
         return classPerColumn;
     }

     /**
     * Returns the Map which stores the per-class StringValues, lazily 
     * creates one if null.
     * 
     * @return the per-class storage map of StringValues
     */
    private Map<Class<?>, StringValue> getPerClassMap() {
        if (perClass == null) {
            perClass = new HashMap<Class<?>, StringValue>();
        }
        return perClass;
    }

    /**
     * Returns the Map which stores the per-column StringValues, lazily 
     * creates one if null.
     * 
     * @return the per-column storage map of StringValues
     */
    private Map<Integer, StringValue> getPerColumnMap() {
        if (perColumn == null) {
            perColumn = new HashMap<Integer, StringValue>();
        }
        return perColumn;
    }
}
