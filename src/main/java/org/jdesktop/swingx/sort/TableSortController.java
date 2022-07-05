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

import javax.swing.table.TableModel;
import java.text.Collator;
import java.util.Comparator;

/**
 * A SortController to use for a JXTable.<p>
 * 
 * @author Jeanette Winzenburg
 */
public class TableSortController<M extends TableModel> extends DefaultSortController<M>  {
    /**
     * Underlying model.
     */
    private M tableModel;
    
    public TableSortController() {
        this(null);
    }
    
    /**
     * @param model
     */
    public TableSortController(M model) {
        super();
        setModel(model);
    }

    /**
     * Sets the <code>TableModel</code> to use as the underlying model
     * for this <code>TableRowSorter</code>.  A value of <code>null</code>
     * can be used to set an empty model.
     *
     * @param model the underlying model to use, or <code>null</code>
     */
    public void setModel(M model) {
        tableModel = model;
        if (model != null)
            cachedModelRowCount = model.getRowCount();
        setModelWrapper(new TableRowSorterModelWrapper());
    }

    
    /**
     * Returns the <code>Comparator</code> for the specified 
     * column.  If a <code>Comparator</code> has not been specified using
     * the <code>setComparator</code> method a <code>Comparator</code>
     * will be returned based on the column class
     * (<code>TableModel.getColumnClass</code>) of the specified column.
     * If the column class is <code>String</code>,
     * <code>Collator.getInstance</code> is returned.  If the
     * column class implements <code>Comparable</code> a private
     * <code>Comparator</code> is returned that invokes the
     * <code>compareTo</code> method.  Otherwise
     * <code>Collator.getInstance</code> is returned.<p>
     * 
     * PENDING JW: think about implications to string value lookup!
     *
     * @throws IndexOutOfBoundsException {@inheritDoc}
     */
    @Override
    public Comparator<?> getComparator(int column) {
        Comparator<?> comparator = super.getComparator(column);
        if (comparator != null) {
            return comparator;
        }
        Class<?> columnClass = getModel().getColumnClass(column);
        if (columnClass == String.class) {
            return Collator.getInstance();
        }
        if (Comparable.class.isAssignableFrom(columnClass)) {
            return COMPARABLE_COMPARATOR;
        }
        return Collator.getInstance();
    }

    /**
     * {@inheritDoc}<p>
     * Note: must implement same logic as the overridden comparator
     * lookup, otherwise will throw ClassCastException because 
     * here the comparator is never null. <p>
     * 
     * PENDING JW: think about implications to string value lookup!
     * 
     * @throws IndexOutOfBoundsException {@inheritDoc}
     */
    @Override
    protected boolean useToString(int column) {
        Comparator<?> comparator = super.getComparator(column);
        if (comparator != null) {
            return false;
        }
        Class<?> columnClass = getModel().getColumnClass(column);
        if (columnClass == String.class) {
            return false;
        }
        if (Comparable.class.isAssignableFrom(columnClass)) {
            return false;
        }
        return true;
    }


    /**
     * Implementation of DefaultRowSorter.ModelWrapper that delegates to a
     * TableModel.
     */
    private class TableRowSorterModelWrapper extends ModelWrapper<M,Integer> {
        @Override
        public M getModel() {
            return tableModel;
        }

        @Override
        public int getColumnCount() {
            return (tableModel == null) ? 0 : tableModel.getColumnCount();
        }

        @Override
        public int getRowCount() {
            return (tableModel == null) ? 0 : tableModel.getRowCount();
        }

        @Override
        public Object getValueAt(int row, int column) {
            return tableModel.getValueAt(row, column);
        }

        @Override
        public String getStringValueAt(int row, int column) {
            return getStringValueProvider().getStringValue(row, column)
                .getString(getValueAt(row, column));
        }

        @Override
        public Integer getIdentifier(int index) {
            return index;
        }
    }


}
