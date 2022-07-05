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

import javax.swing.*;

/**
 * A SortController to use with JXList.
 * 
 * @author Jeanette Winzenburg
 */
public class ListSortController<M extends ListModel> extends DefaultSortController<M> {

    /** underlying model */
    private M listModel;
    /**
     * @param model
     */
    public ListSortController(M model) {
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
        listModel = model;
        if (model != null)
            cachedModelRowCount = model.getSize();
        setModelWrapper(new ListRowSorterModelWrapper());
    }

    /**
     * Implementation of DefaultRowSorter.ModelWrapper that delegates to a
     * TableModel.
     */
    private class ListRowSorterModelWrapper extends ModelWrapper<M,Integer> {
        @Override
        public M getModel() {
            return listModel;
        }

        @Override
        public int getColumnCount() {
            return (listModel == null) ? 0 : 1;
        }

        @Override
        public int getRowCount() {
            return (listModel == null) ? 0 : listModel.getSize();
        }

        @Override
        public Object getValueAt(int row, int column) {
            return listModel.getElementAt(row);
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
