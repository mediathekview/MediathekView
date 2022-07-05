/*
 * $Id: AbstractAutoCompleteAdaptor.java 4045 2011-07-19 18:39:17Z kschaefe $
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
package org.jdesktop.swingx.autocomplete;

import javax.swing.text.JTextComponent;

/**
 * This is the interface that binds the mechanism for automatic completion to
 * a data model, a selection model (e.g. those used by JList, JComboBox and JTable)
 * and the JTextComponent for which the automatic completion should happen.
 * It is used to search and select a matching item and to mark the completed text
 * inside the JTextComponent. Using this interface the mechanism for automatic
 * completion is independent from the underlying data and selection model.
 *
 * @see ComboBoxAdaptor
 * @see ListAdaptor
 *
 * @author Thomas Bierhance
 */
public abstract class AbstractAutoCompleteAdaptor {
    
    /** the string representation in use for the currently selected item*/
    private String selectedItemAsString;
    
    /**
     * Returns the currently selected item.
     * @return the selected item
     */
    public abstract Object getSelectedItem();
    
    /**
     * Sets the selected item.
     * @param item the item that is to be selected
     */
    public abstract void setSelectedItem(Object item);
    
    /**
     * Returns the string representation in use for the currently selected item.
     * @return the string representation in use for the currently selected item
     */
    public String getSelectedItemAsString() {
        return this.selectedItemAsString;
    }
    
    /**
     * Sets the string representation in use for the currently selected item.
     * @param itemAsString the string representation in use for the currently selected item
     */
    public void setSelectedItemAsString(String itemAsString) {
        this.selectedItemAsString = itemAsString;
    }

    /**
     * Returns the number of items in the list.
     * @return the number of items in the list
     */
    public abstract int getItemCount();
    
    /**
     * Returns the item at a given index. It is supposed that <code>0&lt;=index&lt;<b>getItemCount()</b></code>.
     * @param index the index of the item that is to be returned
     * @return the item at the given <code>index</code>
     */
    public abstract Object getItem(int index);
    
    /**
     * Returns true if the list contains the currently selected item.
     * @return true if the list contains the currently selected item.
     */
    public boolean listContainsSelectedItem() {
        Object selectedItem = getSelectedItem();
        for (int i=0,n=getItemCount(); i<n; i++) {
            if (getItem(i)==selectedItem) return true;
        }
        return false;
    }
    
    /**
     * Returns the text component that is being used for the automatic completion.
     * @return the text component being used for the automatic completion
     */
    public abstract JTextComponent getTextComponent();
    
    /**
     * Marks/selects the entire text that is displayed inside the text component.
     */
    public void markEntireText() {
        markText(0);
    }
    
    /**
     * Marks/selects the text that is displayed inside the text component starting from the
     * character with index <tt>start</tt>.
     * @param start index of the first character that should be marked
     */
    public void markText(int start) {
        getTextComponent().setCaretPosition(getTextComponent().getText().length());
        getTextComponent().moveCaretPosition(start);
    }
}
