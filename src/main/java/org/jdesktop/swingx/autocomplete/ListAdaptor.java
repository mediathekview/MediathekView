/*
 * $Id: ListAdaptor.java 4045 2011-07-19 18:39:17Z kschaefe $
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

import javax.swing.*;
import javax.swing.event.ListSelectionListener;
import javax.swing.text.JTextComponent;

/**
 * An implementation of the AbstractAutoCompleteAdaptor that is suitable for a
 * JList in conjunction with a JTextComponent.
 * 
 * @author Thomas Bierhance
 */
public class ListAdaptor extends AbstractAutoCompleteAdaptor implements ListSelectionListener {
    
    /** the list containing the items */
    JList list;
    /** the text component that is used for automatic completion*/
    JTextComponent textComponent;
    /** the converter used to transform items to strings */
    ObjectToStringConverter stringConverter;
    
    /**
     * Creates a new JListAdaptor for the given list and text component.
     * @param list the list that contains the items that are used for automatic
     * completion
     * @param textComponent the text component that will be used automatic
     * completion
     */
    public ListAdaptor(JList list, JTextComponent textComponent) {
        this(list, textComponent, ObjectToStringConverter.DEFAULT_IMPLEMENTATION);
    }
    
    /**
     * Creates a new JListAdaptor for the given list and text component.
     * @param list the list that contains the items that are used for automatic
     * completion
     * @param textComponent the text component that will be used automatic
     * completion
     * @param stringConverter the converter used to transform items to strings
     */
    public ListAdaptor(JList list, JTextComponent textComponent, ObjectToStringConverter stringConverter) {
        this.list = list;
        this.textComponent = textComponent;
        this.stringConverter = stringConverter;
        // when a new item is selected set and mark the text
        list.addListSelectionListener(this);
    }
    
    /**
     * Implementation side effect - do not invoke.
     * @param listSelectionEvent -
     */
    // ListSelectionListener (listening to list)
    @Override
    public void valueChanged(javax.swing.event.ListSelectionEvent listSelectionEvent) {
        // set the text to the currently selected item
        getTextComponent().setText(stringConverter.getPreferredStringForItem(list.getSelectedValue()));
        // mark the entire text
        markEntireText();
    }
    
    @Override
    public Object getSelectedItem() {
        return list.getSelectedValue();
    }
    
    @Override
    public int getItemCount() {
        return list.getModel().getSize();
    }
    
    @Override
    public Object getItem(int index) {
        return list.getModel().getElementAt(index);
    }
    
    @Override
    public void setSelectedItem(Object item) {
        list.setSelectedValue(item, true);
    }
    
    @Override
    public JTextComponent getTextComponent() {
        return textComponent;
    }
}
