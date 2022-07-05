/*
 * $Id: TextComponentAdaptor.java 4045 2011-07-19 18:39:17Z kschaefe $
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
import java.util.List;

/**
 * An implementation of the AbstractAutoCompleteAdaptor that is suitable for a
 * JTextComponent.
 * 
 * @author Thomas Bierhance
 */
public class TextComponentAdaptor extends AbstractAutoCompleteAdaptor {
    
    /** a <tt>List</tt> containing the strings to be used for automatic
     * completion */
    List<?> items;
    /** the text component that is used for automatic completion*/
    JTextComponent textComponent;
    /** the item that is currently selected */
    Object selectedItem;
    
    /**
     * Creates a new <tt>TextComponentAdaptor</tt> for the given list and text
     * component.
     * 
     * @param items a <tt>List</tt> that contains the items that are used for
     * automatic completion
     * @param textComponent the text component that will be used automatic
     * completion
     */
    public TextComponentAdaptor(JTextComponent textComponent, List<?> items) {
        this.items = items;
        this.textComponent = textComponent;
    }
    
    @Override
    public Object getSelectedItem() {
        return selectedItem;
    }
    
    @Override
    public int getItemCount() {
        return items.size();
    }
    
    @Override
    public Object getItem(int index) {
        return items.get(index);
    }
    
    @Override
    public void setSelectedItem(Object item) {
        selectedItem = item;
    }
    
    @Override
    public JTextComponent getTextComponent() {
        return textComponent;
    }
}
