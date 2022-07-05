/*
 * $Id: DefaultListRenderer.java 3779 2010-09-07 18:01:55Z kschaefe $
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

import javax.swing.*;
import java.awt.*;

/**
 * Adapter to glue SwingX renderer support to core API. It has convenience
 * constructors to create a LabelProvider, optionally configured with a
 * StringValue and horizontal alignment. Typically, client code does not
 * interact with this class except at instantiation time.
 * <p>
 * 
 * Note: core DefaultListCellRenderer shows either an icon or the element's
 * toString representation, depending on whether or not the given value 
 * is of type icon or implementors. This renderer's empty/null provider 
 * constructor takes care of configuring the default provider with a converter
 * which mimics that behaviour. When instantiating this renderer with
 * any of the constructors which have converters as parameters,
 * it's up to the client code to supply the appropriate converter, if needed:
 * 
 * 
 * <pre><code>
 * StringValue sv = new StringValue() {
 * 
 *     public String getString(Object value) {
 *         if (value instanceof Icon) {
 *             return &quot;&quot;;
 *         }
 *         return StringValue.TO_STRING.getString(value);
 *     }
 * 
 * };
 * StringValue lv = new MappedValue(sv, IconValue.ICON);
 * listRenderer = new DefaultListRenderer(lv, alignment);
 * 
 * </code></pre>
 * 
 * <p>
 * 
 * 
 * @author Jeanette Winzenburg
 * 
 * @see ComponentProvider
 * @see StringValue
 * @see IconValue
 * @see MappedValue
 * 
 * 
 */
public class DefaultListRenderer extends AbstractRenderer
    implements ListCellRenderer {

    protected ListCellContext cellContext;

    /**
     * Instantiates a default list renderer with the default component
     * provider.
     *
     */
    public DefaultListRenderer() {
        this((ComponentProvider<?>) null);
    }

    /**
     * Instantiates a ListCellRenderer with the given ComponentProvider.
     * If the provider is null, creates and uses a default. The default
     * provider is of type <code>LabelProvider</code><p>
     * 
     * Note: the default provider is configured with a custom StringValue
     * which behaves exactly as core DefaultListCellRenderer: depending on 
     * whether or not given value is of type icon or implementors, it shows 
     * either the icon or the element's toString.  
     * 
     * @param componentProvider the provider of the configured component to
     *   use for cell rendering
     */
    public DefaultListRenderer(ComponentProvider<?> componentProvider) {
        super(componentProvider);
        this.cellContext = new ListCellContext();
    }

    /**
     * Instantiates a default table renderer with a default component controller
     * using the given converter.<p>
     * 
     * PENDING JW: how to guarantee core consistent icon handling? Leave to 
     * client code?
     * 
     * @param converter the converter to use for mapping the content value to a
     *        String representation.
     * 
     */
    public DefaultListRenderer(StringValue converter) {
        this(new LabelProvider(converter));
    }

    /**
     * Instantiates a default list renderer with a default component
     * controller using the given converter and horizontal 
     * alignment. 
     * 
     * PENDING JW: how to guarantee core consistent icon handling? Leave to
     * client code?
     * 
     * 
     * @param converter the converter to use for mapping the
     *   content value to a String representation.
     * @param alignment the horizontal alignment.
     */
    public DefaultListRenderer(StringValue converter, int alignment) {
        this(new LabelProvider(converter, alignment));
    }

    
    /**
     * Instantiates a default list renderer with default component provider
     * using both converters.
     * 
     * @param stringValue the converter to use for the string representation
     * @param iconValue the converter to use for the icon representation
     */
    public DefaultListRenderer(StringValue stringValue, IconValue iconValue) {
        this(new MappedValue(stringValue, iconValue));
    }

    /**
     * Instantiates a default list renderer with default component provider
     * using both converters and the given alignment.
     * 
     * @param stringValue the converter to use for the string representation
     * @param iconValue the converter to use for the icon representation
     * @param alignment the rendering component's horizontal alignment
     */
    public DefaultListRenderer(StringValue stringValue, IconValue iconValue,
            int alignment) {
        this(new MappedValue(stringValue, iconValue), alignment);
    }

    // -------------- implements javax.swing.table.ListCellRenderer
    /**
     * 
     * Returns a configured component, appropriate to render the given
     * list cell.  <p>
     * 
     * Note: The component's name is set to "List.cellRenderer" for the sake
     * of Synth-based LAFs.
     * 
     * @param list the <code>JList</code> to render on
     * @param value the value to assign to the cell 
     * @param isSelected true if cell is selected
     * @param cellHasFocus true if cell has focus
     * @param index the row index (in view coordinates) of the cell to render
     * @return a component to render the given list cell.
     */
    @Override
    public Component getListCellRendererComponent(JList list, Object value,
            int index, boolean isSelected, boolean cellHasFocus) {
        cellContext.installContext(list, value, index, 0, isSelected,
                cellHasFocus, true, true);
        Component comp = componentController.getRendererComponent(cellContext);
        // fix issue #1040-swingx: memory leak if value not released
        cellContext.replaceValue(null);
        return comp;
    }

    /**
     * {@inheritDoc}
     */ 
    @Override
    protected ComponentProvider<?> createDefaultComponentProvider() {
        return new LabelProvider(createDefaultStringValue());
    }

    /**
     * Creates and returns the default StringValue for a JList.<p>
     * This is added to keep consistent with core list rendering which
     * shows either the Icon (for Icon value types) or the default 
     * to-string for non-icon types.
     * 
     * @return the StringValue to use by default.
     */
    private StringValue createDefaultStringValue() {
        return MappedValues.STRING_OR_ICON_ONLY;
    }
}
