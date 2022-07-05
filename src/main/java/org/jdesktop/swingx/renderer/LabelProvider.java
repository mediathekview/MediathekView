/*
 * $Id: LabelProvider.java 3100 2008-10-14 22:33:10Z rah003 $
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

/**
 * A component provider which uses a <code>JLabel</code> as rendering
 * component. <p>
 * 
 * It configures the Label's text and icon property from the  
 * StringValue.
 * 
 * @author Jeanette Winzenburg
 * 
 * @see StringValue
 * @see FormatStringValue
 * @see IconValue
 */
public class LabelProvider extends ComponentProvider<JLabel> {

    /**
     * Instantiates a LabelProvider with default to-String converter and LEADING
     * horizontal alignment .
     * <p>
     * 
     */
    public LabelProvider() {
        this(null);
    }
    
    /**
     * Instantiates a LabelProvider with the given to-String converter and LEADING
     * horizontal alignment. If the converter is null, the default TO_STRING is
     * used.
     * <p>
     * 
     * @param converter the converter to use for mapping the cell value to a
     *        String representation.
     */
    public LabelProvider(StringValue converter) {
        this(converter, JLabel.LEADING);
    }

    /**
     * Instantiates a LabelProvider with default to-String converter and given
     * horizontal alignment. 
     * 
     * @param alignment the horizontal alignment.
     */
    public LabelProvider(int alignment) {
        this(null, alignment);
    }

    /**
     * Instantiates a LabelProvider with given to-String converter and given
     * horizontal alignment. If the converter is null, the default TO_STRING is
     * used.
     * 
     * @param converter the converter to use for mapping the cell value to a
     *        String representation.
     * @param alignment the horizontal alignment.
     */
    public LabelProvider(StringValue converter, int alignment) {
        super(converter, alignment);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected JLabel createRendererComponent() {
        return new JRendererLabel();
    }

    /**
     * {@inheritDoc}
     * Here: sets the Label's horizontal alignment to the alignment as configured 
     * in the controller.
     */
    @Override
    protected void configureState(CellContext context) {
       rendererComponent.setHorizontalAlignment(getHorizontalAlignment());
    }

    /**
     * {@inheritDoc}
     * Here: sets the labels's text and icon property to the value as 
     * returned by getValueAsString/Icon, respectively.
     * 
     * @param context the cellContext to use
     * 
     * @see #getValueAsString(CellContext)
     * @see #getValueAsIcon(CellContext) 
     */
    @Override
    protected void format(CellContext context) {
        rendererComponent.setIcon(getValueAsIcon(context));
        rendererComponent.setText(getValueAsString(context));
    }

    
}
