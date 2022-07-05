/*
 * $Id: ToolTipHighlighter.java 3676 2010-04-26 15:42:26Z kschaefe $
 *
 * Copyright 2010 Sun Microsystems, Inc., 4150 Network Circle,
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
package org.jdesktop.swingx.decorator;

import org.jdesktop.swingx.renderer.StringValue;

import javax.swing.*;
import java.awt.*;

/**
 * A highlighter for setting a tool tip on the component.
 * 
 * @author kschaefer
 */
public class ToolTipHighlighter extends AbstractHighlighter {
    private StringValue toolTipValue;
    
    /**
     * Instantiates a ToolTipHighlighter with null StringValue. The Highlighter is 
     * applied always.
     */
    public ToolTipHighlighter() {
        this((HighlightPredicate) null);
    }

    /**
     * Instantiates a ToolTipHighlighter with the specified StringValue. The Highlighter is applied
     * always.
     * 
     * @param toolTipValue
     *            the StringValue used to create the tool tip
     */
    public ToolTipHighlighter(StringValue toolTipValue) {
        this(null, toolTipValue);
    }

    /**
     * Instantiates a ToolTipHighlighter with the specified HighlightPredicate and a null
     * StringValue.
     * 
     * @param predicate
     *            the HighlightPredicate to use, may be null to default to ALWAYS.
     */
    public ToolTipHighlighter(HighlightPredicate predicate) {
        this(predicate, null);
    }
    
    /**
     * Instantiates a ToolTipHighlighter with the specified HighlightPredicate and StringValue.
     * 
     * @param predicate
     *            the HighlightPredicate to use, may be null to default to ALWAYS.
     * @param toolTipValue
     *            the StringValue used to create the tool tip
     */
    public ToolTipHighlighter(HighlightPredicate predicate, StringValue toolTipValue) {
        super(predicate);
        
        this.toolTipValue = toolTipValue;
    }

    /**
     * Returns the StringValue used for decoration.
     * 
     * @return the StringValue used for decoration
     * 
     * @see #setToolTipValue(Font)
     */
    public StringValue getToolTipValue() {
        return toolTipValue;
    }
    
    /**
     * Sets the StringValue used for decoration. May be null to use default decoration.
     * 
     * @param font the Font used for decoration, may be null to use default decoration.
     * 
     * @see #getToolTipValue()
     */
    public void setToolTipValue(StringValue toolTipValue) {
        if (areEqual(toolTipValue, getToolTipValue())) return;
        this.toolTipValue = toolTipValue;
        fireStateChanged();
    }

    /**
     * {@inheritDoc}<p>
     * 
     * Implemented to return false if the component is not a JComponent.
     */
    @Override
    protected boolean canHighlight(Component component, ComponentAdapter adapter) {
        return component instanceof JComponent;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Component doHighlight(Component component, ComponentAdapter adapter) {
        String toolTipText = null;
        
        if (toolTipValue == null) {
            toolTipText = adapter.getString();
        } else {
            toolTipText = toolTipValue.getString(adapter.getValue());
        }
        
        ((JComponent) component).setToolTipText(toolTipText);
        
        return component;
    }
}
