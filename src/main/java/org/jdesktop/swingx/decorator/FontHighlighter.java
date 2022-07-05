/*
 * $Id: FontHighlighter.java 1164 2009-11-03 04:22:00Z kschaefe $
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
package org.jdesktop.swingx.decorator;

import java.awt.*;

/**
 * A Highlighter which sets the Font of the component.<p>
 * 
 * @author Karl George Schaefer
 *
 */
public class FontHighlighter extends AbstractHighlighter {
    private Font font;
    
    /**
     * Instantiates a FontHighlighter with null Font. The Highlighter is 
     * applied always.
     */
    public FontHighlighter() {
        this((HighlightPredicate) null);
    }
    
    /**
     * Instantiates a FontHighlighter with the given Font. The Highlighter is 
     * applied always.
     * 
     * @param font the Font to apply
     */
    public FontHighlighter(Font font) {
        this(null, font);
    }
    
    /**
     * Instantiates a FontHighlighter with the given HighlightPredicate and null Font.
     * 
     * @param predicate the HighlightPredicate to use, may be null to default to ALWAYS.
     */
    public FontHighlighter(HighlightPredicate predicate) {
        this(predicate, null);
    }
    
    /**
     * Instantiates a FontHighlighter with the given Font and HighlightPredicate.
     * 
     * @param predicate the HighlightPredicate to use, may be null to default to ALWAYS.
     * @param font the Font to apply, may be null
     */
    public FontHighlighter(HighlightPredicate predicate, Font font) {
        super(predicate);
        this.font = font;
    }

    /**
     * Returns the Font used for decoration.
     * 
     * @return the Font used for decoration
     * 
     * @see #setFont(Font)
     */
    public Font getFont() {
        return font;
    }
    
    /**
     * Sets the Font used for decoration. May be null to not decorate.
     * 
     * @param font the Font used for decoration, may be null to not decorate.
     * 
     * @see #getFont()
     */
    public void setFont(Font font) {
        if (areEqual(font, getFont())) return;
        this.font = font;
        fireStateChanged();
    }

    /**
     * {@inheritDoc}<p>
     * 
     * Implemented to return false if the font property is null.
     */
    @Override
    protected boolean canHighlight(Component component, ComponentAdapter adapter) {
        return font != null;
    }
    
    /**
     * {@inheritDoc}<p>
     * 
     * Implemented to set the component's Font.
     */
    @Override
    protected Component doHighlight(Component component, ComponentAdapter adapter) {
        component.setFont(font);
        return component;
    }
}
