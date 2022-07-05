/*
 * $Id: IconHighlighter.java 4192 2012-06-27 19:20:56Z kschaefe $
 *
 * Copyright 2007 Sun Microsystems, Inc., 4150 Network Circle,
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

import org.jdesktop.swingx.renderer.IconAware;

import javax.swing.*;
import java.awt.*;

/**
 * Highlighter which decorates by setting the icon property of a JLabel.<p>
 * 
 * Note: The limitation to JLabel icons (vs. covering AbstractButton as well) 
 * is intentional. Highlighters are allowed to touch only those properties of the 
 * rendering component which are guaranteed to be reset by the corresponding 
 * ComponentProvider, this implementation is safe enough - LabelProvider guarantees
 * to reset both text and icon. On the other hand, CheckBoxProvider doesn't touch
 * the icon (which is LAF depend), consequently this Highlighter must not touch
 * it as well. Custom subclasses trying to cover AbstractButton 
 * must take care that their custom providers reset the icon property.
 *  
 * @author Jeanette Winzenburg
 * 
 * @see org.jdesktop.swingx.renderer.ComponentProvider
 * @see org.jdesktop.swingx.renderer.LabelProvider
 * @see org.jdesktop.swingx.renderer.CheckBoxProvider
 */
public class IconHighlighter extends AbstractHighlighter {

    private Icon icon;

    /**
     * Instantiates a IconHighlighter with null Icon and default
     * HighlightPredicate.
     */
    public IconHighlighter() {
        this((HighlightPredicate) null);
    }
    
    /**
     * Instantiates a IconHighlighter with null Icon the given predicate.
     * 
     * @param predicate the HighlightPredicate to use.
     */
    public IconHighlighter(HighlightPredicate predicate) {
        this(predicate, null);
    }


    /**
     * Instantiates a IconHighlighter with the specified Icon and default
     * HighlightPredicate.
     * 
     * @param icon the icon to use for decoration.
     */
    public IconHighlighter(Icon icon) {
        this(null, icon);
    }

    
    /**
     * Instantiates a IconHighlighter with the specified Icon and 
     * HighlightPredicate.
     * 
     * @param predicate the HighlightPredicate to use.
     * @param icon the Icon to use for decoration.
     */
    public IconHighlighter(HighlightPredicate predicate, Icon icon) {
        super(predicate);
        setIcon(icon);
    }

    /**
     * Sets the icon to use for decoration. A null icon indicates 
     * to not decorate. <p>
     * 
     * The default value is null.
     * 
     * @param icon the Icon to use for decoration, might be null.
     */
    public void setIcon(Icon icon) {
        if (areEqual(icon, getIcon())) return;
        this.icon = icon;
        fireStateChanged();
    }

    /**
     * Returns the Icon used for decoration.
     * 
     * @return icon the Icon used for decoration.
     * @see #setIcon(Icon)
     */
    public Icon getIcon() {
        return icon;
    }
    
    /**
     * {@inheritDoc}
     * 
     * Implemented to set the component's Icon property, if possible and
     * this Highlighter's icon is not null. Does nothing if the decorating icon is null.
     * @see #canHighlight(Component, ComponentAdapter)
     * @see #setIcon(Icon)
     */
    @Override
    protected Component doHighlight(Component component,
            ComponentAdapter adapter) {
        if (getIcon() != null) {
            if (component instanceof IconAware) {
                ((IconAware) component).setIcon(getIcon());
            } else if (component instanceof JLabel) {
                ((JLabel) component).setIcon(getIcon());
            }
        }
        return component;
    }

    /**
     * {@inheritDoc} <p>
     * 
     * Overridden to return true if the component is of type IconAware or
     * of type JLabel, false otherwise. <p>
     * 
     * Note: special casing JLabel is for backward compatibility - application
     * highlighting code which doesn't use the Swingx renderers would stop working
     * otherwise.
     */
    @Override
    protected boolean canHighlight(Component component, ComponentAdapter adapter) {
        return component instanceof IconAware || component instanceof JLabel;
    }

    
}
