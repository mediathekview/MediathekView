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
package org.jdesktop.swingx.decorator;

import java.awt.*;

/**
 * A Highlighter which sets the enabled property.<p>
 * 
 * Note: the enabled is a mutable property of this Highlighter which defaults to false 
 * because we assume that's the most common use case to make a rendering component
 * look disabled when the parent is enabled. It's mutable for symmetry reasons, though
 * the other way round - enabled looking rendering component on a disabled parent -
 * most probably will confuse users.
 * 
 * @author Jeanette Winzenburg (slight cleanup)
 * @author original contributed by swingx member martinm1000
 */
public class EnabledHighlighter extends AbstractHighlighter {
    
    private boolean enabled;

    /**
     * Instantiates a EnabledHighlighter with default enabled property (== false). 
     * The Highlighter is applied always.
     */
    public EnabledHighlighter() {
        this(null);
    }
    
    /**
     * Instantiates a EnabledHighlighter with the specified enabled property. 
     * The Highlighter is applied always.
     * 
     * @param enabled the enabled property
     */
    public EnabledHighlighter(boolean enabled) {
        this(null, enabled);
    }
    
    /**
     * Instantiates a EnabledHighlighter with the specified HighlightPredicate and 
     * default enabled property (== false). 
     * @param predicate the HighlightPredicate to use, may be null to default to ALWAYS.
     */
    public EnabledHighlighter(HighlightPredicate predicate) {
        this(predicate, false);
    }
 
    /**
     * Instantiates a EnabledHighlighter with the specified HighlightPredicate and 
     * default enabled property. 
     * @param predicate the HighlightPredicate to use, may be null to default to ALWAYS.
     * @param enabled the enabled property
     */
    public EnabledHighlighter(HighlightPredicate predicate, boolean enabled) {
        super(predicate);
        this.enabled = enabled;
    }
    
    
    /**
     * Returns the enabled property. 
     * @return the enabled
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * Sets the enabled property. The default value is false.
     * 
     * @param enabled the enabled to set
     */
    public void setEnabled(boolean enabled) {
        if (isEnabled() == enabled) return;
        this.enabled = enabled;
        fireStateChanged();
    }

    /**
     * {@inheritDoc} <p>
     * 
     * Implemented to set the rendering component's enabled property.
     */
    @Override
    protected Component doHighlight(Component renderer, ComponentAdapter adapter) {
        renderer.setEnabled(enabled);
        return renderer;
    }
 

}
