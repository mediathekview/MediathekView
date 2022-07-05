/*
 * $Id: AbstractRenderer.java 3927 2011-02-22 16:34:11Z kleopatra $
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
package org.jdesktop.swingx.renderer;

import org.jdesktop.swingx.plaf.UIDependent;
import org.jdesktop.swingx.rollover.RolloverRenderer;

import java.awt.*;
import java.io.Serializable;

/**
 * Convenience common ancestor for SwingX renderers. Concrete subclasses
 * should 
 * 
 *  <ul>
 *  <li> provide a bunch of convenience constructors as appropriate for the type of 
 *      collection component
 *  <li> create a reasonable default ComponentProvider if none is given  
 *  <li> implement the getXXCellRenderer by delegating to the ComponentProvider
 *  </ul>
 * 
 * @author Jeanette Winzenburg
 */
public abstract class AbstractRenderer 
    implements  RolloverRenderer, StringValue, Serializable, UIDependent {

    protected ComponentProvider<?> componentController;

    public AbstractRenderer(ComponentProvider<?> provider) {
        if (provider ==  null) {
            provider = createDefaultComponentProvider();
        }
        this.componentController = provider;
    }
    
    /**
     * Returns the ComponentProvider used by this renderer.
     * 
     * @return the ComponentProvider used by this renderer
     */
    public ComponentProvider<?> getComponentProvider() {
        return componentController;
    }

    /**
     * The default ComponentProvider to use if no special.
     * 
     * @return the default <code>ComponentProvider</code>
     */
    protected abstract ComponentProvider<?> createDefaultComponentProvider();
    
// --------------- implement StringValue    
    
    /**
     * {@inheritDoc}
     */
    @Override
    public String getString(Object value) {
        return componentController.getString(value);
    }

 // ------------ implement RolloverRenderer   
    
    /**
     * {@inheritDoc}
     */
    @Override
    public void doClick() {
        if (isEnabled()) {
            ((RolloverRenderer) componentController).doClick();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isEnabled() {
        return (componentController instanceof RolloverRenderer)
                && ((RolloverRenderer) componentController).isEnabled();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateUI() {
        componentController.updateUI();
    }

//-------------------- legacy: configure arbitrary visuals    
    /**
     * @param background
     */
    public void setBackground(Color background) {
        componentController.getDefaultVisuals().setBackground(background);
    
    }

    /**
     * @param foreground
     */
    public void setForeground(Color foreground) {
        componentController.getDefaultVisuals().setForeground(foreground);
    }


}
