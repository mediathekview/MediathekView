/*
 * $Id: VerticalLayout.java 4147 2012-02-01 17:13:24Z kschaefe $
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
package org.jdesktop.swingx;

import org.jdesktop.beans.JavaBean;
import org.jdesktop.swingx.util.Separator;

import java.awt.*;
import java.io.Serial;

/**
 * Organizes components in a vertical layout.
 * 
 * @author fred
 * @author Karl Schaefer
 */
@JavaBean
public class VerticalLayout extends AbstractLayoutManager {
    @Serial
    private static final long serialVersionUID = 5342270033773736441L;
    
    private int gap;

    /**
     * Creates a layout without a gap between components.
     */
    public VerticalLayout() {
        this(0);
    }

    /**
     * Creates a layout with the specified gap between components.
     * 
     * @param gap
     *            the gap between components
     */
    //TODO should we allow negative gaps?
    public VerticalLayout(int gap) {
        this.gap = gap;
    }

    /**
     * The current gap to place between components.
     * 
     * @return the current gap
     */
    public int getGap() {
        return gap;
    }

    /**
     * The new gap to place between components.
     * 
     * @param gap
     *            the new gap
     */
    //TODO should we allow negative gaps?
    public void setGap(int gap) {
        this.gap = gap;
    }
    
    /**
     * {@inheritDoc}
     */
    @Override
    public Dimension preferredLayoutSize(Container parent) {
        Dimension pref = new Dimension(0, 0);
        Separator<Integer> sep = new Separator<>(0, gap);
        
        for (int i = 0, c = parent.getComponentCount(); i < c; i++) {
            Component m = parent.getComponent(i);
            
            if (m.isVisible()) {
                Dimension componentPreferredSize = parent.getComponent(i).getPreferredSize();
                pref.height += componentPreferredSize.height + sep.get();
                pref.width = Math.max(pref.width, componentPreferredSize.width);
            }
        }
        
        Insets insets = parent.getInsets();
        pref.width += insets.left + insets.right;
        pref.height += insets.top + insets.bottom;
        
        return pref;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void layoutContainer(Container parent) {
        Insets insets = parent.getInsets();
        Dimension size = parent.getSize();
        int width = size.width - insets.left - insets.right;
        int height = insets.top;

        for (int i = 0, c = parent.getComponentCount(); i < c; i++) {
            Component m = parent.getComponent(i);
            if (m.isVisible()) {
                m.setBounds(insets.left, height, width, m.getPreferredSize().height);
                height += m.getSize().height + gap;
            }
        }
    }
}
