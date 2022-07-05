/*
 * $Id: HorizontalLayout.java 4147 2012-02-01 17:13:24Z kschaefe $
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

package org.jdesktop.swingx;

import org.jdesktop.beans.JavaBean;
import org.jdesktop.swingx.util.Separator;

import java.awt.*;

/**
 * Organizes components in a horizontal layout.
 *
 * @author Romain Guy <romain.guy@mac.com>
 * @author Karl Schaefer
 */
@JavaBean
public class HorizontalLayout extends AbstractLayoutManager {
    private static final long serialVersionUID = 8640046926840737487L;
    
    private int gap;

    public HorizontalLayout() {
        this(0);
    }

    //TODO should we allow negative gaps?
    public HorizontalLayout(int gap) {
        this.gap = gap;
    }

    public int getGap() {
        return gap;
    }

    //TODO should we allow negative gaps?
    public void setGap(int gap) {
        this.gap = gap;
    }

    @Override
    public void layoutContainer(Container parent) {
        Insets insets = parent.getInsets();
        Dimension size = parent.getSize();
        
        int height = size.height - insets.top - insets.bottom;
        int width = insets.left;
        
        for (int i = 0, c = parent.getComponentCount(); i < c; i++) {
            Component m = parent.getComponent(i);
            
            if (m.isVisible()) {
                m.setBounds(width, insets.top, m.getPreferredSize().width, height);
                width += m.getSize().width + gap;
            }
        }
    }

    @Override
    public Dimension preferredLayoutSize(Container parent) {
        Dimension pref = new Dimension(0, 0);
        Separator<Integer> sep = new Separator<Integer>(0, gap);

        for (int i = 0, c = parent.getComponentCount(); i < c; i++) {
            Component m = parent.getComponent(i);
            if (m.isVisible()) {
                Dimension componentPreferredSize =
                        parent.getComponent(i).getPreferredSize();
                pref.height = Math.max(pref.height, componentPreferredSize.height);
                pref.width += componentPreferredSize.width + sep.get();
            }
        }
        
        Insets insets = parent.getInsets();
        pref.width += insets.left + insets.right;
        pref.height += insets.top + insets.bottom;
        
        return pref;
    }
}
