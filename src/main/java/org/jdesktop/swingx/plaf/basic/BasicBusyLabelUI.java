/*
 * $Id: BasicBusyLabelUI.java 3927 2011-02-22 16:34:11Z kleopatra $
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
package org.jdesktop.swingx.plaf.basic;

import org.jdesktop.swingx.JXBusyLabel;
import org.jdesktop.swingx.painter.BusyPainter;
import org.jdesktop.swingx.plaf.BusyLabelUI;
import org.jdesktop.swingx.plaf.UIManagerExt;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicLabelUI;
import java.awt.*;

/**
 * Base implementation of the <code>JXBusyLabel</code> UI.
 *
 * @author rah003
 */
public class BasicBusyLabelUI extends BasicLabelUI implements BusyLabelUI {
    
    /** Creates a new instance of BasicBusyLabelUI */
    public BasicBusyLabelUI(JXBusyLabel lbl) {
    }
    
  public static ComponentUI createUI(JComponent c) {
    return new BasicBusyLabelUI((JXBusyLabel)c);
  }

    @Override
    public BusyPainter getBusyPainter(final Dimension dim) {
        BusyPainter p = new BusyPainter() {
            @Override
            protected void init(Shape point, Shape trajectory, Color b, Color h) {
                super.init(dim == null ? UIManagerExt.getShape("JXBusyLabel.pointShape") : getScaledDefaultPoint(dim.height), 
                        dim == null ? UIManagerExt.getShape("JXBusyLabel.trajectoryShape") : getScaledDefaultTrajectory(dim.height),
                        UIManagerExt.getSafeColor("JXBusyLabel.baseColor", Color.LIGHT_GRAY),
                        UIManagerExt.getSafeColor("JXBusyLabel.highlightColor", Color.BLACK));
            }
        };
        return p;
    }

    @Override
    public int getDelay() {
        return UIManager.getInt("JXBusyLabel.delay");
    }


}
