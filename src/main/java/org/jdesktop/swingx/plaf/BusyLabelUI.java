/*
 * $Id: BusyLabelUI.java 3964 2011-03-17 19:12:29Z kschaefe $
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

package org.jdesktop.swingx.plaf;

import org.jdesktop.swingx.painter.BusyPainter;

import java.awt.*;

/**
 *
 * @author rah003
 */
public interface BusyLabelUI {
    /**
     * @return The BusyPainter for the JXBusyLabel. If
     * this method returns null, then no progress indication will be shown by busy label.
     */
    public BusyPainter getBusyPainter(Dimension dim);
    
    /**
     * Delay between moving from one point to another. The exact timing will be close to the selected value but is not guaranteed to be precise (subject to the timing precision of underlaying jvm).
     * @return Delay in ms.
     */
    public int getDelay();
}
