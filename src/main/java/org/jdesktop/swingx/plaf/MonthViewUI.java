/*
 * $Id: MonthViewUI.java 3100 2008-10-14 22:33:10Z rah003 $
 *
 * Copyright 2008 Sun Microsystems, Inc., 4150 Network Circle,
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

import javax.swing.plaf.ComponentUI;
import java.util.Date;

public abstract class MonthViewUI extends ComponentUI {

    /**
     * Returns an array of String to use as names for the days of the week.
     *  
     * @return array of names for the days of the week.
     */
    public abstract String[] getDaysOfTheWeek();

    
    /**
     * Returns the Date at the given location. May be null if the
     * coordinates don't map to a day in the month which contains the 
     * coordinates. Specifically: hitting leading/trailing dates returns null.
     * 
     * Mapping pixel to calendar day.
     *
     * @param x the x position of the location in pixel
     * @param y the y position of the location in pixel
     * @return the day at the given location or null if the location
     *   doesn't map to a day in the month which contains the coordinates.
     */ 
    public abstract Date getDayAtLocation(int x, int y);

    


    /**
     * Returns the last possible date that can be displayed.
     * This is implemented by the UI since it is in control of layout
     * and may possibly yeild different results based on implementation. <p>
     * 
     * It's up to the UI to keep this property, based on internal state and
     * the firstDisplayed as controlled by the JXMonthView.
     * 
     * @return Date The date.
     */
    public abstract Date getLastDisplayedDay();
    


}
