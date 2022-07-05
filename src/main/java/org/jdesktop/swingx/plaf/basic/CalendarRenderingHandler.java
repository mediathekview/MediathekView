/*
 * $Id: CalendarRenderingHandler.java 3166 2009-01-02 13:27:18Z rah003 $
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
package org.jdesktop.swingx.plaf.basic;

import org.jdesktop.swingx.JXMonthView;

import javax.swing.*;
import java.util.Calendar;
import java.util.Locale;

/**
 * The RenderingHandler responsible for text rendering. It provides 
 * and configures a rendering component for the given cell of
 * a JXMonthView. <p>
 * 
 * @author Jeanette Winzenburg
 */
public interface CalendarRenderingHandler {

    /**
     * Configures and returns a component for rendering of the given monthView cell.
     * 
     * @param monthView the JXMonthView to render onto
     * @param calendar the cell value
     * @param state the DayState of the cell
     * @return a component configured for rendering the given cell
     */
    public JComponent prepareRenderingComponent(JXMonthView monthView,
                                                Calendar calendar, CalendarState state);

    /**
     * Updates internal state to the given Locale.
     * 
     * PENDING JW: ideally, the handler should be stateless and this method
     * removed. Currently needed because there is no way to get the Locale 
     * from a Calendar. 
     * 
     * @param locale the new Locale.
     */
    public void setLocale(Locale locale);

}
