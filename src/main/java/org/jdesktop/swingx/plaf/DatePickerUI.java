/*
 * $Id: DatePickerUI.java 3100 2008-10-14 22:33:10Z rah003 $
 *
 * Copyright 2005 Sun Microsystems, Inc., 4150 Network Circle,
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
import java.beans.PropertyVetoException;
import java.util.Date;

/**
 * The ComponentUI for a JXDatePicker.
 * <p>
 * 
 * Responsible for keeping the date property of all participants synchronized at
 * all "stable" points in their life-cycle. That is the following invariant is
 * guaranteed:
 * 
 * <pre><code>
 * Date selected = datePicker.getMonthView().getSelectedDate();
 * assertEquals(selected, datePicker.getDate());
 * assertEquals(selected, datePicker.getEditor().getValue());
 * </code></pre>
 * 
 * @author Joshua Outwater
 * @author Jeanette Winzenburg
 */
public abstract class DatePickerUI extends ComponentUI {
    /**
     * Get the baseline for the specified component, or a value less
     * than 0 if the baseline can not be determined.  The baseline is measured
     * from the top of the component.
     *
     * @param width  Width of the component to determine baseline for.
     * @param height Height of the component to determine baseline for.
     * @return baseline for the specified component
     */
    public int getBaseline(int width, int height) {
        return -1;
    }

    /**
     * Checks the given date for validity for selection. If valid, 
     * returns the date as appropriate in the picker's context, otherwise
     * throws a propertyVetoException. Note that the returned date might
     * be different from the input date, f.i. the time fields might be
     * cleared. The input date is guaranteed to be unchanged.
     * 
     * @param date date to check
     * @return the date as allowed in the context of the picker.
     * 
     * @throws PropertyVetoException if the given date is not valid for 
     *    selection
     */
    public abstract Date getSelectableDate(Date date) throws PropertyVetoException;

}
