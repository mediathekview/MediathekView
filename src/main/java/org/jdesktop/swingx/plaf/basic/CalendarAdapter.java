/*
 * $Id: CalendarAdapter.java 3877 2010-11-03 11:36:33Z kleopatra $
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
 *
 */
package org.jdesktop.swingx.plaf.basic;

import org.jdesktop.swingx.JXMonthView;
import org.jdesktop.swingx.decorator.ComponentAdapter;

import java.util.Calendar;

/**
 * ComponentAdapter for a JXMonthView (experimental for internal use of BasicMonthViewUI).<p>
 * 
 * For now, this effectively disables all notion of row/column coordinates. It's focused
 * on an externally provided date (as Calendar) and CalendarState. Yeah, I know, that's
 * tweaking too much but then, I want to use highlighters which need an adapter...
 *
 * 
 * @author Jeanette Winzenburg
 */
class CalendarAdapter extends ComponentAdapter {

    Calendar calendar;
    CalendarState dayState;

    /**
     * @param component
     */
    public CalendarAdapter(JXMonthView component) {
        super(component);
    }

    /**
     * @param calendar2
     * @param dayState2
     * @return
     */
    public CalendarAdapter install(Calendar calendar, CalendarState dayState) {
        this.calendar = calendar;
        this.dayState = dayState;
        return this;
    }


    @Override
    public JXMonthView getComponent() {
        return (JXMonthView) super.getComponent();
    }
    
    public CalendarState getCalendarState() {
        return dayState;
    }
    
    public boolean isFlagged() {
        if (getComponent() == null || calendar == null) {
            return false;
        }
        return getComponent().isFlaggedDate(calendar.getTime());
    }
    
    public boolean isUnselectable() {
        if (getComponent() == null || calendar == null || !isSelectable()) {
            return false;
        }
        return getComponent().isUnselectableDate(calendar.getTime());
    }

    /**
     * @param dayState
     * @return
     */
    private boolean isSelectable() {
        return (CalendarState.IN_MONTH == getCalendarState()) || (CalendarState.TODAY == getCalendarState());
    }

    @Override
    public boolean isSelected() {
        if (getComponent() == null || calendar == null) {
            return false;
        }
        return getComponent().isSelected(calendar.getTime());
    }
    
    
    @Override
    public Object getFilteredValueAt(int row, int column) {
        return getValueAt(row, column);
    }

    @Override
    public Object getValueAt(int row, int column) {
        return calendar;
    }
    
    @Override
    public boolean hasFocus() {
        return false;
    }

    @Override
    public boolean isCellEditable(int row, int column) {
        return false;
    }

    @Override
    public boolean isEditable() {
        return false;
    }
}
