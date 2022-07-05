/*
 * $Id: DateSelectionEvent.java 3272 2009-02-25 11:06:37Z kleopatra $
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
package org.jdesktop.swingx.event;

import org.jdesktop.swingx.calendar.DateSelectionModel;

import java.util.Date;
import java.util.EventObject;
import java.util.SortedSet;

/**
 * @author Joshua Outwater
 */
public class DateSelectionEvent extends EventObject {
    public static enum EventType {
        DATES_ADDED,
        DATES_REMOVED,
        DATES_SET,
        SELECTION_CLEARED,
        SELECTABLE_DATES_CHANGED,
        SELECTABLE_RANGE_CHANGED,
        UNSELECTED_DATES_CHANGED,
        LOWER_BOUND_CHANGED,
        UPPER_BOUND_CHANGED, 
        ADJUSTING_STARTED, ADJUSTING_STOPPED, 
        CALENDAR_CHANGED,
    }

    private EventType eventType;
    private boolean adjusting;

    /**
     * Constructs a prototypical Event.
     *
     * @param source The object on which the Event initially occurred.
     * @param eventType the type of the event
     * @param adjusting the adjusting property of the source
     * @throws IllegalArgumentException if source is null.
     */
    public DateSelectionEvent(Object source, EventType eventType, boolean adjusting) {
        super(source);
        this.eventType = eventType;
        this.adjusting = adjusting;
    }

    /**
     * Returns the selection of the source dateSelectionModel.<p>
     * 
     * PENDING JW: that's the "live" selection, that is the source is re-queried on every call
     * to this method. Bug or feature?
     * 
     * @return the selection of the source.
     */
    public SortedSet<Date> getSelection() {
        return ((DateSelectionModel)source).getSelection();
    }

    /**
     * Returns the type of this event.
     * 
     * @return the type of event.
     */
    public final EventType getEventType() {
        return eventType;
    }

    /**
     * Returns a boolean indicating whether the event source is in adjusting state.
     * 
     * @return true if the event is fired while the model is in adjusting state.
     */
    public boolean isAdjusting() {
        return adjusting;
    }

    @Override
    public String toString() {
        return "[" + String.valueOf(getSource()) + " type: " + getEventType() + " isAdjusting: " + isAdjusting();
    }
    
    
}
