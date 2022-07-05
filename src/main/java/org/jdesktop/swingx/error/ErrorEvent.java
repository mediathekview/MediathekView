/*
 * $Id: ErrorEvent.java 2979 2008-07-08 01:32:06Z kschaefe $
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

package org.jdesktop.swingx.error;

import java.util.EventObject;

/**
 * Defines an event which encapsulates an error which occurred in a JX Swing component
 * which supports ErrorListeners.
 *
 * @author Joshua Marinacci joshua.marinacci@sun.com
 * @see ErrorListener
 * @see ErrorSupport
 */
public class ErrorEvent extends EventObject {
    private Throwable throwable;
    
    /**
     * Creates a new instance of <CODE>ErrorEvent</CODE>
     * @param throwable The Error or Exception which occurred.
     * @param source The object which threw the Error or Exception
     */
    public ErrorEvent(Throwable throwable, Object source) {
        super(source);
        this.throwable = throwable;
    }

    /**
     * Gets the Error or Exception which occurred.
     * @return The Error or Exception which occurred.
     */
    public Throwable getThrowable() {
        return throwable;
    }
    
}
