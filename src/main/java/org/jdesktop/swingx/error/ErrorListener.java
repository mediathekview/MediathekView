/*
 * $Id: ErrorListener.java 2979 2008-07-08 01:32:06Z kschaefe $
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

import java.util.EventListener;

/**
 * ErrorListener defines the interface for an object which listens to errors generated
 * by a JX Swing component. ErrorEvents are only generated for internal un-recoverable errors
 * that cannot be thrown. An example would be an internal Action implementation that cannot
 * throw an Exception directly because the ActionListener interface forbids it. Exceptions
 * which can be throw directly (say from the constructor of the JX component) should not use
 * the ErrorListener mechanism.
 *
 * @see ErrorEvent
 * @see ErrorSupport
 * @author Joshua Marinacci joshua.marinacci@sun.com
 */
public interface ErrorListener extends EventListener {
    
    /**
     * Tells listeners that an error has occured within the watched component.
     * @param event 
     */
    public void errorOccured(ErrorEvent event);
}
