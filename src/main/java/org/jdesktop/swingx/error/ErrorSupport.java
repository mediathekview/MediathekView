/*
 * $Id: ErrorSupport.java 3840 2010-10-09 03:25:17Z kschaefe $
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

import javax.swing.*;
import java.util.ArrayList;
import java.util.List;

/**
 * ErrorSupport provides support for managing error listeners.
 * @author Joshua Marinacci joshua.marinacci@sun.com
 * @see ErrorListener
 * @see ErrorEvent
 */
public class ErrorSupport {
    private List<ErrorListener> listeners;
    private Object source;
    
    /**
     * Creates a new instance of <CODE>ErrorSupport</CODE>
     * @param source The object which will fire the <CODE>ErrorEvent</CODE>s
     */
    public ErrorSupport(Object source) {
        this.source = source;
        listeners = new ArrayList<ErrorListener>();
    }
    
    /**
     * Add an ErrorListener
     * @param listener the listener to add
     */
    public void addErrorListener(ErrorListener listener) {
        listeners.add(listener);
    }
    
    /**
     * Remove an error listener
     * @param listener the listener to remove
     */
    public void removeErrorListener(ErrorListener listener) {
        listeners.remove(listener);
    }
        
    /**
     * Returns an array of all the listeners which were added to the 
     * <CODE>ErrorSupport</CODE> object with <CODE>addErrorListener()</CODE>.
     * @return all of the <CODE>ErrorListener</CODE>s added or an empty array if no listeners have been 
     * added.
     */
    public ErrorListener[] getErrorListeners() {
        return listeners.toArray(new ErrorListener[0]);
    }

    /**
     * Report that an error has occurred
     * @param throwable The <CODE>{@link Error}</CODE> or <CODE>{@link Exception}</CODE> which occured.
     */
    public void fireErrorEvent(final Throwable throwable) {
        final ErrorEvent evt = new ErrorEvent(throwable, source);
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                for(ErrorListener el : listeners) {
                    el.errorOccured(evt);
                }
            }
        });
    }
    
}
