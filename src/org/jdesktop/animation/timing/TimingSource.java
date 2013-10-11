/**
 * Copyright (c) 2007, Sun Microsystems, Inc
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above
 *     copyright notice, this list of conditions and the following 
 *     disclaimer in the documentation and/or other materials provided 
 *     with the distribution.
 *   * Neither the name of the TimingFramework project nor the names of its
 *     contributors may be used to endorse or promote products derived 
 *     from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.jdesktop.animation.timing;

import java.util.ArrayList;

/**
 * This class provides a generic wrapper for arbitrary
 * Timers that may be used with the Timing Framework.
 * Animator creates its own internal TimingSource by default,
 * but an Animator can be directed to use a different 
 * TimingSource by calling {@link Animator#setTimer(TimingSource)}.
 *
 * The implementation details of any specific timer may
 * vary widely, but any timer should be able to expose
 * the basic capabilities used in this interface. Animator
 * depends on these capabilities for starting, stopping,
 * and running any TimingSource.
 *
 * The usage of an external TimingSource object for sending in timing
 * events to an Animator is to implement this interface appropriately, 
 * pass in that object to {@link Animator#setTimer(TimingSource)},
 * which adds the Animator as a listener to the TimingSource object,
 * and then send in any later timing events from the object to the
 * protected method {@link #timingEvent()}, which will send these timing
 * events to all listeners.
 * 
 * @author Chet
 */
public abstract class TimingSource {

    // listeners that will receive timing events
    private ArrayList<TimingEventListener> listeners = 
            new ArrayList<TimingEventListener>();
    
    /**
     * Starts the TimingSource
     */
    public abstract void start();
    
    /**
     * Stops the TimingSource
     */
    public abstract void stop();
        
    /**
     * Sets the delay between callback events. This 
     * will be called by Animator if its
     * {@link Animator#setResolution(int) setResolution(int)}
     * method is called. Note that the actual resolution may vary,
     * according to the resolution of the timer used by the framework as well
     * as system load and configuration; this value should be seen more as a
     * minimum resolution than a guaranteed resolution.
     * @param resolution delay, in milliseconds, between 
     * each timing event callback.
     * @throws IllegalArgumentException resolution must be >= 0
     * @see Animator#setResolution(int)
     */
    public abstract void setResolution(int resolution);
    
    /**
     * Sets delay which should be observed by the 
     * TimingSource after a call to {@link #start()}. Some timers may not be
     * able to adhere to specific resolution requests
     * @param delay delay, in milliseconds, to pause before
     * starting timing events.
     * @throws IllegalArgumentException resolution must be >= 0
     * @see Animator#setStartDelay(int)
     */
    public abstract void setStartDelay(int delay);
    
    /**
     * Adds a TimingEventListener to the set of listeners that
     * receive timing events from this TimingSource.
     * @param listener the listener to be added.
     */
    public final void addEventListener(TimingEventListener listener) {
        synchronized(listeners) {
            if (!listeners.contains(listener)) {
                listeners.add(listener);
            }
        }
    }
    
    /**
     * Removes a TimingEventListener from the set of listeners that
     * receive timing events from this TimingSource.
     * @param listener the listener to be removed.
     */
    public final void removeEventListener(TimingEventListener listener) {
        synchronized(listeners) {
            listeners.remove(listener);
        }
    }
    

    /**
     * Subclasses call this method to post timing events to this
     * object's {@link TimingEventListener} objects.
     */
     protected final void timingEvent() {
         synchronized(listeners) {
             for (TimingEventListener listener : listeners) {
                 listener.timingSourceEvent(this);
             }
         }
    }
}
