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

package org.jdesktop.animation.timing.triggers;

/**
 * Mouse Enter/Exit/Press/Release/Click events
 *
 * @author Chet
 */
public class MouseTriggerEvent extends TriggerEvent {
    /**
     * Event fired when mouse enters
     */
    public static final MouseTriggerEvent ENTER = 
            new MouseTriggerEvent("Entered");
    /**
     * Event fired when mouse exits
     */
    public static final MouseTriggerEvent EXIT = 
            new MouseTriggerEvent("Exit");
    /**
     * Event fired when mouse button is pressed
     */
    public static final MouseTriggerEvent PRESS = 
            new MouseTriggerEvent("Press");
    /**
     * Event fired when mouse button is released
     */
    public static final MouseTriggerEvent RELEASE = 
            new MouseTriggerEvent("Release");
    /**
     * Event fired when mouse is clicked
     */
    public static final MouseTriggerEvent CLICK = 
            new MouseTriggerEvent("Click");

    /**
     * Protected constructor; this helps ensure type-safe use of 
     * pre-define TriggerEvent objects.
     */
    private MouseTriggerEvent(String name) {
        super(name);
    }

    /**
     * This method finds the opposite of the current event.: <BR/>
     * ENTER -> EXIT <BR/>
     * EXIT -> ENTER <BR/>
     * PRESS -> RELEASE <BR/>
     * RELEASE -> PRESS <BR/>
     * Note that CLICK has no obvious opposite so
     * it simply returns CLICK (this method should probably not be called
     * for that case).
     * 
     */
    public TriggerEvent getOppositeEvent() {
        if (this == MouseTriggerEvent.ENTER) {
            return MouseTriggerEvent.EXIT;
        } else if (this == MouseTriggerEvent.EXIT) {
            return MouseTriggerEvent.ENTER;
        } else if (this == MouseTriggerEvent.PRESS) {
            return MouseTriggerEvent.RELEASE;
        } else if (this == MouseTriggerEvent.RELEASE) {
            return MouseTriggerEvent.PRESS;
        }
        // Possible to reach here for REPEAT action (but probably should not
        // have been called with this event)
        return this;
    }
}
