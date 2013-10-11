/**
 * Copyright (c) 2006, Sun Microsystems, Inc
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
 * Superclass for all TriggerEvents used in the Trigger classes.  The methods
 * here are mostly protected; it is expected that callers will not use this
 * class directly, but will instead use subclasses with pre-defined event
 * types. The purpose of this superclass is to provide the ability for 
 * {@link Trigger} to treat event types generically, rather than to have
 * all even logic in the subclasses of Trigger.
 *
 * @author Chet
 */
public class TriggerEvent {
    
    /**
     * The ID of events are simple strings.  It is expected that subclasses
     * will define static objects that callers will use instead of users
     * having to manually create TriggerEvent objects from strings directly
     */
    private String name;
    
    /**
     * Protected constructor; this helps ensure type-safe use of 
     * pre-define TriggerEvent objects.
     */
    protected TriggerEvent(String name) {
        this.name = name;
    }
        
    /**
     * This method returns the 'opposite' event from itself. This is used by
     * {@link Trigger} in running an auto-reversing animation, to determine 
     * whether an opposite event has occurred (and whether to stop/reverse
     * the animation).  Note that some events may have no opposite.
     * Default behavior returns same event; subclasses with multiple/opposite
     * events must override to do the right thing here.
     */
    public TriggerEvent getOppositeEvent() {
        return this;
    }
}
