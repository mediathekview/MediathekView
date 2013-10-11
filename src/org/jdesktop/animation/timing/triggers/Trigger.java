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

import org.jdesktop.animation.timing.Animator;

/**
 * This abstract class should be overridden by any class wanting to
 * implement a new Trigger.  The subclass will define the events to trigger
 * off of and any listeners to handle those events. That subclass will call
 * either {@link #fire()} or {@link #fire(TriggerEvent)} to start the
 * animator based on an event that occurred.
 * <p>
 * Subclasses should call one of the constructors in Trigger, according to
 * whether they want Trigger to discern between different TriggerEvents
 * and whether they want Trigger to auto-reverse the animation based on
 * opposite TriggerEvents.  
 * <p>
 * Subclasses should call one of the <code>fire</code> methods based on
 * whether they want Trigger to perform any event logic or simply start
 * the animation.
 *
 * @author Chet
 */
public abstract class Trigger {

    private boolean disarmed = false;
    private Animator animator, reverseAnimator;
    private TriggerEvent triggerEvent;
    private boolean autoReverse = false;

    /**
     * Creates a Trigger that will start the animator when {@link #fire()}
     * is called. Subclasses call this method to set up a simple Trigger
     * that will be started by calling {@link #fire()}, and will have
     * no dependency upon the specific {@link TriggerEvent} that must have
     * occurred to start the animator.
     * @param animator the Animator that will start when the Trigger
     * is fired
     */
    protected Trigger(Animator animator) {
        this(animator, null);
    }
    
    /**
     * Creates a Trigger that will start the animator when 
     * {@link #fire(TriggerEvent)} is called with an event that equals
     * triggerEvent.
     * @param animator the Animator that will start when the Trigger
     * is fired
     * @param triggerEvent the TriggerEvent that must occur for this
     * Trigger to fire
     */
    protected Trigger(Animator animator, TriggerEvent triggerEvent) {
        this(animator, triggerEvent, false);
    }
    
    /**
     * Creates a Trigger that will start the animator when 
     * {@link #fire(TriggerEvent)} is called with an event that equals
     * triggerEvent. Also, automatically stops and reverses animator when 
     * opposite event occurs, and stops reversing animator likewise
     * when triggerEvent occurs.
     * @param animator the Animator that will start when the Trigger
     * is fired
     * @param triggerEvent the TriggerEvent that must occur for this
     * Trigger to fire
     * @param autoReverse flag to determine whether the animator should
     * stop and reverse based on opposite triggerEvents.
     * @see TriggerEvent#getOppositeEvent()
     */
    protected Trigger(Animator animator, TriggerEvent triggerEvent,
            boolean autoReverse) {
        this.animator = animator;
        this.triggerEvent = triggerEvent;
        this.autoReverse = autoReverse;
    }
    
    /**
     * This method disables this Trigger and effectively noop's any actions
     * that would otherwise occur
     */
    public void disarm() {
        disarmed = true;
    }

    /**
     * Called by subclasses to start the animator if currentEvent equals
     * the event that the Trigger is based upon.  Also, if the Trigger is
     * set to autoReverse, stops and reverses the animator running in the
     * opposite direction as appropriate.
     * @param currentEvent the {@link TriggerEvent} that just occurred, which
     * will be compared with the TriggerEvent used to construct this Trigger
     * and determine whether the animator should be started or reversed
     */
    protected void fire(TriggerEvent currentEvent) {
        if (disarmed) {
            return;
        }
        if (currentEvent == triggerEvent) {
            // event occurred; fire the animation
            if (autoReverse) {
                if (animator.isRunning()) {
                    float f = animator.getTimingFraction();
                    animator.stop();
                    animator.setStartFraction(f);
                } else {
                    animator.setStartFraction(0f);
                }
            }
            if (animator.isRunning()) {
                animator.stop();
            }
            animator.setStartDirection(Animator.Direction.FORWARD);
            fire();
        } else if (triggerEvent != null && 
                currentEvent == triggerEvent.getOppositeEvent()) {
            // Opposite event occurred - run reverse anim if autoReverse
            if (autoReverse) {
                if (animator.isRunning()) {
                    float f = animator.getTimingFraction();
                    animator.stop();
                    animator.setStartFraction(f);
                } else {
                    animator.setStartFraction(1f - 
                            animator.getStartFraction());
                }
                animator.setStartDirection(Animator.Direction.BACKWARD);
                fire();
            }
        }
    }
    
    /**
     * Utility method called by subclasses to start the animator.  This variant
     * assumes that there need be no check of the TriggerEvent that fired,
     * which is useful for subclasses with simple events.
     */
    protected void fire() {
        if (disarmed) {
            return;
        }
        if (animator.isRunning()) {
            animator.stop();
        }
        animator.start();
    }
    
}
