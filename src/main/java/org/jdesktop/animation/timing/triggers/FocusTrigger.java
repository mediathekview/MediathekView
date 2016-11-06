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

import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import javax.swing.JComponent;
import org.jdesktop.animation.timing.*;

/**
 * FocusTrigger handles focus events
 * and triggers an animation based on those events.
 * For example, to have anim start when component receives an 
 * IN event, one might write the following:
 * <pre>
 *     FocusTrigger trigger = 
 *         FocusTrigger.addTrigger(component, anim, FocusTriggerEvent.IN);
 * </pre>
 * 
 * 
 * 
 * @author Chet
 */
public class FocusTrigger extends Trigger implements FocusListener {

    /**
     * Creates a non-auto-reversing FocusTrigger and adds it as a FocusListener
     * to the component.
     *
     * @param component component that will generate FocusEvents for this
     * trigger
     * @param animator the Animator that will start when the event occurs
     * @param event the FocusTriggerEvent that will cause the action to fire
     * @return FocusTrigger the resulting trigger
     */
    public static FocusTrigger addTrigger(JComponent component, 
            Animator animator, FocusTriggerEvent event) {
        return addTrigger(component, animator, event, false);
    }
    
    /**
     * Creates a FocusTrigger and adds it as a FocusListener
     * to the component.
     * 
     * @param component component that will generate FocusEvents for this
     * trigger
     * @param animator the Animator that will start when the event occurs
     * @param event the FocusTriggerEvent that will cause the action to fire
     * @param autoReverse flag to determine whether the animator should
     * stop and reverse based on opposite triggerEvents.
     * @return FocusTrigger the resulting trigger
     */
    public static FocusTrigger addTrigger(JComponent component, 
            Animator animator, FocusTriggerEvent event, boolean autoReverse) {
        FocusTrigger trigger = new FocusTrigger(animator, event, autoReverse);
        component.addFocusListener(trigger);
        return trigger;
    }
    
    /**
     * Creates a non-auto-reversing FocusTrigger, which should be added
     * to a Component that will generate the focus events of interest.
     * @param animator the Animator that will start when the event occurs
     * @param event the FocusTriggerEvent that will cause the action to fire
     */
    public FocusTrigger(Animator animator, FocusTriggerEvent event) {
        this(animator, event, false);
    }

    /**
     * Creates a FocusTrigger, which should be added
     * to a Component that will generate the focus events of interest.
     * @param animator the Animator that will start when the event occurs
     * @param event the FocusTriggerEvent that will cause the action to fire
     * @param autoReverse flag to determine whether the animator should
     * stop and reverse based on opposite triggerEvents.
     */
    public FocusTrigger(Animator animator, FocusTriggerEvent event, 
            boolean autoReverse) {
        super(animator, event, autoReverse);
    }

    /**
     * Called by the object which added this trigger as a FocusListener.
     * This method starts the animator if the trigger is waiting for a 
     * IN event.
     */
    public void focusGained(FocusEvent e) {
        fire(FocusTriggerEvent.IN);
    }

    /**
     * Called by the object which added this trigger as a FocusListener.
     * This method starts the animator if the trigger is waiting for a 
     * OUT event.
     */
    public void focusLost(FocusEvent e) {
        fire(FocusTriggerEvent.OUT);
    }
    
}
