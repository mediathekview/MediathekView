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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.Method;
import org.jdesktop.animation.timing.*;

/**
 * ActionTrigger handles action events and
 * starts the animator when actions occur.
 * For example, to have anim start when a button is clicked, 
 * one might write the following:
 * <pre>
 *     ActionTrigger trigger = ActionTrigger.addTrigger(button, anim);
 * </pre>
 *
 * @author Chet
 */
public class ActionTrigger extends Trigger implements ActionListener {
    
    /**
     * Creates an ActionTrigger and adds it as a listener to object.
     *
     * @param object an object that will be used as an event source for
     * this trigger. This object must have the method addActionListener.
     * @param animator the Animator that start when the event occurs
     * @return ActionTrigger the resulting trigger
     * @throws IllegalArgumentException if object has no 
     * <code>addActionListener()</code>
     */
    public static ActionTrigger addTrigger(Object object, Animator animator) {
        ActionTrigger trigger = new ActionTrigger(animator);
        try {
            Method addListenerMethod = 
                    object.getClass().getMethod("addActionListener",
                    ActionListener.class);
            addListenerMethod.invoke(object, trigger);
        } catch (Exception e) {
            throw new IllegalArgumentException("Problem adding listener" +
                    " to object: " + e);
        }
        return trigger;
    }
    
    /**
     * Creates an ActionTrigger that will start the animator upon receiving
     * any ActionEvents. It should be added to any suitable object with
     * an addActionListener method.
     * @param animator the Animator that start when the event occurs
     */
    public ActionTrigger(Animator animator) {
        super(animator);
    }
    
    /**
     * Called by an object generating ActionEvents to which this
     * trigger was added as an ActionListener. This starts the Animator.
     */
    public void actionPerformed(ActionEvent ae) {
        fire();
    }
    
}
