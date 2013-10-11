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

package org.jdesktop.animation.timing.demos;

import java.applet.AudioClip;
import java.net.URL;
import org.jdesktop.animation.timing.TimingTarget;
import org.jdesktop.animation.timing.interpolation.KeyFrames;

/**
 * Simple utility class used to load and play sound effects for
 * MultiStepRace.
 *
 * @author Chet
 */
public class SoundEffects implements TimingTarget {
    
    AudioClip drivingClip;
    AudioClip turningClip;
    KeyFrames keyFrames;
    
    /** Creates a new instance of SoundEffects */
    public SoundEffects(KeyFrames keyFrames) {
        this.keyFrames = keyFrames;
        try {
            URL url  = DemoResources.getResource(DemoResources.VROOM);
            drivingClip = java.applet.Applet.newAudioClip(url);
            url  = DemoResources.getResource(DemoResources.DRIFT);
            turningClip = java.applet.Applet.newAudioClip(url);
        } catch (Exception e) {
            System.out.println("Problem loading sound effects: " + e);
        }
    }
    
    public void drive() {
        if (drivingClip != null) {
            drivingClip.loop();
        }
    }
    
    public void stop() {
        if (drivingClip != null) {
            drivingClip.stop();
        }
        if (turningClip != null) {
            turningClip.stop();
        }
    }
    
    public void turn() {
        if (turningClip != null) {
            turningClip.play();
        }
    }
    
    // TimingTarget implementation
    
    boolean pastFirstTurn = false;
    boolean pastSecondTurn = false;
    boolean pastThirdTurn = false;
    boolean pastFourthTurn = false;
    
    public void begin() {
        drive();
        pastFirstTurn = false;
        pastSecondTurn = false;
        pastThirdTurn = false;
        pastFourthTurn = false;
    }
    
    public void end() {
        stop();
    }
    
    public void timingEvent(float fraction) {
       if (!pastFirstTurn) {
           if (keyFrames.getInterval(fraction) == 1) {
               turn();
               pastFirstTurn = true;
           }
       } else if (!pastSecondTurn) {
           if (keyFrames.getInterval(fraction) == 3) {
               turn();
               pastSecondTurn = true;
           }
       } else if (!pastThirdTurn) {
           if (keyFrames.getInterval(fraction) == 5) {
               turn();
               pastThirdTurn = true;
           }
       } else if (!pastFourthTurn) {
           if (keyFrames.getInterval(fraction) == 7) {
               turn();
               pastFourthTurn = true;
           }
       }
    }

    public void repeat() {
        pastFirstTurn = false;
        pastSecondTurn = false;
        pastThirdTurn = false;
        pastFourthTurn = false;
    }    
}
