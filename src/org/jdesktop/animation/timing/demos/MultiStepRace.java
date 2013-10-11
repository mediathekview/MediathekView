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

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.SwingUtilities;
import org.jdesktop.animation.timing.Animator;
import org.jdesktop.animation.timing.Animator.RepeatBehavior;
import org.jdesktop.animation.timing.interpolation.Interpolator;
import org.jdesktop.animation.timing.interpolation.KeyFrames;
import org.jdesktop.animation.timing.interpolation.KeyTimes;
import org.jdesktop.animation.timing.interpolation.KeyValues;
import org.jdesktop.animation.timing.interpolation.PropertySetter;
import org.jdesktop.animation.timing.interpolation.SplineInterpolator;
import org.jdesktop.animation.timing.triggers.ActionTrigger;

/**
 * The full-blown demo with all of the bells and whistles.  This one uses
 * the facilities shown in all of the other variations, but adds 
 * both multi-step and non-linear interpolation.  It does this by
 * creating a KeyFrames object to hold the times/values/splines
 * used for each segment of the race.  It also adds an animation for
 * the rotation of the car (since the car should turn as it goes around the
 * curves) and sound effects (just to go completely overboard).
 *
 * @author Chet
 */
public class MultiStepRace {
    
    protected Animator animator;
    private SoundEffects soundEffects;
    public static final int RACE_TIME = 10000;
    
    
    /** Creates a new instance of BasicRace */
    public MultiStepRace(String appName) {
        RaceGUI basicGUI = new RaceGUI(appName);
        
        // Now set up an animation that will automatically
        // run itself with PropertySetter
        
        // We're going to need a more involved PropertyRange object
        // that has all curves of the track in it, as well as 
        // non-linear movement around the curves
        Point values[] = {
            TrackView.START_POS,
            TrackView.FIRST_TURN_START, TrackView.FIRST_TURN_END,
            TrackView.SECOND_TURN_START, TrackView.SECOND_TURN_END,
            TrackView.THIRD_TURN_START, TrackView.THIRD_TURN_END,
            TrackView.FOURTH_TURN_START, 
            TrackView.START_POS};
        KeyValues<?> keyValues = KeyValues.create(values);
        // Calculate the keyTimes based on the distances that must be
        // traveled on each leg of the journey
        double totalDistance = 0;
        double segmentDistance[] = new double[values.length];
        for (int i = 0; i < (values.length - 1); ++i) {
            segmentDistance[i] = values[i].distance(values[i + 1]);
            totalDistance += segmentDistance[i];
        }
        segmentDistance[(values.length-1)] = 
                values[(values.length - 1)].distance(values[0]);
        totalDistance += segmentDistance[(values.length-1)];
        float times[] = new float[values.length];
        float elapsedTime = 0.0f;
        times[0] = 0.0f;
        times[values.length - 1] = 1.0f;
        for (int i = 0; i < (values.length - 2); ++i) {
            times[i + 1] = elapsedTime + (float)(segmentDistance[i] / totalDistance);
            elapsedTime = times[i + 1];
        }
        KeyTimes keyTimes = new KeyTimes(times);
        // For realistic movement, we want a big acceleration
        // on the straightaways
        Interpolator initialSpline = new SplineInterpolator(1.00f, 0.00f, 0.2f, .2f);
        Interpolator straightawaySpline = new SplineInterpolator(0.50f, 0.20f, .50f, .80f);
        Interpolator curveSpline = new SplineInterpolator(0.50f, 0.20f, .50f, .80f);
        Interpolator finalSpline = new SplineInterpolator(0.50f, 0.00f, .50f, 1.00f);
        KeyFrames keyFrames = new KeyFrames(keyValues, keyTimes,
                initialSpline, curveSpline, straightawaySpline, curveSpline,
                straightawaySpline, curveSpline,
                straightawaySpline, finalSpline);
        PropertySetter modifier = new PropertySetter(basicGUI.getTrack(), 
                "carPosition", keyFrames);
        
        // Now create the timing controller to run this.  Make it repeating
        animator = new Animator(RACE_TIME, Animator.INFINITE,
                RepeatBehavior.LOOP, modifier);
        
        // Now create similar keyframes for rotation of car
        keyValues = KeyValues.create(360, 315, 270, 225, 180, 135, 90, 45, 0);
        Interpolator straightawayTurnSpline = new SplineInterpolator(1.0f, 0.0f, 1.0f, 0.0f);
        Interpolator curveTurnSpline = new SplineInterpolator(0.0f, 0.5f, 0.5f, 1.0f);
        keyFrames = new KeyFrames(keyValues, keyTimes, 
                straightawayTurnSpline, curveTurnSpline, 
                straightawayTurnSpline, curveTurnSpline, 
                straightawayTurnSpline, curveTurnSpline, 
                straightawayTurnSpline, curveTurnSpline);
        modifier = new PropertySetter(basicGUI.getTrack(), "carRotation", 
                keyFrames);
        animator.addTarget(modifier);
        
        // Finally, add sound effects, triggered by the same animator
        soundEffects = new SoundEffects(keyFrames);
        animator.addTarget(soundEffects);
        
        // Instead of manually tracking the events, have the framework do
        // the work by setting up a trigger
        JButton goButton = basicGUI.getControlPanel().getGoButton();
        JButton stopButton = basicGUI.getControlPanel().getStopButton();
        ActionTrigger.addTrigger(goButton, animator);
        stopButton.addActionListener(new Stopper(animator));
    }
    
    private class Stopper implements ActionListener {
        Animator timer;
        Stopper(Animator timer) {
            this.timer = timer;
        }
        public void actionPerformed(ActionEvent ae) {
            timer.stop();
        }
    }

    public static void main(String args[]) {
        Runnable doCreateAndShowGUI = new Runnable() {
            public void run() {
                new MultiStepRace("Multi Step Race");
            }
        };
        SwingUtilities.invokeLater(doCreateAndShowGUI);
    }
}
