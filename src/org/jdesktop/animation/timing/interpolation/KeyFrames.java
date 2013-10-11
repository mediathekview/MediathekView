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

package org.jdesktop.animation.timing.interpolation;

import java.lang.reflect.Method;
import org.jdesktop.animation.timing.*;

/**
 *
 * KeyFrames holds information about the times at which values are sampled
 * (KeyTimes) and the values at those times (KeyValues).  It also holds
 * information about how to interpolate between these values for
 * times that lie between the sampling points.
 *
 * @author Chet
 */
public class KeyFrames {
    
    private KeyValues keyValues;
    private KeyTimes keyTimes;
    private KeyInterpolators interpolators;
    
    /** 
     * Simplest variation; determine keyTimes based on even division of
     * 0-1 range based on number of keyValues.  This constructor
     * assumes LINEAR interpolation.
     * @param keyValues values that will be assumed at each time in keyTimes
     */
    public KeyFrames(KeyValues keyValues) {
        init(keyValues, null, (Interpolator)null);
    }
    
    /**
     * This variant takes both keyValues (values at each
     * point in time) and keyTimes (times at which values are sampled).
     * @param keyValues values that the animation will assume at each of the
     * corresponding times in keyTimes
     * @param keyTimes times at which the animation will assume the
     * corresponding values in keyValues
     * @throws IllegalArgumentException keyTimes and keySizes must have the
     * same number of elements since these structures are meant to have
     * corresponding entries; an exception is thrown otherwise.
     */
    public KeyFrames(KeyValues keyValues, KeyTimes keyTimes) {
        init(keyValues, keyTimes, (Interpolator)null);
    }
    
    /**
     * Full constructor: caller provides
     * an instance of all key* structures which will be used to calculate
     * between all times in the keyTimes list.  A null interpolator parameter
     * is equivalent to calling {@link KeyFrames#KeyFrames(KeyValues, KeyTimes)}.
     * @param keyValues values that the animation will assume at each of the
     * corresponding times in keyTimes
     * @param keyTimes times at which the animation will assume the
     * corresponding values in keyValues
     * @param interpolators collection of Interpolators that control 
     * the calculation of values in each of the intervals defined by keyFrames.
     * If this value is null, a {@link LinearInterpolator} will be used
     * for all intervals.  If there is only one interpolator, that interpolator
     * will be used for all intervals.  Otherwise, there must be a number of
     * interpolators equal to the number of intervals (which is one less than
     * the number of keyTimes).
     * @throws IllegalArgumentException keyTimes and keyValues must have the
     * same number of elements since these structures are meant to have
     * corresponding entries; an exception is thrown otherwise.
     * @throws IllegalArgumentException The number of interpolators must either
     * be zero (interpolators == null), one, or one less than the size of 
     * keyTimes.
     */
    public KeyFrames(KeyValues keyValues, KeyTimes keyTimes,
            Interpolator... interpolators) {
        init(keyValues, keyTimes, interpolators);
    }

    /**
     * Utility constructor that assumes even division of times according to
     * size of keyValues and interpolation according to interpolators 
     * parameter.
     * @param keyValues values that the animation will assume at each of the
     * corresponding times in keyTimes
     * @param interpolators collection of Interpolators that control 
     * the calculation of values in each of the intervals defined by keyFrames.
     * If this value is null, a {@link LinearInterpolator} will be used
     * for all intervals.  If there is only one interpolator, that interpolator
     * will be used for all intervals.  Otherwise, there must be a number of
     * interpolators equal to the number of intervals (which is one less than
     * the number of keyTimes).
     * @throws IllegalArgumentException The number of interpolators must either
     * be zero (interpolators == null), one, or one less than the size of 
     * keyTimes.
     */
    public KeyFrames(KeyValues keyValues, Interpolator... interpolators) {
        init(keyValues, null, interpolators);
    }

    /**
     * Utility function called by constructors to perform common
     * initialization chores
     */
    private void init(KeyValues keyValues, KeyTimes keyTimes,
            Interpolator... interpolators) {
        int numFrames = keyValues.getSize();
        // If keyTimes null, create our own
        if (keyTimes == null) {
            float keyTimesArray[] = new float[numFrames];
            float timeVal = 0.0f;
            keyTimesArray[0] = timeVal;
            for (int i = 1; i < (numFrames - 1); ++i) {
                timeVal += (1.0f / (numFrames - 1));
                keyTimesArray[i] = timeVal;
            }
            keyTimesArray[numFrames - 1] = 1.0f;
            this.keyTimes = new KeyTimes(keyTimesArray);
        } else {
            this.keyTimes = keyTimes;
        }
        this.keyValues = keyValues;
        if (numFrames != this.keyTimes.getSize()) {
            throw new IllegalArgumentException("keyValues and keyTimes" +
                    " must be of equal size");
        }
        if (interpolators != null && 
                (interpolators.length != (numFrames - 1)) &&
                (interpolators.length != 1)) {
            throw new IllegalArgumentException("interpolators must be " +
                    "either null (implying interpolation for all intervals), " +
                    "a single interpolator (which will be used for all " +
                    "intervals), or a number of interpolators equal to " +
                    "one less than the number of times.");
        }
        this.interpolators = new KeyInterpolators(numFrames - 1, interpolators);
    }
        
    Class getType() {
        return keyValues.getType();
    }
    
    KeyValues getKeyValues() {
        return keyValues;
    }
    
    KeyTimes getKeyTimes() {
        return keyTimes;
    }
    
    /**
     * Returns time interval that contains this time fraction
     */
    public int getInterval(float fraction) {
        return keyTimes.getInterval(fraction);
    }
    
    /**
     * Returns a value for the given fraction elapsed of the animation
     * cycle.  Given the fraction, this method will determine what
     * interval the fraction lies within, how much of that interval has
     * elapsed, what the boundary values are (from KeyValues), what the
     * interpolated fraction is (from the Interpolator for the interval),
     * and what the final interpolated intermediate value is (using the
     * appropriate Evaluator).
     * This method will call into the Interpolator for the time interval
     * to get the interpolated method. To ensure that future operations
     * succeed, the value received from the interpolation will be clamped
     * to the interval [0,1].
     */
    Object getValue(float fraction) {
        // First, figure out the real fraction to use, given the
        // interpolation type and keyTimes
        int interval = getInterval(fraction);
        float t0 = keyTimes.getTime(interval);
        float t1 = keyTimes.getTime(interval + 1);
        float t = (fraction - t0) / (t1 - t0);
        float interpolatedT = interpolators.interpolate(interval, t);
        // clamp to avoid problems with buggy Interpolators
        if (interpolatedT < 0f) {
            interpolatedT = 0f;
        } else if (interpolatedT > 1f) {
            interpolatedT = 1f;
        }
        return keyValues.getValue(interval, (interval+1), interpolatedT);
    }
    
}
