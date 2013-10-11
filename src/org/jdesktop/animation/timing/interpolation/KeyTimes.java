/**
 * Copyright (c) 2005-2006, Sun Microsystems, Inc
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

import java.util.ArrayList;
import org.jdesktop.animation.timing.*;

/**
 * Stores a list of times from 0 to 1 (the elapsed fraction of an animation
 * cycle) that are used in calculating interpolated
 * values for PropertySetter given a matching set of KeyValues and
 * Interpolators for those time intervals.  In the simplest case, a
 * KeyFrame will consist of just two times in KeyTimes: 0 and 1.
 *
 * @author Chet
 */
public class KeyTimes {
    
    private ArrayList<Float> times = new ArrayList<Float>();
    
    /** 
     * Creates a new instance of KeyTimes.  Times should be in increasing
     * order and should all be in the range [0,1], with the first value
     * being zero and the last being 1
     * @throws IllegalArgumentException Time values must be ordered in
     * increasing value, the first value must be 0 and the last value
     * must be 1
     */
    public KeyTimes(float... times) {
        if (times[0] != 0) {
            throw new IllegalArgumentException("First time value must" +
                    " be zero");
        }
        if (times[times.length - 1] != 1.0f) {
            throw new IllegalArgumentException("Last time value must" +
                    " be one");
        }
        float prevTime = 0;
        for (float time : times) {
            if (time < prevTime) {
                throw new IllegalArgumentException("Time values must be" +
                        " in increasing order");
            }
            this.times.add(time);
            prevTime = time;
        }
    }
    
    ArrayList getTimes() {
        return times;
    }
    
    int getSize() {
        return times.size();
    }

    /**
     * Returns time interval that contains this time fraction
     */
    int getInterval(float fraction) {
        int prevIndex = 0;
        for (int i = 1; i < times.size(); ++i) {
            float time = times.get(i);
            if (time >= fraction) { 
                // inclusive of start time at next interval.  So fraction==1
                // will return the final interval (times.size() - 1)
                return prevIndex;
            }
            prevIndex = i;
        }
        return prevIndex;
    }

    float getTime(int index) {
        return times.get(index);
    }
}