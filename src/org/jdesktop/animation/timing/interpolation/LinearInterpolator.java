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

import org.jdesktop.animation.timing.*;

/**
 * This class implements the Interpolator interface by providing a
 * simple interpolate function that simply returns the value that
 * it was given. The net effect is that callers will end up calculating
 * values linearly during intervals.
 * <p>
 * Because there is no variation to this class, it is a singleton and
 * is referenced by using the {@link #getInstance} static method.
 *
 * @author Chet
 */
public final class LinearInterpolator implements Interpolator {
    
    private static LinearInterpolator instance = null;
    
    private LinearInterpolator() {}
    
    /**
     * Returns the single DiscreteInterpolator object
     */
    public static LinearInterpolator getInstance() {
        if (instance == null) {
            instance = new LinearInterpolator();
        }
        return instance;
    }
    
    /**
     * This method always returns the value it was given, which will cause
     * callers to calculate a linear interpolation between boundary values.
     * @param fraction a value between 0 and 1, representing the elapsed
     * fraction of a time interval (either an entire animation cycle or an 
     * interval between two KeyTimes, depending on where this Interpolator has
     * been set)
     * @return the same value passed in as <code>fraction</code>
     */
    public float interpolate(float fraction) {
        return fraction;
    }
    
}
