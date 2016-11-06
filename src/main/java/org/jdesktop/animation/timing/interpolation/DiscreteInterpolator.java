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

/**
 * This class implements the Interpolator interface.  It should
 * be used in cases where a "discrete" animation is desired.  A
 * discrete animation is defined to be one where the values during
 * an animation do not change smoothly between the boundary values,
 * but suddenly, at the boundary points.  For example, a discrete animation
 * with KeyFrames where the KeyTimes are {0, .5, 1.0} and the KeyValues
 * are (0, 1, 2} would, during the animation, retain the value of 0 until
 * half-way through the animation and 1 through the rest of the animation.
 * <p>
 * Because there is no variation to this class, it is a singleton and
 * is referenced by using the {@link #getInstance} static method.
 *
 * @author Chet
 */
public final class DiscreteInterpolator implements Interpolator {
    
    private static DiscreteInterpolator instance = null;
    
    private DiscreteInterpolator() {}
    
    /**
     * Returns the single DiscreteInterpolator object
     */
    public static DiscreteInterpolator getInstance() {
        if (instance == null) {
            instance = new DiscreteInterpolator();
        }
        return instance;
    }
    
    /**
     * This method always returns 0 for inputs less than 1, 
     * which will force users of this
     * interpolation to assign a value equal to the value at the beginning
     * of this timing interval, which is the desired behavior for discrete
     * animations.  An input of 1 will return 1, since this means the
     * end of the current interval (and start to the next interval).
     * @param fraction a value between 0 and 1, representing the elapsed
     * fraction of a time interval (either an entire animation cycle or an 
     * interval between two KeyTimes, depending on where this Interpolator has
     * been set)
     * @return number representing the start of the current interval, usually
     * 0, but if <code>fracton == 0</code>, returns 1.
     */
    public float interpolate(float fraction) {
        if (fraction < 1.0f) {
            return 0;
        }
        return 1.0f;
    }
    
}
