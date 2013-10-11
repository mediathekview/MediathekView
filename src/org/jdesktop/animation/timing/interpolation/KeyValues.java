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

import java.awt.Point;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Stores a list of values that correspond to the times in a {@link
 * KeyTimes} object.  These structures are then used to create a
 * {@link KeyFrames} object, which is then used to create a
 * {@link PropertySetter} for the purposes of modifying an object's
 * property over time.
 * <p>
 * At each of the times in {@link KeyTimes}, the property will take
 * on the corresponding value in the KeyValues object.  Between these
 * times, the property will take on a value based on the interpolation
 * information stored in the KeyFrames object and the {@link 
 * Evaluator} for the type of the values in KeyValues.
 * <p>
 * This class has built-in support for various known types, as defined
 * in {@link Evaluator}.
 * <p>
 * For a simple example using KeyValues to create a KeyFrames and 
 * PropertySetter object, see the class header comments in 
 * {@link PropertySetter}.
 * 
 * 
 * @author Chet
 */
public class KeyValues<T> {

    private final List<T> values = new ArrayList<T>();
    private final Evaluator<T> evaluator;
    private final Class<?> type;
    private T startValue;

    /**
     * Constructs a KeyValues object from one or more values.  The
     * internal Evaluator is automatically determined by the
     * type of the parameters.
     * 
     * @param params the values to interpolate between.  If there is only
     * one parameter, this is assumed to be a "to" animation where the
     * first value is dynamically determined at runtime when the animation
     * is started.
     * @throws IllegalArgumentException if an {@link Evaluator} cannot be 
     * found that can interpolate between the value types supplied
     */
    public static <T> KeyValues<T> create(T... params) {
        return new KeyValues(params);
    }

    /**
     * Constructs a KeyValues object from a Evaluator
     * and one or more values.
     * 
     * @param params the values to interpolate between.  If there is only
     * one parameter, this is assumed to be a "to" animation where the
     * first value is dynamically determined at runtime when the animation
     * is started.
     * @throws IllegalArgumentException if params does not have at least
     * one value.
     */
    public static <T> KeyValues<T> create(Evaluator evaluator, T... params) {
        return new KeyValues(evaluator, params);
    }

    /**
     * Private constructor, called by factory method
     */
    private KeyValues(T... params) {
        this(Evaluator.create(params.getClass().getComponentType()),
                params);
    }

    /**
     * Private constructor, called by factory method
     */
    private KeyValues(Evaluator evaluator, T... params) {
        if (params == null) {
            throw new IllegalArgumentException("params array cannot be null");
        } else if (params.length == 0) {
            throw new IllegalArgumentException(
                "params array must have at least one element");
        }
        if (params.length == 1) {
            // this is a "to" animation; set first element to null
            values.add(null);
        }
        Collections.addAll(values, params);
        this.type = params.getClass().getComponentType();
        this.evaluator = evaluator;
    }
    
    /**
     * Returns the number of values stored in this object.
     *
     * @return the number of values stored in this object
     */
    int getSize() {
        return values.size();
    }

    /**
     * Returns the data type of the values stored in this object.
     *
     * @return a Class value representing the type of values stored in this
     *         object
     */
    Class<?> getType() {
        return this.type;
    }

    /**
     * Called at start of animation; sets starting value in simple
     * "to" animations.
     */
    void setStartValue(T startValue) {
        if (isToAnimation()) {
            this.startValue = startValue;
        }
    }
    
    /**
     * Utility method for determining whether this is a "to" animation
     * (true if the first value is null).
     */
    boolean isToAnimation() {
        return (values.get(0) == null);
    }

    /**
     * Returns value calculated from the value at the lower index, the
     * value at the upper index, the fraction elapsed between these 
     * endpoints, and the evaluator set up by this object at construction
     * time.
     */
    T getValue(int i0, int i1, float fraction) {
        T value;
        T lowerValue = values.get(i0);
        if (lowerValue == null) {
            // "to" animation
            lowerValue = startValue;
        }
        if (i0 == i1) {
            // trivial case
            value = lowerValue;
        } else {
            T v0 = lowerValue;
            T v1 = values.get(i1);
            value = evaluator.evaluate(v0, v1, fraction);
        }
        return value;
    }
}
