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

import java.beans.PropertyDescriptor;
import java.lang.reflect.Method;
import java.util.Locale;

import org.jdesktop.animation.timing.Animator;
import org.jdesktop.animation.timing.TimingTargetAdapter;

/**
 * This class enables automating animation of object properties.
 * The class is a TimingTarget, and should be used as a target of
 * timing events from an Animator.  These events will be used to 
 * change a specified property over time, according to how the
 * PropertySetter is constructed.
 * <p>
 * For example, here is an animation of the "background" property
 * of some object "obj" from blue to red over a period of one second:
 * <pre>
 *  PropertySetter ps = new PropertySetter(obj, "background", Color.BLUE, 
 *                                         Color.RED);
 *  Animator anim = new Animator(1000, ps);
 *  anim.start();
 * </pre>
 * Here is the same animation, created using one of the utility
 * factory methods that returns an animator:
 * <pre>
 *  Animator animator = PropertySetter.createAnimator(1000, obj, "background", 
 *                                                    Color.BLUE, Color.RED);
 *  anim.start();
 * </pre>
 * <p>
 * More complex animations can be created by passing in multiple values
 * for the property to take on, for example:
 * <pre>
 *  Animator animator = PropertySetter.createAnimator(1000, obj, "background", 
 *                                                    Color.BLUE, Color.RED, 
 *                                                    Color.GREEN);
 *  anim.start();
 * </pre>
 * It is also possible to define more involved and tightly-controlled
 * steps in the animation, including the times between the values and
 * how the values are interpolated by using the constructor that takes
 * a {@link KeyFrames} object.  KeyFrames defines the fractional times at which
 * an object takes on specific values, the values to assume at those times,
 * and the method of interpolation between those values.  For example,
 * here is the same animation as above, specified through KeyFrames, where the
 * RED color will be set 10% of the way through the animation (note that
 * we are not setting an Interpolator, so the timing intervals will use the
 * default LinearInterpolator):
 * <pre>
 *  KeyValues vals = KeyValues.create(Color.BLUE, Color.RED, Color.GREEN);
 *  KeyTimes times = new KeyTimes(0.0f, .1f, 1.0f);
 *  KeyFrames frames = new KeyFrames(vals, times);
 *  Animator animator = PropertySetter.createAnimator(1000, obj, "background", 
 *                                                    frames);
 *  anim.start();
 * </pre>
 *
 * @author Chet
 */
public class PropertySetter extends TimingTargetAdapter {

    private Object object;
    private String propertyName;
    private KeyFrames keyFrames;
    private Method propertySetter;
    private Method propertyGetter;

    /**
     * Utility method that constructs a PropertySetter and an Animator using
     * that PropertySetter and returns the Animator
     * @param duration the duration, in milliseconds, of the animation
     * @param object the object whose property will be animated
     * @param propertyName the name of the property to be animated.  For
     * any propertyName "foo" there must be an accessible "setFoo" method
     * on the object.  If only one value is supplied in creating the
     * KeyValues for the keyFrames, the animation
     * will also need a "getFoo" method.
     * @param keyFrames the fractional times, values, and interpolation
     * to be used in calculating the values set on the object's property.
     * @throws IllegalArgumentException if appropriate set/get methods
     * cannot be found for propertyName.
     */
    public static Animator createAnimator(int duration, Object object, 
            String propertyName, KeyFrames keyFrames) {
        PropertySetter ps = new PropertySetter(object, propertyName, keyFrames);
        Animator animator = new Animator(duration, ps);
        return animator;
    }

    /**
     * Utility method that constructs a PropertySetter and an Animator using
     * that PropertySetter and returns the Animator
     * @param duration the duration, in milliseconds, of the animation
     * @param object the object whose property will be animated
     * @param propertyName the name of the property to be animated.  For
     * any propertyName "foo" there must be an accessible "setFoo" method
     * on the object.  If only one value is supplied in creating the
     * KeyValues for the keyFrames, the animation
     * will also need a "getFoo" method.
     * @param params the values that the object will take on during the
     * animation.  Internally, a KeyFrames object will be created that
     * will use times that split the total duration evenly. Supplying
     * only one value for params implies that this is a "to" animation
     * whose intial value will be determined dynamically when the animation
     * starts.
     * @throws IllegalArgumentException if appropriate set/get methods
     * cannot be found for propertyName.
     */
    public static <T> Animator createAnimator(int duration, 
            Object object, String propertyName, T... params) {
        PropertySetter ps = new PropertySetter(object, propertyName, params);
        Animator animator = new Animator(duration, ps);
        return animator;
    }
    
    /**
     * Utility method that constructs a PropertySetter and an Animator using
     * that PropertySetter and returns the Animator
     * 
     * @param duration the duration, in milliseconds, of the animation
     * @param object the object whose property will be animated
     * @param propertyName the name of the property to be animated.  For
     * any propertyName "foo" there must be an accessible "setFoo" method
     * on the object.  If only one value is supplied in creating the
     * KeyValues for the keyFrames, the animation
     * will also need a "getFoo" method.
     * @param evaluator KeyValues knows how to calculate intermediate values
     * for many built-in types, but if you want to supply values in 
     * types not understood by KeyValues, you will need to supply your
     * own Evaluator.
     * @param params the values that the object will take on during the
     * animation.  Internally, a KeyFrames object will be created that
     * will use times that split the total duration evenly. Supplying
     * only one value for params implies that this is a "to" animation
     * whose intial value will be determined dynamically when the animation
     * starts.
     * @throws IllegalArgumentException if appropriate set/get methods
     * cannot be found for propertyName.
     */
    public static <T> Animator createAnimator(int duration, 
            Object object, String propertyName, 
            Evaluator evaluator, T... params) {
        PropertySetter ps = new PropertySetter(object, propertyName, evaluator, 
                params);
        Animator animator = new Animator(duration, ps);
        return animator;
    }

    /**
     * Constructor for a PropertySetter where the values the propert
     * takes on during the animation are specified in a {@link KeyFrames}
     * object.
     * @param object the object whose property will be animated
     * @param propertyName the name of the property to be animated.  For
     * any propertyName "foo" there must be an accessible "setFoo" method
     * on the object.  If only one value is supplied in creating the
     * KeyValues for the keyFrames, the animation
     * will also need a "getFoo" method.
     * @param keyFrames the fractional times, values, and interpolation
     * to be used in calculating the values set on the object's property.
     * @throws IllegalArgumentException if appropriate set/get methods
     * cannot be found for propertyName.
     */
    public PropertySetter(Object object, String propertyName,
            KeyFrames keyFrames) {
        this.object = object;
        this.propertyName = propertyName;
        this.keyFrames = keyFrames;
        try {
            setupMethodInfo();
        } catch (NoSuchMethodException e) {
            throw new IllegalArgumentException("Bad property name (" +
                    propertyName +"): could not find " +
                    "an appropriate setter or getter method for that property");
        }
    }
    
    /**
     * Constructor for a PropertySetter where the values the propert
     * takes on during the animation are specified in a {@link KeyFrames}
     * object.
     * @param object the object whose property will be animated
     * @param propertyName the name of the property to be animated.  For
     * any propertyName "foo" there must be an accessible "setFoo" method
     * on the object.  If only one value is supplied in params, the animation
     * will also need a "getFoo" method.
     * @param params the values that the object will take on during the
     * animation.  Internally, a KeyFrames object will be created that
     * will use times that split the total duration evenly. Supplying
     * only one value for params implies that this is a "to" animation
     * whose intial value will be determined dynamically when the animation
     * starts.
     * @throws IllegalArgumentException if appropriate set/get methods
     * cannot be found for propertyName.
     */
    public <T> PropertySetter(Object object, String propertyName, T... params) {
        this(object, propertyName, new KeyFrames(KeyValues.create(params)));
    }
    
    /**
     * Constructor for a PropertySetter where the values the propert
     * takes on during the animation are specified in a {@link KeyFrames}
     * object.
     * 
     * @param object the object whose property will be animated
     * @param propertyName the name of the property to be animated.  For
     * any propertyName "foo" there must be an accessible "setFoo" method
     * on the object.  If only one value is supplied in params, the animation
     * will also need a "getFoo" method.
     * @param evaluator KeyValues knows how to calculate intermediate values
     * for many built-in types, but if you want to supply values in 
     * types not understood by KeyValues, you will need to supply your
     * own Evaluator.
     * @param params the values that the object will take on during the
     * animation.  Internally, a KeyFrames object will be created that
     * will use times that split the total duration evenly. Supplying
     * only one value for params implies that this is a "to" animation
     * whose intial value will be determined dynamically when the animation
     * starts.
     * @throws IllegalArgumentException if appropriate set/get methods
     * cannot be found for propertyName.
     */
    public <T> PropertySetter(Object object, String propertyName, 
            Evaluator evaluator, T... params) {
        this(object, propertyName, 
                new KeyFrames(KeyValues.create(evaluator, params)));
    }
    
    /**
     * Translates the property name used in the PropertyRange object into
     * the appropriate Method in the Object to be modified.  This uses
     * standard JavaBean naming convention (e.g., propertyName would
     * become setPropertyName).
     * @throws NoSuchMethodException if there is no method on the
     * object with the appropriate name
     * @throws SecurityException if the application does not have
     * appropriate permissions to request access to the Method
     */
    private void setupMethodInfo() throws NoSuchMethodException {
        try {
            String firstChar = propertyName.substring(0, 1);
            String remainder = propertyName.substring(1);
            Class propertyType = getType();
            String propertySetterName = "set" + firstChar.toUpperCase(Locale.ENGLISH) + remainder;

            PropertyDescriptor prop = new PropertyDescriptor(propertyName, object.getClass(),
                    null, propertySetterName);
            propertySetter = prop.getWriteMethod();
            if (isToAnimation()) {
                // Only need the getter for "to" animations
                String propertyGetterName = "get" + firstChar.toUpperCase(Locale.ENGLISH) + 
                        remainder;
                prop = new PropertyDescriptor(propertyName, 
                        object.getClass(), propertyGetterName, null);
                propertyGetter = prop.getReadMethod();
            }
        } catch (Exception e) {
            throw new NoSuchMethodException("Cannot find property methods: " + e);
        }
    }
    
    //
    // TimingTargetAdapter overrides
    //
    
    /**
     * Called by Animator to signal that the timer is about to start.
     * The only operation performed in this method is setting an initial
     * value for the animation if appropriate; this accounts
     * for "to" animations, which need to start from the current value.
     * <p>
     * This method is not intended for use by application code.
     */
    public void begin() {
        if (isToAnimation()) {
            try {
                setStartValue(propertyGetter.invoke(object));
            } catch (Exception e) {
                System.out.println("Problem setting start value on object " +
                        object + ": " + e);
            }
        }
    }

    /**
     * Called from Animator to signal a timing event.  This
     * causes PropertySetter to invoke the property-setting method (as 
     * specified by the propertyName in the constructor) with the
     * appropriate value of the property given the range of values in the
     * KeyValues object and the fraction of the timing cycle that has
     * elapsed.
     * <p>
     * This method is not intended for use by application code.
     */
    public void timingEvent(float fraction) {
        try {
            setValue(object, propertySetter, fraction);
        } catch (Exception e) {
            System.out.println("Problem calling setValue in " +
                    "PropertySetter.timingEvent: " + e);
        }
    }
    
    private String getPropertyName() {
        return propertyName;
    }
    
    /**
     * Called during begin() if this is a "to" animation, to set the start
     * value of the animation to whatever the current value is.
     */
    private void setStartValue(Object object) {
        keyFrames.getKeyValues().setStartValue(object);
    }
    
    /**
     * Sets the appropriate value on the property given the current fraction
     */
    private void setValue(Object object, Method method, float fraction) {
        try {
            method.invoke(object, keyFrames.getValue(fraction));
        } catch (Exception e) {
            System.out.println("Problem invoking method " +
                    propertySetter + " in object " + object + 
                    " in setValue" + e);
        }
    }
        
    /**
     * Returns the type used in this property setter (defers to KeyFrames
     * for this information).
     */
    private Class getType() {
        return keyFrames.getType();
    }
    
    /**
     * Utility method for determining whether this is a "to" animation
     * (true if the first value is null).
     */
    private boolean isToAnimation() {
        return (keyFrames.getKeyValues().isToAnimation());
    }
    
}
