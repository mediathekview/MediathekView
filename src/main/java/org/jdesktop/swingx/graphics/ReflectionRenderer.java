/*
 * $Id: ReflectionRenderer.java 4082 2011-11-15 18:39:43Z kschaefe $
 *
 * Dual-licensed under LGPL (Sun and Romain Guy) and BSD (Romain Guy).
 *
 * Copyright 2006 Sun Microsystems, Inc., 4150 Network Circle,
 * Santa Clara, California 95054, U.S.A. All rights reserved.
 *
 * Copyright (c) 2006 Romain Guy <romain.guy@mac.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.jdesktop.swingx.graphics;

import org.jdesktop.swingx.image.StackBlurFilter;
import org.jdesktop.swingx.util.GraphicsUtilities;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;

/**
 * <p>A reflection renderer generates the reflection of a given picture. The
 * result can be either the reflection itself, or an image containing both the
 * source image and its reflection.</p>
 * <h2>Reflection Properties</h2>
 * <p>A reflection is defined by three properties:
 * <ul>
 *   <li><i>opacity</i>: the opacity of the reflection. You will usually
 *   change this valued according to the background color.</li>
 *   <li><i>length</i>: the length of the reflection. The length is a fraction
 *   of the height of the source image.</li>
 *   <li><i>blur enabled</i>: perfect reflections are hardly natural. You can
 *    blur the reflection to make it look a bit more natural.</li>
 * </ul>
 * You can set these properties using the provided mutators or the appropriate
 * constructor. Here are two ways of creating a blurred reflection, with an
 * opacity of 50% and a length of 30% the height of the original image:
 * <pre>
 * ReflectionRenderer renderer = new ReflectionRenderer(0.5f, 0.3f, true);
 * // ..
 * renderer = new ReflectionRenderer();
 * renderer.setOpacity(0.5f);
 * renderer.setLength(0.3f);
 * renderer.setBlurEnabled(true);
 * </pre>
 * The default constructor provides the following default values:
 * <ul>
 *   <li><i>opacity</i>: 35%</li>
 *   <li><i>length</i>: 40%</li>
 *   <li><i>blur enabled</i>: false</li>
 * </ul></p>
 * <h2>Generating Reflections</h2>
 * <p>A reflection is generated as a <code>BufferedImage</code> from another
 * <code>BufferedImage</code>. Once the renderer is set up, you must call
 * {@link #createReflection(BufferedImage)} to actually generate
 * the reflection:
 * <pre>
 * ReflectionRenderer renderer = new ReflectionRenderer();
 * // renderer setup
 * BufferedImage reflection = renderer.createReflection(bufferedImage);
 * </pre></p>
 * <p>The returned image contains only the reflection. You will have to append
 * it to the source image at painting time to get a realistic results. You can
 * also asks the rendered to return a picture composed of both the source image
 * and its reflection:
 * <pre>
 * ReflectionRenderer renderer = new ReflectionRenderer();
 * // renderer setup
 * BufferedImage reflection = renderer.appendReflection(bufferedImage);
 * </pre></p>
 * <h2>Properties Changes</h2>
 * <p>This renderer allows to register property change listeners with
 * {@link #addPropertyChangeListener}. Listening to properties changes is very
 * useful when you embed the renderer in a graphical component and give the API
 * user the ability to access the renderer. By listening to properties changes,
 * you can easily repaint the component when needed.</p>
 * <h2>Threading Issues</h2>
 * <p><code>ReflectionRenderer</code> is not guaranteed to be thread-safe.</p>
 *
 * @author Romain Guy <romain.guy@mac.com>
 */
public class ReflectionRenderer {
    /**
     * <p>Identifies a change to the opacity used to render the reflection.</p>
     */
    public static final String OPACITY_CHANGED_PROPERTY = "reflection_opacity";

    /**
     * <p>Identifies a change to the length of the rendered reflection.</p>
     */
    public static final String LENGTH_CHANGED_PROPERTY = "reflection_length";

    /**
     * <p>Identifies a change to the blurring of the rendered reflection.</p>
     */
    public static final String BLUR_ENABLED_CHANGED_PROPERTY = "reflection_blur";

    // opacity of the reflection
    private float opacity;

    // length of the reflection
    private float length;

    // should the reflection be blurred?
    private boolean blurEnabled;

    // notifies listeners of properties changes
    private PropertyChangeSupport changeSupport;
    private StackBlurFilter stackBlurFilter;

    /**
     * <p>Creates a default good looking reflections generator.
     * The default reflection renderer provides the following default values:
     * <ul>
     *   <li><i>opacity</i>: 35%</li>
     *   <li><i>length</i>: 40%</li>
     *   <li><i>blurring</i>: disabled with a radius of 1 pixel</li>
     * </ul></p>
     * <p>These properties provide a regular, good looking reflection.</p>
     *
     * @see #getOpacity()
     * @see #setOpacity(float)
     * @see #getLength()
     * @see #setLength(float)
     * @see #isBlurEnabled()
     * @see #setBlurEnabled(boolean)
     * @see #getBlurRadius()
     * @see #setBlurRadius(int)
     */
    public ReflectionRenderer() {
        this(0.35f, 0.4f, false);
    }

    /**
     * <p>Creates a default good looking reflections generator with the
     * specified opacity. The default reflection renderer provides the following
     * default values:
     * <ul>
     *   <li><i>length</i>: 40%</li>
     *   <li><i>blurring</i>: disabled with a radius of 1 pixel</li>
     * </ul></p>
     *
     * @param opacity the opacity of the reflection, between 0.0 and 1.0
     * @see #getOpacity()
     * @see #setOpacity(float)
     * @see #getLength()
     * @see #setLength(float)
     * @see #isBlurEnabled()
     * @see #setBlurEnabled(boolean)
     * @see #getBlurRadius()
     * @see #setBlurRadius(int)
     */
    public ReflectionRenderer(float opacity) {
        this(opacity, 0.4f, false);
    }

    /**
     * <p>Creates a reflections generator with the specified properties. Both
     * opacity and length are numbers between 0.0 (0%) and 1.0 (100%). If the
     * provided numbers are outside this range, they are clamped.</p>
     * <p>Enabling the blur generates a different kind of reflections that might
     * look more natural. The default blur radius is 1 pixel</p>
     *
     * @param opacity the opacity of the reflection
     * @param length the length of the reflection
     * @param blurEnabled if true, the reflection is blurred
     * @see #getOpacity()
     * @see #setOpacity(float)
     * @see #getLength()
     * @see #setLength(float)
     * @see #isBlurEnabled()
     * @see #setBlurEnabled(boolean)
     * @see #getBlurRadius()
     * @see #setBlurRadius(int)
     */
    public ReflectionRenderer(float opacity, float length, boolean blurEnabled) {
        //noinspection ThisEscapedInObjectConstruction
        this.changeSupport = new PropertyChangeSupport(this);
        this.stackBlurFilter = new StackBlurFilter(1);

        setOpacity(opacity);
        setLength(length);
        setBlurEnabled(blurEnabled);
    }

    /**
     * <p>Add a PropertyChangeListener to the listener list. The listener is
     * registered for all properties. The same listener object may be added
     * more than once, and will be called as many times as it is added. If
     * <code>listener</code> is null, no exception is thrown and no action
     * is taken.</p>
     *
     * @param listener the PropertyChangeListener to be added
     */
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        changeSupport.addPropertyChangeListener(listener);
    }

    /**
     * <p>Remove a PropertyChangeListener from the listener list. This removes
     * a PropertyChangeListener that was registered for all properties. If
     * <code>listener</code> was added more than once to the same event source,
     * it will be notified one less time after being removed. If
     * <code>listener</code> is null, or was never added, no exception is thrown
     * and no action is taken.</p>
     *
     * @param listener the PropertyChangeListener to be removed
     */
    public void removePropertyChangeListener(PropertyChangeListener listener) {
        changeSupport.removePropertyChangeListener(listener);
    }

    /**
     * <p>Gets the opacity used by the factory to generate reflections.</p>
     * <p>The opacity is comprised between 0.0f and 1.0f; 0.0f being fully
     * transparent and 1.0f fully opaque.</p>
     *
     * @return this factory's shadow opacity
     * @see #getOpacity()
     * @see #createReflection(BufferedImage)
     * @see #appendReflection(BufferedImage)
     */
    public float getOpacity() {
        return opacity;
    }

    /**
     * <p>Sets the opacity used by the factory to generate reflections.</p>
     * <p>Consecutive calls to {@link #createReflection} will all use this
     * opacity until it is set again.</p>
     * <p>The opacity is comprised between 0.0f and 1.0f; 0.0f being fully
     * transparent and 1.0f fully opaque. If you provide a value out of these
     * boundaries, it will be restrained to the closest boundary.</p>
     *
     * @param opacity the generated reflection opacity
     * @see #setOpacity(float)
     * @see #createReflection(BufferedImage)
     * @see #appendReflection(BufferedImage)
     */
    public void setOpacity(float opacity) {
        float oldOpacity = this.opacity;

        if (opacity < 0.0f) {
            opacity = 0.0f;
        } else if (opacity > 1.0f) {
            opacity = 1.0f;
        }

        if (oldOpacity != opacity) {
            this.opacity = opacity;
            changeSupport.firePropertyChange(OPACITY_CHANGED_PROPERTY,
                                             oldOpacity,
                                             this.opacity);
        }
    }

    /**
     * <p>Returns the length of the reflection. The result is a number between
     * 0.0 and 1.0. This number is the fraction of the height of the source
     * image that is used to compute the size of the reflection.</p>
     *
     * @return the length of the reflection, as a fraction of the source image
     *   height
     * @see #setLength(float)
     * @see #createReflection(BufferedImage)
     * @see #appendReflection(BufferedImage)
     */
    public float getLength() {
        return length;
    }

    /**
     * <p>Sets the length of the reflection, as a fraction of the height of the
     * source image.</p>
     * <p>Consecutive calls to {@link #createReflection} will all use this
     * opacity until it is set again.</p>
     * <p>The opacity is comprised between 0.0f and 1.0f; 0.0f being fully
     * transparent and 1.0f fully opaque. If you provide a value out of these
     * boundaries, it will be restrained to the closest boundary.</p>
     *
     * @param length the length of the reflection, as a fraction of the source
     *   image height
     * @see #getLength()
     * @see #createReflection(BufferedImage)
     * @see #appendReflection(BufferedImage)
     */
    public void setLength(float length) {
        float oldLength = this.length;

        if (length < 0.0f) {
            length = 0.0f;
        } else if (length > 1.0f) {
            length = 1.0f;
        }

        if (oldLength != length) {
            this.length = length;
            changeSupport.firePropertyChange(LENGTH_CHANGED_PROPERTY,
                                             oldLength,
                                             this.length);
        }
    }

    /**
     * <p>Returns true if the blurring of the reflection is enabled, false
     * otherwise. When blurring is enabled, the reflection is blurred to look
     * more natural.</p>
     *
     * @return true if blur is enabled, false otherwise
     * @see #setBlurEnabled(boolean)
     * @see #createReflection(BufferedImage)
     * @see #appendReflection(BufferedImage)
     */
    public boolean isBlurEnabled() {
        return blurEnabled;
    }

    /**
     * <p>Setting the blur to true will enable the blurring of the reflection
     * when {@link #createReflection} is invoked.</p>
     * <p>Enabling the blurring of the reflection can yield to more natural
     * results which may or may not be better looking, depending on the source
     * picture.</p>
     * <p>Consecutive calls to {@link #createReflection} will all use this
     * opacity until it is set again.</p>
     *
     * @param blurEnabled true to enable the blur, false otherwise
     * @see #isBlurEnabled()
     * @see #createReflection(BufferedImage)
     * @see #appendReflection(BufferedImage)
     */
    public void setBlurEnabled(boolean blurEnabled) {
        if (blurEnabled != this.blurEnabled) {
            boolean oldBlur = this.blurEnabled;
            this.blurEnabled= blurEnabled;

            changeSupport.firePropertyChange(BLUR_ENABLED_CHANGED_PROPERTY,
                                             oldBlur,
                                             this.blurEnabled);
        }
    }

    /**
     * <p>Returns the effective radius, in pixels, of the blur used by this
     * renderer when {@link #isBlurEnabled()} is true.</p>
     *
     * @return the effective radius of the blur used when
     *   <code>isBlurEnabled</code> is true
     * @see #isBlurEnabled()
     * @see #setBlurEnabled(boolean)
     * @see #setBlurRadius(int)
     * @see #getBlurRadius()
     */
    public int getEffectiveBlurRadius() {
        return stackBlurFilter.getEffectiveRadius();
    }

    /**
     * <p>Returns the radius, in pixels, of the blur used by this renderer when
     * {@link #isBlurEnabled()} is true.</p>
     *
     * @return the radius of the blur used when <code>isBlurEnabled</code>
     *         is true
     * @see #isBlurEnabled()
     * @see #setBlurEnabled(boolean)
     * @see #setBlurRadius(int)
     * @see #getEffectiveBlurRadius()
     */
    public int getBlurRadius() {
        return stackBlurFilter.getRadius();
    }

    /**
     * <p>Sets the radius, in pixels, of the blur used by this renderer when
     * {@link #isBlurEnabled()} is true. This radius changes the size of the
     * generated image when blurring is applied.</p>
     *
     * @param radius the radius, in pixels, of the blur
     * @see #isBlurEnabled()
     * @see #setBlurEnabled(boolean)
     * @see #getBlurRadius()
     */
    public void setBlurRadius(int radius) {
        this.stackBlurFilter = new StackBlurFilter(radius);
    }

    /**
     * <p>Returns the source image and its reflection. The appearance of the
     * reflection is defined by the opacity, the length and the blur
     * properties.</p>
     * * <p>The width of the generated image will be augmented when
     * {@link #isBlurEnabled()} is true. The generated image will have the width
     * of the source image plus twice the effective blur radius (see
     * {@link #getEffectiveBlurRadius()}). The default blur radius is 1 so the
     * width will be augmented by 6. You might need to take this into account
     * at drawing time.</p>
     * <p>The returned image height depends on the value returned by
     * {@link #getLength()} and {@link #getEffectiveBlurRadius()}. For instance,
     * if the length is 0.5 (or 50%) and the source image is 480 pixels high,
     * then the reflection will be 246 (480 * 0.5 + 3 * 2) pixels high.</p>
     * <p>You can create only the reflection by calling
     * {@link #createReflection(BufferedImage)}.</p>
     *
     * @param image the source image
     * @return the source image with its reflection below
     * @see #createReflection(BufferedImage)
     */
    public BufferedImage appendReflection(BufferedImage image) {
        BufferedImage reflection = createReflection(image);
        BufferedImage buffer = GraphicsUtilities.createCompatibleTranslucentImage(
                reflection.getWidth(), image.getHeight() + reflection.getHeight());
        Graphics2D g2 = buffer.createGraphics();

        try {
            int effectiveRadius = isBlurEnabled() ? stackBlurFilter
                    .getEffectiveRadius() : 0;
            g2.drawImage(image, effectiveRadius, 0, null);
            g2.drawImage(reflection, 0, image.getHeight() - effectiveRadius,
                    null);
        } finally {
            g2.dispose();
        }
        
        reflection.flush();

        return buffer;
    }

    /**
     * <p>Returns the reflection of the source image. The appearance of the
     * reflection is defined by the opacity, the length and the blur
     * properties.</p>
     * * <p>The width of the generated image will be augmented when
     * {@link #isBlurEnabled()} is true. The generated image will have the width
     * of the source image plus twice the effective blur radius (see
     * {@link #getEffectiveBlurRadius()}). The default blur radius is 1 so the
     * width will be augmented by 6. You might need to take this into account
     * at drawing time.</p>
     * <p>The returned image height depends on the value returned by
     * {@link #getLength()} and {@link #getEffectiveBlurRadius()}. For instance,
     * if the length is 0.5 (or 50%) and the source image is 480 pixels high,
     * then the reflection will be 246 (480 * 0.5 + 3 * 2) pixels high.</p>
     * <p>The returned image contains <strong>only</strong>
     * the reflection. You will have to append it to the source image to produce
     * the illusion of a reflective environment. The method
     * {@link #appendReflection(BufferedImage)} provides an easy
     * way to create an image containing both the source and the reflection.</p>
     *
     * @param image the source image
     * @return the reflection of the source image
     * @see #appendReflection(BufferedImage)
     */
    public BufferedImage createReflection(BufferedImage image) {
        if (length == 0.0f) {
            return GraphicsUtilities.createCompatibleTranslucentImage(1, 1);
        }

        int blurOffset = isBlurEnabled() ?
                         stackBlurFilter.getEffectiveRadius() : 0;
        int height = (int) (image.getHeight() * length);

        BufferedImage buffer =
                GraphicsUtilities.createCompatibleTranslucentImage(
                        image.getWidth() + blurOffset * 2,
                        height + blurOffset * 2);
        Graphics2D g2 = buffer.createGraphics();

        try {
            g2.translate(0, image.getHeight());
            g2.scale(1.0, -1.0);

            g2.drawImage(image, blurOffset, -blurOffset, null);

            g2.scale(1.0, -1.0);
            g2.translate(0, -image.getHeight());

            g2.setComposite(AlphaComposite.DstIn);
            g2.setPaint(new GradientPaint(0.0f, 0.0f, new Color(0.0f, 0.0f,
                    0.0f, getOpacity()), 0.0f, buffer.getHeight(), new Color(
                    0.0f, 0.0f, 0.0f, 0.0f), true));
            g2.fillRect(0, 0, buffer.getWidth(), buffer.getHeight());
        } finally {
            g2.dispose();
        }
        
        return isBlurEnabled() ? stackBlurFilter.filter(buffer, null) :
                buffer;
    }
}
