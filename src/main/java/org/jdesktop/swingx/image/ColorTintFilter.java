/*
 * $Id: ColorTintFilter.java 4082 2011-11-15 18:39:43Z kschaefe $
 *
 * Dual-licensed under LGPL (Sun and Romain Guy) and BSD (Romain Guy).
 *
 * Copyright 2005 Sun Microsystems, Inc., 4150 Network Circle,
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

package org.jdesktop.swingx.image;

import org.jdesktop.swingx.util.GraphicsUtilities;

import java.awt.*;
import java.awt.image.BufferedImage;

/**
 * <p>A color tint filter can be used to mix a solid color to an image. The
 * result is an image tinted by the specified color. The force of the effect
 * can be controlled with the <code>mixValue</code>, a number between  0.0 and
 * 1.0 that can be seen as the percentage of the mix (0.0 does not affect the
 * source image and 1.0 replaces all the pixels by the solid color).</p>
 * <p>The color of the pixels in the resulting image is computed as follows:</p>
 * <pre>
 * cR = cS * (1 - mixValue) + cM * mixValue
 * </pre>
 * <p>Definition of the parameters:</p>
 * <ul>
 *   <li><code>cR</code>: color of the resulting pixel</li>
 *   <li><code>cS</code>: color of the source pixel</li>
 *   <li><code>cM</code>: the solid color to mix with the source image</li>
 *   <li><code>mixValue</code>: strength of the mix, a value between 0.0 and 1.0</li>
 * </ul>
 *
 * @author Romain Guy <romain.guy@mac.com>
 */

public class ColorTintFilter extends AbstractFilter {
    private final Color mixColor;
    private final float mixValue;

    private int[] preMultipliedRed;
    private int[] preMultipliedGreen;
    private int[] preMultipliedBlue;

    /**
     * <p>Creates a new color mixer filter. The specified color will be used
     * to tint the source image, with a mixing strength defined by
     * <code>mixValue</code>.</p>
     *
     * @param mixColor the solid color to mix with the source image
     * @param mixValue the strength of the mix, between 0.0 and 1.0; if the
     *   specified value lies outside this range, it is clamped
     * @throws IllegalArgumentException if <code>mixColor</code> is null
     */
    public ColorTintFilter(Color mixColor, float mixValue) {
        if (mixColor == null) {
            throw new IllegalArgumentException("mixColor cannot be null");
        }

        this.mixColor = mixColor;
        if (mixValue < 0.0f) {
            mixValue = 0.0f;
        } else if (mixValue > 1.0f) {
            mixValue = 1.0f;
        }
        this.mixValue = mixValue;

        int mix_r = (int) (mixColor.getRed()   * mixValue);
        int mix_g = (int) (mixColor.getGreen() * mixValue);
        int mix_b = (int) (mixColor.getBlue()  * mixValue);

        // Since we use only lookup tables to apply the filter, this filter
        // could be implemented as a LookupOp.
        float factor = 1.0f - mixValue;
        preMultipliedRed   = new int[256];
        preMultipliedGreen = new int[256];
        preMultipliedBlue  = new int[256];

        for (int i = 0; i < 256; i++) {
            int value = (int) (i * factor);
            preMultipliedRed[i]   = value + mix_r;
            preMultipliedGreen[i] = value + mix_g;
            preMultipliedBlue[i]  = value + mix_b;
        }
    }

    /**
     * <p>Returns the mix value of this filter.</p>
     *
     * @return the mix value, between 0.0 and 1.0
     */
    public float getMixValue() {
        return mixValue;
    }

    /**
     * <p>Returns the solid mix color of this filter.</p>
     *
     * @return the solid color used for mixing
     */
    public Color getMixColor() {
        return mixColor;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BufferedImage filter(BufferedImage src, BufferedImage dst) {
        if (dst == null) {
            dst = createCompatibleDestImage(src, null);
        }

        int width = src.getWidth();
        int height = src.getHeight();

        int[] pixels = new int[width * height];
        GraphicsUtilities.getPixels(src, 0, 0, width, height, pixels);
        mixColor(pixels);
        GraphicsUtilities.setPixels(dst, 0, 0, width, height, pixels);

        return dst;
    }

    private void mixColor(int[] pixels) {
        for (int i = 0; i < pixels.length; i++) {
            int argb = pixels[i];
            pixels[i] = (argb & 0xFF000000) |
                        preMultipliedRed[(argb >> 16)   & 0xFF] << 16 |
                        preMultipliedGreen[(argb >> 8)  & 0xFF] <<  8 |
                        preMultipliedBlue[argb & 0xFF];
        }
    }
}
