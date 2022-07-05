/*
 * $Id: ColorUtilities.java 1496 2006-10-22 03:26:24Z gfx $
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

package org.jdesktop.swingx.graphics;

import java.awt.*;

/**
 * <p><code>ColorUtilities</code> contains a set of tools to perform
 * common color operations easily.</p>
 *
 * @author Romain Guy <romain.guy@mac.com>
 */
public class ColorUtilities {
    private ColorUtilities() {
    }

    /**
     * <p>Returns the HSL (Hue/Saturation/Luminance) equivalent of a given
     * RGB color. All three HSL components are between 0.0 and 1.0.</p>
     *
     * @param color the RGB color to convert
     * @return a new array of 3 floats corresponding to the HSL components
     */
    public static float[] RGBtoHSL(Color color) {
        return RGBtoHSL(color.getRed(), color.getGreen(), color.getBlue(), null);
    }

    /**
     * <p>Returns the HSL (Hue/Saturation/Luminance) equivalent of a given
     * RGB color. All three HSL components are between 0.0 and 1.0.</p>
     *
     * @param color the RGB color to convert
     * @param hsl a pre-allocated array of floats; can be null
     * @return <code>hsl</code> if non-null, a new array of 3 floats otherwise
     * @throws IllegalArgumentException if <code>hsl</code> has a length lower
     *   than 3
     */
    public static float[] RGBtoHSL(Color color, float[] hsl) {
        return RGBtoHSL(color.getRed(), color.getGreen(), color.getBlue(), hsl);
    }

    /**
     * <p>Returns the HSL (Hue/Saturation/Luminance) equivalent of a given
     * RGB color. All three HSL components are between 0.0 and 1.0.</p>
     *
     * @param r the red component, between 0 and 255
     * @param g the green component, between 0 and 255
     * @param b the blue component, between 0 and 255
     * @return a new array of 3 floats corresponding to the HSL components
     */
    public static float[] RGBtoHSL(int r, int g, int b) {
        return RGBtoHSL(r, g, b, null);
    }

    /**
     * <p>Returns the HSL (Hue/Saturation/Luminance) equivalent of a given
     * RGB color. All three HSL components are floats between 0.0 and 1.0.</p>
     *
     * @param r the red component, between 0 and 255
     * @param g the green component, between 0 and 255
     * @param b the blue component, between 0 and 255
     * @param hsl a pre-allocated array of floats; can be null
     * @return <code>hsl</code> if non-null, a new array of 3 floats otherwise
     * @throws IllegalArgumentException if <code>hsl</code> has a length lower
     *   than 3
     */
    public static float[] RGBtoHSL(int r, int g, int b, float[] hsl) {
        if (hsl == null) {
            hsl = new float[3];
        } else if (hsl.length < 3) {
            throw new IllegalArgumentException("hsl array must have a length of" +
                                               " at least 3");
        }

        if (r < 0) r = 0;
        else if (r > 255) r = 255;
        if (g < 0) g = 0;
        else if (g > 255) g = 255;
        if (b < 0) b = 0;
        else if (b > 255) b = 255;

        float var_R = (r / 255f);
        float var_G = (g / 255f);
        float var_B = (b / 255f);

        float var_Min;
        float var_Max;
        float del_Max;

        if (var_R > var_G) {
            var_Min = var_G;
            var_Max = var_R;
        } else {
            var_Min = var_R;
            var_Max = var_G;
        }
        if (var_B > var_Max) {
            var_Max = var_B;
        }
        if (var_B < var_Min) {
            var_Min = var_B;
        }

        del_Max = var_Max - var_Min;

        float H, S, L;
        L = (var_Max + var_Min) / 2f;

        if (del_Max - 0.01f <= 0.0f) {
            H = 0;
            S = 0;
        } else {
            if (L < 0.5f) {
                S = del_Max / (var_Max + var_Min);
            } else {
                S = del_Max / (2 - var_Max - var_Min);
            }

            float del_R = (((var_Max - var_R) / 6f) + (del_Max / 2f)) / del_Max;
            float del_G = (((var_Max - var_G) / 6f) + (del_Max / 2f)) / del_Max;
            float del_B = (((var_Max - var_B) / 6f) + (del_Max / 2f)) / del_Max;

            if (var_R == var_Max) {
                H = del_B - del_G;
            } else if (var_G == var_Max) {
                H = (1 / 3f) + del_R - del_B;
            } else {
                H = (2 / 3f) + del_G - del_R;
            }
            if (H < 0) {
                H += 1;
            }
            if (H > 1) {
                H -= 1;
            }
        }

        hsl[0] = H;
        hsl[1] = S;
        hsl[2] = L;

        return hsl;
    }

    /**
     * <p>Returns the RGB equivalent of a given HSL (Hue/Saturation/Luminance)
     * color.</p>
     *
     * @param h the hue component, between 0.0 and 1.0
     * @param s the saturation component, between 0.0 and 1.0
     * @param l the luminance component, between 0.0 and 1.0
     * @return a new <code>Color</code> object equivalent to the HSL components
     */
    public static Color HSLtoRGB(float h, float s, float l) {
        int[] rgb = HSLtoRGB(h, s, l, null);
        return new Color(rgb[0], rgb[1], rgb[2]);
    }

    /**
     * <p>Returns the RGB equivalent of a given HSL (Hue/Saturation/Luminance)
     * color. All three RGB components are integers between 0 and 255.</p>
     *
     * @param h the hue component, between 0.0 and 1.0
     * @param s the saturation component, between 0.0 and 1.0
     * @param l the luminance component, between 0.0 and 1.0
     * @param rgb a pre-allocated array of ints; can be null
     * @return <code>rgb</code> if non-null, a new array of 3 ints otherwise
     * @throws IllegalArgumentException if <code>rgb</code> has a length lower
     *   than 3
     */
    public static int[] HSLtoRGB(float h, float s, float l, int[] rgb) {
        if (rgb == null) {
            rgb = new int[3];
        } else if (rgb.length < 3) {
            throw new IllegalArgumentException("rgb array must have a length of" +
                                               " at least 3");
        }

        if (h < 0) h = 0.0f;
        else if (h > 1.0f) h = 1.0f;
        if (s < 0) s = 0.0f;
        else if (s > 1.0f) s = 1.0f;
        if (l < 0) l = 0.0f;
        else if (l > 1.0f) l = 1.0f;

        int R, G, B;

        if (s - 0.01f <= 0.0f) {
            R = (int) (l * 255.0f);
            G = (int) (l * 255.0f);
            B = (int) (l * 255.0f);
        } else {
            float var_1, var_2;
            if (l < 0.5f) {
                var_2 = l * (1 + s);
            } else {
                var_2 = (l + s) - (s * l);
            }
            var_1 = 2 * l - var_2;

            R = (int) (255.0f * hue2RGB(var_1, var_2, h + (1.0f / 3.0f)));
            G = (int) (255.0f * hue2RGB(var_1, var_2, h));
            B = (int) (255.0f * hue2RGB(var_1, var_2, h - (1.0f / 3.0f)));
        }

        rgb[0] = R;
        rgb[1] = G;
        rgb[2] = B;

        return rgb;
    }

    private static float hue2RGB(float v1, float v2, float vH) {
        if (vH < 0.0f) {
            vH += 1.0f;
        }
        if (vH > 1.0f) {
            vH -= 1.0f;
        }
        if ((6.0f * vH) < 1.0f) {
            return (v1 + (v2 - v1) * 6.0f * vH);
        }
        if ((2.0f * vH) < 1.0f) {
            return (v2);
        }
        if ((3.0f * vH) < 2.0f) {
            return (v1 + (v2 - v1) * ((2.0f / 3.0f) - vH) * 6.0f);
        }
        return (v1);
    }
}
