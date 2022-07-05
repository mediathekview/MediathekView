/*
 * $Id: PaintUtils.java 4193 2012-06-27 19:42:05Z kschaefe $
 *
 * Copyright 2004 Sun Microsystems, Inc., 4150 Network Circle,
 * Santa Clara, California 95054, U.S.A. All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

package org.jdesktop.swingx.util;

import java.awt.*;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * A collection of utilities for working with Paints and Colors.
 *
 * @author Mark Davidson
 * @author joshua.marinacci@sun.com
 * @author Karl George Schaefer
 */
@SuppressWarnings("nls")
public class PaintUtils {
    public static final GradientPaint BLUE_EXPERIENCE = new GradientPaint(
            new Point2D.Double(0, 0),
            new Color(168, 204, 241),
            new Point2D.Double(0, 1),
            new Color(44, 61, 146));
    public static final GradientPaint MAC_OSX_SELECTED = new GradientPaint(
            new Point2D.Double(0, 0),
            new Color(81, 141, 236),
            new Point2D.Double(0, 1),
            new Color(36, 96, 192));
    public static final GradientPaint MAC_OSX = new GradientPaint(
            new Point2D.Double(0, 0),
            new Color(167, 210, 250),
            new Point2D.Double(0, 1),
            new Color(99, 147, 206));
    public static final GradientPaint AERITH = new GradientPaint(
            new Point2D.Double(0, 0),
            Color.WHITE,
            new Point2D.Double(0, 1),
            new Color(64, 110, 161));
    public static final GradientPaint GRAY = new GradientPaint(
            new Point2D.Double(0, 0),
            new Color(226, 226, 226),
            new Point2D.Double(0, 1),
            new Color(250, 248, 248));
    public static final GradientPaint RED_XP = new GradientPaint(
            new Point2D.Double(0, 0),
            new Color(236, 81, 81),
            new Point2D.Double(0, 1),
            new Color(192, 36, 36));
    public static final GradientPaint NIGHT_GRAY = new GradientPaint(
            new Point2D.Double(0, 0),
            new Color(102, 111, 127),
            new Point2D.Double(0, 1),
            new Color(38, 45, 61));
    public static final GradientPaint NIGHT_GRAY_LIGHT = new GradientPaint(
            new Point2D.Double(0, 0),
            new Color(129, 138, 155),
            new Point2D.Double(0, 1),
            new Color(58, 66, 82));
    
    
    //originally included in LinearGradientPainter
    public static final Paint ORANGE_DELIGHT = new LinearGradientPaint(
            new Point2D.Double(0, 0),
            new Point2D.Double(1, 0),
            new float[] {0f, .5f, .51f, 1f},
            new Color[] {
                new Color(248, 192, 75),
                new Color(253, 152, 6),
                new Color(243, 133, 0),
                new Color(254, 124, 0)});
    
    //originally included in LinearGradientPainter
    public static final Paint BLACK_STAR = new LinearGradientPaint(
            new Point2D.Double(0, 0),
            new Point2D.Double(1, 0),
            new float[] {0f, .5f, .51f, 1f},
            new Color[] {
                new Color(54, 62, 78),
                new Color(32, 39, 55),
                new Color(74, 82, 96),
                new Color(123, 132, 145)});
    
    private PaintUtils() {
    }
    
    /** Resizes a gradient to fill the width and height available. If the
     * gradient is left to right it will be resized to fill the entire width.
     * If the gradient is top to bottom it will be resized to fill the entire
     * height. If the gradient is on an angle it will be resized to go from
     * one corner to the other of the rectangle formed by (0,0 -> width,height).
     *
     * This method can resize java.awt.GradientPaint, java.awt.LinearGradientPaint,
     * and the LinearGradientPaint implementation from Apache's Batik project. Note,
     * this method does not require the MultipleGradientPaint.jar from Apache to
     * compile or to run. MultipleGradientPaint.jar *is* required if you want
     * to resize the LinearGradientPaint from that jar.
     *
     * Any paint passed into this method which is not a kind of gradient paint (like
     * a Color or TexturePaint) will be returned unmodified. It will not throw
     * an exception. If the gradient cannot be resized due to other errors the
     * original paint will be returned unmodified. It will not throw an
     * exception.
     *
     */
    public static Paint resizeGradient(Paint p, int width, int height) {
        if(p == null) return p;
        
        if(p instanceof GradientPaint) {
            GradientPaint gp = (GradientPaint)p;
            Point2D[] pts = new Point2D[2];
            pts[0] = gp.getPoint1();
            pts[1] = gp.getPoint2();
            pts = adjustPoints(pts, width, height);
            return new GradientPaint(pts[0], gp.getColor1(), pts[1], gp.getColor2(), gp.isCyclic());
        }
        
        if("java.awt.LinearGradientPaint".equals(p.getClass().getName()) ||
           "org.apache.batik.ext.awt.LinearGradientPaint".equals(p.getClass().getName())) {
            return resizeLinearGradient(p,width,height);
        }
        return p;
    }
    
    
    private static Paint resizeLinearGradient(Paint p, int width, int height) {
        try {
            Point2D[] pts = new Point2D[2];
            pts[0] = (Point2D) invokeMethod(p,"getStartPoint");
            pts[1] = (Point2D) invokeMethod(p,"getEndPoint");
            pts = adjustPoints(pts, width, height);
            float[] fractions = (float[]) invokeMethod(p,"getFractions");
            Color[] colors = (Color[]) invokeMethod(p,"getColors");
            
            Constructor<?> con = p.getClass().getDeclaredConstructor(
                    Point2D.class, Point2D.class,
                    new float[0].getClass(),
                    new Color[0].getClass());
            return (Paint) con.newInstance(pts[0],pts[1],fractions, colors);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return p;
    }
    
    private static Object invokeMethod(final Object p, final String methodName)
            throws NoSuchMethodException, InvocationTargetException, IllegalArgumentException, SecurityException, IllegalAccessException {
        Method meth = p.getClass().getMethod(methodName);
        return meth.invoke(p);
    }
    
    
    private static Point2D[] adjustPoints(Point2D[] pts, int width, int height) {
        Point2D start = pts[0];
        Point2D end = pts[1];
        
        double angle = calcAngle(start,end);
        double a2 = Math.toDegrees(angle);
        double e = 1;
        
        // if it is near 0 degrees
        if(Math.abs(angle) < Math.toRadians(e) ||
                Math.abs(angle) > Math.toRadians(360 - e)) {
            start = new Point2D.Float(0, 0);
            end = new Point2D.Float(normalize(end.getX(), width), 0);
        }

        // near 45
        if (isNear(a2, 45, e)) {
            start = new Point2D.Float(0, 0);
            end = new Point2D.Float(normalize(end.getX(), width), normalize(end.getY(), height));
        }

        // near 90
        if (isNear(a2, 90, e)) {
            start = new Point2D.Float(0, 0);
            end = new Point2D.Float(0, normalize(end.getY(), height));
        }

        // near 135
        if (isNear(a2, 135, e)) {
            start = new Point2D.Float(normalize(start.getX(), width), 0);
            end = new Point2D.Float(0, normalize(end.getY(), height));
        }

        // near 180
        if (isNear(a2, 180, e)) {
            start = new Point2D.Float(normalize(start.getX(), width), 0);
            end = new Point2D.Float(0, 0);
        }

        // near 225
        if (isNear(a2, 225, e)) {
            start = new Point2D.Float(normalize(start.getX(), width), normalize(start.getY(), height));
            end = new Point2D.Float(0, 0);
        }

        // near 270
        if (isNear(a2, 270, e)) {
            start = new Point2D.Float(0, normalize(start.getY(), height));
            end = new Point2D.Float(0, 0);
        }

        // near 315
        if (isNear(a2, 315, e)) {
            start = new Point2D.Float(0, normalize(start.getY(), height));
            end = new Point2D.Float(normalize(end.getX(), width), 0);
        }
        
        return new Point2D[] { start, end };
    }
    
    private static boolean isNear(double angle, double target, double error) {
        return Math.abs(target - Math.abs(angle)) < error;
    }
    
    private static float normalize(double original, float target) {
        if (original < 1f) {
            return target * (float) original;
        }
        
        return target;
    }
    
    private static double calcAngle(Point2D p1, Point2D p2) {
        double x_off = p2.getX() - p1.getX();
        double y_off = p2.getY() - p1.getY();
        double angle = Math.atan(y_off / x_off);
        if (x_off < 0) {
            angle = angle + Math.PI;
        }
        
        if(angle < 0) { angle+= 2*Math.PI; }
        if(angle > 2*Math.PI) { angle -= 2*Math.PI; }
        return angle;
    }
/*    
    public static void main(String ... args) {
        LinearGradientPaint in = new LinearGradientPaint(
                new Point(0,0), new Point(10,0),
                new float[] {0f, 0.5f, 1f},
                new Color[] {Color.RED, Color.GREEN, Color.BLUE});
        log.fine("in  = " + toString(in));
        Paint out = resizeGradient(in,100,100);
        log.fine(("out = " + toString((MultipleGradientPaint) out));
    }*/
    /*
    private static String toString(MultipleGradientPaint paint) {
        StringBuffer buffer = new StringBuffer();
        buffer.append(paint.getClass().getName());
        Color[] colors = paint.getColors();
        float[] values = paint.getFractions();
        buffer.append("[");
        for(int i=0; i<colors.length; i++) {
            buffer.append("#").append(Integer.toHexString(colors[i].getRGB()));
            buffer.append(":");
            buffer.append(values[i]);
            buffer.append(", ");
        }
        buffer.append("]");
        if(paint instanceof LinearGradientPaint) {
            LinearGradientPaint lgp = (LinearGradientPaint) paint;
            buffer.append(", ");
            buffer.append(""+lgp.getStartPoint().getX() + ", " + lgp.getStartPoint().getY());
            buffer.append("->");
            buffer.append(""+lgp.getEndPoint().getX() + ", " + lgp.getEndPoint().getY());
        }
        
        return buffer.toString();
    }*/

    /**
     * Creates a new {@code Paint} that is a checkered effect using the colors {@link Color#GRAY
     * gray} and {@link Color#WHITE}.
     * 
     * @return a the checkered paint
     */
    public static Paint getCheckerPaint() {
        return getCheckerPaint(Color.WHITE, Color.GRAY, 20);
    }

    /**
     * Creates a new {@code Paint} that is a checkered effect using the specified colors.
     * <p>
     * While this method supports transparent colors, this implementation performs painting
     * operations using the second color after it performs operations using the first color. This
     * means that to create a checkered paint with a fully-transparent color, you MUST specify that
     * color first.
     * 
     * @param c1
     *            the first color
     * @param c2
     *            the second color
     * @param size
     *            the size of the paint
     * @return a new {@code Paint} checkering the supplied colors
     */
    public static Paint getCheckerPaint(Paint c1, Paint c2, int size) {
        BufferedImage img = GraphicsUtilities.createCompatibleTranslucentImage(size, size);
        Graphics2D g = img.createGraphics();
        
        try {
            g.setPaint(c1);
            g.fillRect(0, 0, size, size);
            g.setPaint(c2);
            g.fillRect(0, 0, size / 2, size / 2);
            g.fillRect(size / 2, size / 2, size / 2, size / 2);
        } finally {
            g.dispose();
        }
        
        return new TexturePaint(img,new Rectangle(0,0,size,size));
    }

    /**
     * Creates a {@code String} that represents the supplied color as a
     * hex-value RGB triplet, including the "#". The return value is suitable
     * for use in HTML. The alpha (transparency) channel is neither include nor
     * used in producing the string.
     * 
     * @param color
     *            the color to convert
     * @return the hex {@code String}
     */
    public static String toHexString(Color color) {
        return "#" + Integer.toHexString(color.getRGB() | 0xFF000000).substring(2);
    }

    /**
     * Returns a new color equal to the old one, except that there is no alpha
     * (transparency) channel.
     * <p>
     * This method is a convenience and has the same effect as {@code
     * setAlpha(color, 255)}.
     * 
     * @param color
     *            the color to remove the alpha (transparency) from
     * @return a new non-transparent {@code Color}
     * @throws NullPointerException
     *             if {@code color} is {@code null}
     */
    public static Color removeAlpha(Color color) {
        return setAlpha(color, 255);
    }

    /**
     * Returns a new color equal to the old one, except alpha (transparency)
     * channel is set to the new value.
     * 
     * @param color
     *            the color to modify
     * @param alpha
     *            the new alpha (transparency) level. Must be an int between 0
     *            and 255
     * @return a new alpha-applied {@code Color}
     * @throws IllegalArgumentException
     *             if {@code alpha} is not between 0 and 255 inclusive
     * @throws NullPointerException
     *             if {@code color} is {@code null}
     */
    public static Color setAlpha(Color color, int alpha) {
        if (alpha < 0 || alpha > 255) {
            throw new IllegalArgumentException("invalid alpha value");
        }
    
        return new Color(
                color.getRed(), color.getGreen(), color.getBlue(), alpha);
    }

    /**
     * Returns a new color equal to the old one, except the saturation is set to
     * the new value. The new color will have the same alpha (transparency) as
     * the original color.
     * <p>
     * The color is modified using HSB calculations. The saturation must be a
     * float between 0 and 1. If 0 the resulting color will be gray. If 1 the
     * resulting color will be the most saturated possible form of the passed in
     * color.
     * 
     * @param color
     *            the color to modify
     * @param saturation
     *            the saturation to use in the new color
     * @return a new saturation-applied {@code Color}
     * @throws IllegalArgumentException
     *             if {@code saturation} is not between 0 and 1 inclusive
     * @throws NullPointerException
     *             if {@code color} is {@code null}
     */
    public static Color setSaturation(Color color, float saturation) {
        if (saturation < 0f || saturation > 1f) {
            throw new IllegalArgumentException("invalid saturation value");
        }
    
        int alpha = color.getAlpha();
        
        float[] hsb = Color.RGBtoHSB(
                color.getRed(), color.getGreen(), color.getBlue(), null);
        Color c = Color.getHSBColor(hsb[0], saturation, hsb[2]);
        
        return setAlpha(c, alpha);
    }

    /**
     * Returns a new color equal to the old one, except the brightness is set to
     * the new value. The new color will have the same alpha (transparency) as
     * the original color.
     * <p>
     * The color is modified using HSB calculations. The brightness must be a
     * float between 0 and 1. If 0 the resulting color will be black. If 1 the
     * resulting color will be the brightest possible form of the passed in
     * color.
     * 
     * @param color
     *            the color to modify
     * @param brightness
     *            the brightness to use in the new color
     * @return a new brightness-applied {@code Color}
     * @throws IllegalArgumentException
     *             if {@code brightness} is not between 0 and 1 inclusive
     * @throws NullPointerException
     *             if {@code color} is {@code null}
     */
    public static Color setBrightness(Color color, float brightness) {
        if (brightness < 0f || brightness > 1f) {
            throw new IllegalArgumentException("invalid brightness value");
        }
    
        int alpha = color.getAlpha();
    
        float[] hsb = Color.RGBtoHSB(
                color.getRed(), color.getGreen(), color.getBlue(), null);
        Color c = Color.getHSBColor(hsb[0], hsb[1], brightness);
    
        return setAlpha(c, alpha);
    }

    /**
     * Blends two colors to create a new color. The {@code origin} color is the
     * base for the new color and regardless of its alpha component, it is
     * treated as fully opaque (alpha 255).
     * 
     * @param origin
     *            the base of the new color
     * @param over
     *            the alpha-enabled color to add to the {@code origin} color
     * @return a new color comprised of the {@code origin} and {@code over}
     *         colors
     */
    public static Color blend(Color origin, Color over) {
        if (over == null) {
            return origin;
        }
    
        if (origin == null) {
            return over;
        }
    
        int a = over.getAlpha();
        
        int rb = (((over.getRGB() & 0x00ff00ff) * (a + 1))
                    + ((origin.getRGB() & 0x00ff00ff) * (0xff - a))) & 0xff00ff00;
        int g = (((over.getRGB() & 0x0000ff00) * (a + 1))
                    + ((origin.getRGB() & 0x0000ff00) * (0xff - a))) & 0x00ff0000;
    
        return new Color((over.getRGB() & 0xff000000) | ((rb | g) >> 8));
    }

    /**
     * Interpolates a color.
     * 
     * @param b
     *            the first color
     * @param a
     *            the second color
     * @param t
     *            the amount to interpolate
     * @return a new color
     */
        public static Color interpolate(Color b, Color a, float t) {
            float[] acomp = a.getRGBComponents(null);
            float[] bcomp = b.getRGBComponents(null);
            float[] ccomp = new float[4];
            
    //        log.fine(("a comp ");
    //        for(float f : acomp) {
    //            log.fine((f);
    //        }
    //        for(float f : bcomp) {
    //            log.fine((f);
    //        }
            for(int i=0; i<4; i++) {
                ccomp[i] = acomp[i] + (bcomp[i]-acomp[i])*t;
            }
    //        for(float f : ccomp) {
    //            log.fine((f);
    //        }
            
            return new Color(ccomp[0],ccomp[1],ccomp[2],ccomp[3]);
        }

    /**
     * Computes an appropriate foreground color (either white or black) for the
     * given background color.
     * 
     * @param bg
     *            the background color
     * @return {@code Color.WHITE} or {@code Color.BLACK}
     * @throws NullPointerException
     *             if {@code bg} is {@code null}
     */
    public static Color computeForeground(Color bg) {
        float[] rgb = bg.getRGBColorComponents(null);
        float y = .3f * rgb[0] + .59f * rgb[1] + .11f * rgb[2];
        
        return y > .5f ? Color.BLACK : Color.WHITE;
    }
}
