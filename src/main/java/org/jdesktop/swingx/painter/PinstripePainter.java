/*
 * $Id: PinstripePainter.java 4259 2012-11-14 20:07:00Z kschaefe $
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

package org.jdesktop.swingx.painter;

import org.jdesktop.beans.JavaBean;

import java.awt.*;
import java.awt.geom.Area;
import java.awt.geom.Line2D;

import static java.lang.Math.*;
import static org.jdesktop.swingx.painter.PainterUtils.getForegroundPaint;

/**
 * <p>
 * A fun Painter that paints pinstripes. You can specify the Paint to paint those pinstripes in
 * (could even be a texture paint!), the angle at which to paint the pinstripes, and the spacing
 * between stripes.
 * </p>
 * 
 * <p>
 * The default PinstripePainter configuration will paint the pinstripes using the foreground color
 * of the component (the default behavior if a Paint is not specified) at a 45 degree angle with 8
 * pixels between stripes
 * </p>
 * 
 * <p>
 * Here is a custom code snippet that paints Color.GRAY pinstripes at a 135 degree angle:
 * 
 * <pre>
 * <code>
 *  PinstripePainter p = new PinstripePainter();
 *  p.setAngle(135);
 *  p.setPaint(Color.GRAY);
 * </code>
 * </pre>
 * 
 * @author rbair
 */
@JavaBean
public class PinstripePainter extends AbstractPainter<Object> {
    /**
     * The angle in degrees to paint the pinstripes at. The default value is 45. The value will be
     * between 0 and 360 inclusive. The setAngle method will ensure this.
     */
    private double angle;
    
    /**
     * The spacing between pinstripes
     */
    private double spacing;

    /**
     * The stroke width of the pinstripes
     */
    private double stripeWidth;

    /**
     * The Paint to use when drawing the pinstripes
     */
    private Paint paint;

    /**
     * Create a new PinstripePainter. By default the angle with be 45 degrees, the spacing will be 8
     * pixels, and the color will be the Component foreground color.
     */
    public PinstripePainter() {
        this(null);
    }

    /**
     * Create a new PinstripePainter using an angle of 45, 8 pixel spacing, and the given Paint.
     * 
     * @param paint
     *            the paint used when drawing the stripes
     */
    public PinstripePainter(Paint paint) {
        this(paint, 45);
    }
    
    /**
     * Create a new PinstripePainter using the given angle, 8 pixel spacing, and the foreground
     * color of the Component
     * 
     * @param angle
     *            the angle, in degrees, in which to paint the pinstripes
     */
    public PinstripePainter(double angle) {
        this(null, angle);
    }

    /**
     * Create a new PinstripePainter using the given angle, 8 pixel spacing, and the given Paint
     * 
     * @param paint
     *            the paint used when drawing the stripes
     * @param angle
     *            the angle, in degrees, in which to paint the pinstripes
     */
    public PinstripePainter(Paint paint, double angle) {
        this(paint, angle, 1, 8);
    }

    /**
     * Create a new PinstripePainter with the specified paint, angle, stripe width, and stripe
     * spacing.
     * 
     * @param paint
     * @param angle
     * @param stripeWidth
     * @param spacing
     */
    public PinstripePainter(Paint paint, double angle, double stripeWidth, double spacing) {
        this.paint = paint;
        this.angle = angle;
        this.stripeWidth = stripeWidth;
        this.spacing = spacing;
    }
    
    /**
     * Get the current paint used for drawing the pinstripes
     * 
     * @return the Paint to use to draw the pinstripes
     */
    public Paint getPaint() {
        return paint;
    }

    /**
     * Set the paint to use for drawing the pinstripes
     * 
     * @param p
     *            the Paint to use. May be a Color.
     */
    public void setPaint(Paint p) {
        Paint old = getPaint();
        this.paint = p;
        firePropertyChange("paint", old, getPaint());
    }
    
    /**
     * Gets the current angle of the pinstripes
     * 
     * @return the angle, in degrees, at which the pinstripes are painted
     */
    public double getAngle() {
        return angle;
    }

    /**
     * Sets the angle, in degrees, at which to paint the pinstripes. If the given angle is < 0 or >
     * 360, it will be appropriately constrained. For example, if a value of 365 is given, it will
     * result in 5 degrees. The conversion is not perfect, but "a man on a galloping horse won't be
     * able to tell the difference".
     * 
     * @param angle
     *            the Angle in degrees at which to paint the pinstripes
     */
    public void setAngle(double angle) {
        if (angle > 360) {
            angle = angle % 360;
        }

        if (angle < 0) {
            angle = 360 - ((angle * -1) % 360);
        }

        double old = getAngle();
        this.angle = angle;
        firePropertyChange("angle", old, getAngle());
    }

    /**
     * Gets the current width of the pinstripes
     * 
     * @return the current pinstripe width
     */
    public double getStripeWidth() {
        return stripeWidth;
    }

    /**
     * Set the width of the pinstripes
     * 
     * @param stripeWidth
     *            a new width for the pinstripes
     */
    public void setStripeWidth(double stripeWidth) {
        double oldSripeWidth = getStripeWidth();
        this.stripeWidth = stripeWidth;
        firePropertyChange("stripeWidth", oldSripeWidth, getStripeWidth());
    }
    
    /**
     * Get the current spacing between the stripes
     * 
     * @return the spacing between pinstripes
     */
    public double getSpacing() {
        return spacing;
    }

    /**
     * Sets the spacing between pinstripes
     * 
     * @param spacing
     *            spacing between pinstripes
     */
    public void setSpacing(double spacing) {
        double old = getSpacing();
        this.spacing = spacing;
        firePropertyChange("spacing", old, getSpacing());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void doPaint(Graphics2D g, Object component, int width, int height) {
        // draws pinstripes at the angle specified in this class and at the given distance apart
        Shape oldClip = g.getClip();
        Shape newClip;

        if (oldClip == null) {
            newClip = new Rectangle(width, height);
        } else {
            Rectangle r = oldClip.getBounds();
            r.width = width;
            r.height = height;

            Area a = new Area(r);
            a.intersect(new Area(oldClip));

            newClip = a;
        }

        int startX = newClip.getBounds().x;
        int startY = newClip.getBounds().y;

        g.setClip(newClip);
        g.setPaint(getForegroundPaint(getPaint(), component));
        g.setStroke(new BasicStroke((float) getStripeWidth()));
        g.rotate(toRadians(getAngle()));

        double hypLength = hypot(width, height);
        double gap = getSpacing() + getStripeWidth();

        int numLines = (int) round(hypLength / gap);

        for (int i = 0; i < numLines; i++) {
            double x = i * gap;

            g.draw(new Line2D.Double(startX + x, startY - hypLength, startX + x, startY + hypLength));
        }

        g.setClip(oldClip);
    }
}
