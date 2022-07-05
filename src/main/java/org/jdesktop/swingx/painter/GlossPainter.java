/*
 * $Id: GlossPainter.java 4147 2012-02-01 17:13:24Z kschaefe $
 *
 * Copyright 2006 Sun Microsystems, Inc., 4150 Network Circle,
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
import java.awt.geom.Ellipse2D;

/**
 * <p>A Painter implementation that simulates a gloss effect. The gloss can
 * be positioned at the top or bottom of the drawing area. To fill the gloss,
 * this painter uses a Paint instance which can be used to fill with a color
 * (opaque or translucent), a texture, a gradient...</p>
 * <p>The following example creates a white gloss at the top of the drawing
 * area:</p>
 * <pre>
 *  GlossPainter p = new GlossPainter();
 *  p.setPaint(new Color(1.0f, 1.0f, 1.0f, 0.2f);
 *  p.setPosition(GlossPainter.GlossPosition.TOP);
 *  panel.setBackgroundPainter(p);
 * </pre>
 * <p>The values shown in this examples are the values used by default if
 * they are not specified.</p>
 *
 * @author Romain Guy <romain.guy@mac.com>
 */
@JavaBean
@SuppressWarnings("nls")
public class GlossPainter extends AbstractPainter<Object> {
    /**
     * <p>Used to define the position of the gloss on the painted area.</p>
     */
    public enum GlossPosition {
        TOP, BOTTOM
    }
    
    private Paint paint;
    private GlossPosition position;
    
    /**
     * <p>Creates a new gloss painter positioned at the top of the painted
     * area with a 20% translucent white color.</p>
     */
    public GlossPainter() {
        this(new Color(1.0f, 1.0f, 1.0f, 0.2f), GlossPosition.TOP);
    }
    
    /**
     * <p>Creates a new gloss painter positioned at the top of the painted
     * area with the specified paint.</p>
     *
     * @param paint The paint to be used when filling the gloss
     */
    public GlossPainter(Paint paint) {
        this(paint, GlossPosition.TOP);
    }
    
    /**
     * <p>Creates a new gloss painter positioned at the specified position
     * and using a white, 20% translucent paint.</p>
     *
     * @param position The position of the gloss on the painted area
     */
    public GlossPainter(GlossPosition position) {
        this(new Color(1.0f, 1.0f, 1.0f, 0.2f), position);
    }
    
    /**
     * <p>Creates a new gloss painter positioned at the specified position
     * and painted with the specified paint.</p>
     *
     * @param paint The paint to be used when filling the gloss
     * @param position The position of the gloss on the painted area
     */
    public GlossPainter(Paint paint, GlossPosition position) {
        this.setPaint(paint);
        this.setPosition(position);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void doPaint(Graphics2D g, Object component, int width, int height) {
        if (getPaint() != null) {
            Ellipse2D ellipse = new Ellipse2D.Double(-width / 2.0,
                height / 2.7, width * 2.0,
                height * 2.0);

            Area gloss = new Area(ellipse);
            if (getPosition() == GlossPosition.TOP) {
                Area area = new Area(new Rectangle(0, 0,
                    width, height));
                area.subtract(new Area(ellipse));
                gloss = area;
            }
            /*
            if(getClip() != null) {
                gloss.intersect(new Area(getClip()));
            }*/
            g.setPaint(getPaint());
            g.fill(gloss);
        }
    }

    /**
     * <p>Returns the paint currently used by the painter to fill the gloss.</p>
     *
     * @return the current Paint instance used by the painter
     */
    public Paint getPaint() {
        return paint;
    }

    /**
     * <p>Changes the paint to be used to fill the gloss. When the specified
     * paint is null, nothing is painted. A paint can be an instance of
     * Color.</p>
     *
     * @param paint The Paint instance to be used to fill the gloss
     */
    public void setPaint(Paint paint) {
        Paint old = this.paint;
        this.paint = paint;
        setDirty(true);
        firePropertyChange("paint", old, getPaint());
    }

    /**
     * <p>Returns the position at which the gloss is painted.</p>
     *
     * @return the position of the gloss in the painted area
     */
    public GlossPosition getPosition() {
        return position;
    }

    /**
     * <p>Changes the position of the gloss in the painted area. Only the
     * values defined in the GlossPosition enum are valid.</p>
     *
     * @param position The position at which the gloss is painted
     */
    public void setPosition(GlossPosition position) {
        GlossPosition old = this.position;
        this.position = position;
        setDirty(true);
        firePropertyChange("position", old, getPosition());
    }
}
