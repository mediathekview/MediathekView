/*
 * $Id: ShapePainter.java 4147 2012-02-01 17:13:24Z kschaefe $
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
import org.jdesktop.swingx.painter.effects.AreaEffect;

import java.awt.*;
import java.awt.geom.Ellipse2D;

import static org.jdesktop.swingx.painter.PainterUtils.getBackgroundPaint;
import static org.jdesktop.swingx.painter.PainterUtils.getForegroundPaint;


/**
 * <p>A Painter that paints java.awt.Shapes. It uses a stroke and a fillPaint to do so. The
 * shape is painted as is, at a specific location. If no Shape is specified, nothing
 * will be painted. If no stroke is specified, the default for the Graphics2D
 * will be used. If no fillPaint is specified, the component background color
 * will be used. The shape can be positioned using the insets, horizontal, and
 * vertical properties.</p>
 * 
 * <p>Here is an example that draws a rectangle aligned on the center right:
 * <pre><code>
 *  Rectangle2D.Double rect = new Rectangle2D.Double(0, 0, 50, 50);
 *  ShapePainter p = new ShapePainter(rect);
 * p.setHorizontal(HorizontalAlignment.RIGHT);
 * p.setVertical(VerticalAlignment.CENTER);
 * </code></pre>
 * @author rbair
 */
@JavaBean
@SuppressWarnings("nls")
public class ShapePainter extends AbstractAreaPainter<Object> {
    /**
     * The Shape to fillPaint. If null, nothing is painted.
     */
    private Shape shape;
    
    /**
     * Create a new ShapePainter
     */
    public ShapePainter() {
        super();
        this.shape = new Ellipse2D.Double(0,0,100,100);
        this.setBorderWidth(3);
        this.setFillPaint(Color.RED);
        this.setBorderPaint(Color.BLACK);
    }
    
    /**
     * Create a new ShapePainter with the specified shape.
     *
     *
     * @param shape the shape to fillPaint
     */
    public ShapePainter(Shape shape) {
        super();
        this.shape = shape;
    }
    
    /**
     * Create a new ShapePainter with the specified shape and fillPaint.
     *
     *
     * @param shape the shape to fillPaint
     * @param paint the fillPaint to be used to fillPaint the shape
     */
    public ShapePainter(Shape shape, Paint paint) {
        super();
        this.shape = shape;
        this.setFillPaint(paint);
    }
    
    /**
     * Create a new ShapePainter with the specified shape and fillPaint. The shape
     * can be filled or stroked (only the outline is painted).
     *
     *
     * @param shape the shape to fillPaint
     * @param paint the fillPaint to be used to fillPaint the shape
     * @param style specifies the ShapePainter.Style to use for painting this shape.
     *        If null, then Style.BOTH is used
     */
    public ShapePainter(Shape shape, Paint paint, Style style) {
        super();
        this.shape = shape;
        this.setFillPaint(paint);
        this.setStyle(style == null ? Style.BOTH : style);
    }
    
    /**
     * Sets the shape to fillPaint. This shape is not resized when the component
     * bounds are. To do that, create a custom shape that is bound to the
     * component width/height
     *
     *
     * @param s the Shape to fillPaint. May be null
     */
    public void setShape(Shape s) {
        Shape old = getShape();
        this.shape = s;
        setDirty(true);
        firePropertyChange("shape", old, getShape());
    }
    
    /**
     * Gets the current shape
     * @return the Shape to fillPaint. May be null
     */
    public Shape getShape() {
        return shape;
    }
    
    /**
     * {@inheritDoc}
     */
    @Override
    protected void doPaint(Graphics2D g, Object component, int w, int h) {
        g.setStroke(new BasicStroke(this.getBorderWidth()));
        
        if(getShape() != null) {
            Shape s = provideShape(g,component, w, h);
            Rectangle bounds = s.getBounds();
            Rectangle rect = calculateLayout(bounds.width, bounds.height, w, h);
            //u.p("rect = " + rect);
            g = (Graphics2D)g.create();
            
            try {
                g.translate(rect.x, rect.y);
                //draw/fill the shape
                drawPathEffects(g, s, rect.width, rect.height);
                switch (getStyle()) {
                    case BOTH:
                        drawShape(g, s, component, rect.width, rect.height);
                        fillShape(g, s, component, rect.width, rect.height);
                        break;
                    case FILLED:
                        fillShape(g, s, component, rect.width, rect.height);
                        break;
                    case OUTLINE:
                        drawShape(g, s, component, rect.width, rect.height);
                        break;
                }
            } finally {
                g.dispose();
        }
    }
    }
    
    private void drawShape(Graphics2D g, Shape s, Object component, int w, int h) {
        g.setPaint(calculateStrokePaint(component, w, h));
        g.draw(s);
    }
    
    private void fillShape(Graphics2D g, Shape s, Object component, int w, int h) {
        g.setPaint(calculateFillPaint(component, w, h));
        g.fill(s);
    }
    
    // shape effect stuff
    @Override
    protected Shape provideShape(Graphics2D g, Object comp, int width, int height) {
        return getShape();
    }
    
    private Paint calculateStrokePaint(Object component, int width, int height) {
        Paint p = getForegroundPaint(getBorderPaint(), component);
        if(isPaintStretched()) {
            p = calculateSnappedPaint(p, width, height);
        }
        return p;
    }
    
    private Paint calculateFillPaint(Object component, int width, int height) {
        //set the fillPaint
        Paint p = getBackgroundPaint(getFillPaint(), component);
        if(isPaintStretched()) {
            p = calculateSnappedPaint(p, width, height);
        }
        return p;
    }

    private void drawPathEffects(Graphics2D g, Shape s, int w, int h) {
        if(getAreaEffects() != null) {
            //Paint pt = calculateFillPaint(component, w, h);
            for(AreaEffect ef : getAreaEffects()) {
                ef.apply(g, s, w, h);
            }
        }
    }
}
