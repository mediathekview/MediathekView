/*
 * $Id: AbstractAreaEffect.java 4082 2011-11-15 18:39:43Z kschaefe $
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


package org.jdesktop.swingx.painter.effects;

import java.awt.*;
import java.awt.geom.Area;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;

import static org.jdesktop.swingx.util.GraphicsUtilities.createCompatibleTranslucentImage;

/**
 * The abstract base class for path effects. It takes care
 * of soft clipping and interpolating brush sizes and colors. Subclasses
 *  can change these values to provide prefab effect behavior, like
 * dropshadows and glows.
 * @author joshy
 */
@SuppressWarnings("nls")
public class AbstractAreaEffect implements AreaEffect {
    private static final boolean debug = false;
    /**
     * Creates a new instance of AreaEffect
     */
    public AbstractAreaEffect() {
        setBrushColor(Color.BLACK);
        setBrushSteps(10);
        setEffectWidth(8);
        setRenderInsideShape(false);
        setOffset(new Point(4,4));
        setShouldFillShape(true);
        setShapeMasked(true);
    }
    
    @Override
    public void apply(Graphics2D g, Shape clipShape, int width, int height) {
        // create a rect to hold the bounds
        width = (int)(clipShape.getBounds2D().getWidth() + clipShape.getBounds2D().getX());
        height = (int)(clipShape.getBounds2D().getHeight() + clipShape.getBounds2D().getY());
        Rectangle effectBounds = new Rectangle(0,0,
                width  + getEffectWidth()*2 + 1,
                height + getEffectWidth()*2 + 1);
        
        // Apply the border glow effect
        if (isShapeMasked()) {
            BufferedImage clipImage = getClipImage(effectBounds);
            Graphics2D g2 = clipImage.createGraphics();
            
            try {
                // clear the buffer
                g2.setPaint(Color.BLACK);
                g2.setComposite(AlphaComposite.Clear);
                g2.fillRect(0, 0, effectBounds.width, effectBounds.height);

                if (debug) {
                    g2.setPaint(Color.WHITE);
                    g2.setComposite(AlphaComposite.SrcOver);
                    g2.drawRect(0, 0, effectBounds.width - 1,
                            effectBounds.height - 1);
                }

                // turn on smoothing
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                        RenderingHints.VALUE_ANTIALIAS_ON);
                g2.translate(getEffectWidth() - getOffset().getX(),
                        getEffectWidth() - getOffset().getY());
                paintBorderGlow(g2, clipShape, width, height);

                // clip out the parts we don't want
                g2.setComposite(AlphaComposite.Clear);
                g2.setColor(Color.WHITE);
                if (isRenderInsideShape()) {
                    // clip the outside
                    Area area = new Area(effectBounds);
                    area.subtract(new Area(clipShape));
                    g2.fill(area);
                } else {
                    // clip the inside
                    g2.fill(clipShape);
                }
            } finally {
                // draw the final image
                g2.dispose();
            }
            
            g.drawImage(clipImage, -getEffectWidth() + (int) getOffset().getX(), -getEffectWidth() + (int) getOffset().getY(), null);
        }  else {
            g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            paintBorderGlow(g, clipShape, width, height);
        }
        
        //g.setColor(Color.MAGENTA);
        //g.draw(clipShape.getBounds2D());
        //g.drawRect(0,0,width,height);
        
    }
    
    BufferedImage _clipImage = null;
    private BufferedImage getClipImage(final Rectangle effectBounds) {
        // set up a temp buffer
        if(_clipImage == null ||
                _clipImage.getWidth() != effectBounds.width ||
                _clipImage.getHeight() != effectBounds.height) {
            _clipImage = createCompatibleTranslucentImage(effectBounds.width, effectBounds.height);
        }
        _clipImage.getGraphics().clearRect(0,0,_clipImage.getWidth(), _clipImage.getHeight());
        return _clipImage;
    }
    
    
    /*
    private BufferedImage createClipImage(Shape s, Graphics2D g, int width, int height) {
        // Create a translucent intermediate image in which we can perform
        // the soft clipping
     
        GraphicsConfiguration gc = g.getDeviceConfiguration();
        BufferedImage img = gc.createCompatibleImage(width, height, Transparency.TRANSLUCENT);
        Graphics2D g2 = img.createGraphics();
     
        // Clear the image so all pixels have zero alpha
        g2.setComposite(AlphaComposite.Clear);
        g2.fillRect(0, 0, width, height);
     
        // Render our clip shape into the image.  Note that we enable
        // antialiasing to achieve the soft clipping effect.  Try
        // commenting out the line that enables antialiasing, and
        // you will see that you end up with the usual hard clipping.
        g2.setComposite(AlphaComposite.Src);
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setColor(Color.WHITE);
        g2.fill(s);
        g2.dispose();
     
        return img;
    }*/
    
    
    /* draws the actual shaded border to the specified graphics
     */
    /**
     * Paints the border glow
     * @param g2
     * @param clipShape
     * @param width
     * @param height
     */
    protected void paintBorderGlow(Graphics2D g2,
            Shape clipShape, int width, int height) {
        
        int steps = getBrushSteps();
        float brushAlpha = 1f/steps;
        
        boolean inside = isRenderInsideShape();
        
        g2.setPaint(getBrushColor());
        
        g2.translate(offset.getX(), offset.getY());
        
        if(isShouldFillShape()) {
            // set the inside/outside mode
            if(inside) {
                g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_ATOP, 1f));
                Area a1 = new Area(new Rectangle(
                        (int)-offset.getX()-20,
                        (int)-offset.getY()-20,
                        width+40,height+40));
                Area a2 = new Area(clipShape);
                a1.subtract(a2);
                g2.fill(a1);
            } else {
                g2.setComposite(AlphaComposite.getInstance(AlphaComposite.DST_OVER, 1f));
                g2.fill(clipShape);
            }
            
        }
        
        // set the inside/outside mode
        /*
        if(inside) {
            g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_ATOP, brushAlpha));
        } else {
            g2.setComposite(AlphaComposite.getInstance(AlphaComposite.DST_OVER, brushAlpha));
        }*/
        g2.setComposite(AlphaComposite.getInstance(AlphaComposite.DST_OVER, brushAlpha));
        
        // draw the effect
        for(float i=0; i<steps; i=i+1f) {
            float brushWidth = i * effectWidth/steps;
            g2.setStroke(new BasicStroke(brushWidth,
                    BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
            g2.draw(clipShape);
        }
        g2.translate(-offset.getX(), -offset.getY());
        
    }
    
    /**
     * Holds value of property brushColor.
     */
    private Color brushColor;
    
    /**
     * Utility field used by bound properties.
     */
    private java.beans.PropertyChangeSupport propertyChangeSupport =  new java.beans.PropertyChangeSupport(this);
    
    /**
     * Adds a PropertyChangeListener to the listener list.
     * @param l The listener to add.
     */
    public void addPropertyChangeListener(java.beans.PropertyChangeListener l) {
        propertyChangeSupport.addPropertyChangeListener(l);
    }
    
    /**
     * Removes a PropertyChangeListener from the listener list.
     * @param l The listener to remove.
     */
    public void removePropertyChangeListener(java.beans.PropertyChangeListener l) {
        propertyChangeSupport.removePropertyChangeListener(l);
    }
    
    /**
     * Getter for property brushColor.
     * @return Value of property brushColor.
     */
    public Color getBrushColor() {
        return this.brushColor;
    }
    
    /**
     * Setter for property brushColor.
     * @param brushColor New value of property brushColor.
     */
    public void setBrushColor(Color brushColor) {
        Color oldBrushColor = this.brushColor;
        this.brushColor = brushColor;
        propertyChangeSupport.firePropertyChange("brushColor", oldBrushColor, brushColor);
    }
    
    /**
     * Holds value of property brushSteps.
     */
    private int brushSteps;
    
    /**
     * Getter for property brushSteps.
     * @return Value of property brushSteps.
     */
    public int getBrushSteps() {
        return this.brushSteps;
    }
    
    /**
     * Setter for property brushSteps.
     * @param brushSteps New value of property brushSteps.
     */
    public void setBrushSteps(int brushSteps) {
        int oldBrushSteps = this.brushSteps;
        this.brushSteps = brushSteps;
        propertyChangeSupport.firePropertyChange("brushSteps", Integer.valueOf(oldBrushSteps), Integer.valueOf(brushSteps));
    }
    
    /**
     * Holds value of property effectWidth.
     */
    private int effectWidth;
    
    /**
     * Getter for property effectWidth.
     * @return Value of property effectWidth.
     */
    public int getEffectWidth() {
        return this.effectWidth;
    }
    
    /**
     * Setter for property effectWidth.
     * @param effectWidth New value of property effectWidth.
     */
    public void setEffectWidth(int effectWidth) {
        int oldEffectWidth = this.effectWidth;
        this.effectWidth = effectWidth;
        propertyChangeSupport.firePropertyChange("effectWidth", Integer.valueOf(oldEffectWidth), Integer.valueOf(effectWidth));
    }
    
    /**
     * Holds value of property renderInsideShape.
     */
    private boolean renderInsideShape;
    
    /**
     * Getter for property renderInsideShape.
     * @return Value of property renderInsideShape.
     */
    public boolean isRenderInsideShape() {
        return this.renderInsideShape;
    }
    
    /**
     * Setter for property renderInsideShape.
     * @param renderInsideShape New value of property renderInsideShape.
     */
    public void setRenderInsideShape(boolean renderInsideShape) {
        boolean oldRenderInsideShape = this.renderInsideShape;
        this.renderInsideShape = renderInsideShape;
        propertyChangeSupport.firePropertyChange("renderInsideShape", Boolean.valueOf(oldRenderInsideShape), Boolean.valueOf(renderInsideShape));
    }
    
    /**
     * Holds value of property offset.
     */
    private Point2D offset;
    
    /**
     * Getter for property offset.
     * @return Value of property offset.
     */
    public Point2D getOffset() {
        return this.offset;
    }
    
    /**
     * Setter for property offset.
     * @param offset New value of property offset.
     */
    public void setOffset(Point2D offset) {
        Point2D oldOffset = this.offset;
        this.offset = offset;
        propertyChangeSupport.firePropertyChange("offset", oldOffset, offset);
    }
    
    /**
     * Holds value of property shouldFillShape.
     */
    private boolean shouldFillShape;
    
    /**
     * Getter for property shouldFillShape.
     * @return Value of property shouldFillShape.
     */
    public boolean isShouldFillShape() {
        return this.shouldFillShape;
    }
    
    /**
     * Setter for property shouldFillShape.
     * @param shouldFillShape New value of property shouldFillShape.
     */
    public void setShouldFillShape(boolean shouldFillShape) {
        boolean oldShouldFillShape = this.shouldFillShape;
        this.shouldFillShape = shouldFillShape;
        propertyChangeSupport.firePropertyChange("shouldFillShape", Boolean.valueOf(oldShouldFillShape), Boolean.valueOf(shouldFillShape));
    }
    
    /**
     * Holds value of property shapeMasked.
     */
    private boolean shapeMasked;
    
    /**
     * Getter for property shapeMasked.
     * @return Value of property shapeMasked.
     */
    public boolean isShapeMasked() {
        return this.shapeMasked;
    }
    
    /**
     * Setter for property shapeMasked.
     * @param shapeMasked New value of property shapeMasked.
     */
    public void setShapeMasked(boolean shapeMasked) {
        boolean oldShapeMasked = this.shapeMasked;
        this.shapeMasked = shapeMasked;
        propertyChangeSupport.firePropertyChange("shapeMasked", Boolean.valueOf(oldShapeMasked), Boolean.valueOf(shapeMasked));
    }
    
}
