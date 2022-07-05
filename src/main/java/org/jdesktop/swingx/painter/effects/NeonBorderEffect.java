/*
 * $Id: NeonBorderEffect.java 3829 2010-10-07 02:01:20Z kschaefe $
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

/**
 * An effect which draws a multicolored border around a painter's shape.
 * It will interpolate between two specified colors, creating a neon like effect.
 * @author joshy
 */
public class NeonBorderEffect extends AbstractAreaEffect {
    
    private Color edgeColor;
    private Color centerColor;
    private BorderPosition borderPosition = BorderPosition.Outside;
    
    /**
     * An enum representing the position of the border: inside, outside, or centered on the border.
     */
    public enum BorderPosition { Inside, Centered, Outside }
    
    
    /**
     * Create a new NeonBorderEffect
     */
    public NeonBorderEffect() {
        this(Color.GREEN, Color.WHITE, 10);
    }
    
    /** Creates a new instance of NeonBorderEffect */
    public NeonBorderEffect(Color edgeColor, Color centerColor, int effectWidth) {
        super();
        setEffectWidth(effectWidth);
        this.setEdgeColor(edgeColor);
        this.setCenterColor(centerColor);
        this.setRenderInsideShape(false);
        this.setShouldFillShape(false);
        this.setOffset(new Point(0,0));
    }
    
    @Override
    protected void paintBorderGlow(Graphics2D gfx, Shape clipShape, int width, int height) {
        
        /*
        // draw the effect
        for(float i=steps-1; i>=0; i=i-1f) {
            float brushWidth = i * getEffectWidth()/steps;
            gfx.setPaint(interpolateColor(i/steps,edgeColor,centerColor));
            gfx.setStroke(new BasicStroke(brushWidth,
                    BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
            gfx.draw(clipShape);
        }*/
        
        /* // an interesting outline effect. stroke the shape with a wide brush
         * // then stroke again with slightly less wide one, then don't fill the middle
        for(int i=0; i<2; i++) {
            float brushWidth = (2-i)*5;
            p("widdth = " + brushWidth);
            gfx.setPaint(interpolateColor((float)(1-i), Color.BLACK, Color.WHITE));
            gfx.setStroke(new BasicStroke(brushWidth));
            gfx.draw(clipShape);
        }
         */
        gfx.translate(getOffset().getX(), getOffset().getY());
        gfx.setComposite(AlphaComposite.SrcOver);
        int steps = getEffectWidth();
        if(borderPosition == BorderPosition.Centered) {
            steps = steps/2;
        }
        for(int i=0; i<steps; i++) {
            
            // make the brush width smaller each time until there is nothing left
            float brushWidth = steps+1-i;
            float half = steps/2;
            
            if(borderPosition == BorderPosition.Centered) {
                gfx.setPaint(interpolateColor((float)(steps-i)/steps, getEdgeColor(), getCenterColor()));
            } else {
                if(i<half) {
                    gfx.setPaint(interpolateColor((half-i)/half, getEdgeColor(), getCenterColor()));
                } else {
                    gfx.setPaint(interpolateColor((i-half)/half, getEdgeColor(), getCenterColor()));
                }
            }
            
            // to make the effect softer use a different stroke
            gfx.setStroke(new BasicStroke(brushWidth,
                    BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
            //gfx.setStroke(new BasicStroke(brushWidth));
            gfx.draw(clipShape);
        }
        gfx.translate(-getOffset().getX(), -getOffset().getY());
        
    }
    
    protected Color interpolateColor(float t, Color start, Color end) {
        float[] partsS = start.getRGBComponents(null);
        float[] partsE = end.getRGBComponents(null);
        float[] partsR = new float[4];
        for(int i=0; i<4; i++) {
            partsR[i] = (partsS[i] - partsE[i])*t + partsE[i];
        }
        return new Color(partsR[0],partsR[1],partsR[2],partsR[3]);
    }
    
    /**
     * Gets the current edge color.
     * @return current edge color
     */
    public Color getEdgeColor() {
        return edgeColor;
    }
    
    /**
     * Set the edge color
     * @param edgeColor 
     */
    public void setEdgeColor(Color edgeColor) {
        this.edgeColor = edgeColor;
    }
    
    /**
     * 
     * @return color in the center of the effect
     */
    public Color getCenterColor() {
        return centerColor;
    }
    
    /**
     * 
     * @param centerColor color in the center of the effect.
     * @see #getCenterColor()
     */
    public void setCenterColor(Color centerColor) {
        this.centerColor = centerColor;
    }
    
    /**
     * 
     * @return position of the border relative to the edge of painter covered area.
     * @see BorderPosition
     */
    public BorderPosition getBorderPosition() {
        return borderPosition;
    }
    
    /**
     * 
     * @param borderPosition position of the border relative to the edge of painter covered area.
     * @see #getBorderPosition()
     * @see BorderPosition
     */
    public void setBorderPosition(BorderPosition borderPosition) {
        this.borderPosition = borderPosition;
        switch(borderPosition) {
            case Centered : 
                setShapeMasked(false);
                break;
            case Inside :
                setShapeMasked(true);
                setRenderInsideShape(true);
                break;
            case Outside :
                setShapeMasked(true);
                setRenderInsideShape(false);
                break;
        }
        if(borderPosition == BorderPosition.Centered) {
        }
    }
}
