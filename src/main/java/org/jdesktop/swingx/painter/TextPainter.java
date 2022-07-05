/*
 * $Id: TextPainter.java 4147 2012-02-01 17:13:24Z kschaefe $
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

import javax.swing.*;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.font.GlyphVector;

import static org.jdesktop.swingx.painter.PainterUtils.getComponentFont;
import static org.jdesktop.swingx.painter.PainterUtils.getForegroundPaint;

/**
 * A painter which draws text. If the font, text, and paint are not provided they will be
 * obtained from the object being painted if it is a Swing text component.
 *
 * @author rbair
 */
@JavaBean
@SuppressWarnings("nls")
public class TextPainter extends AbstractAreaPainter<Object> {
    private String text = "";
    private Font font = null;
    
    /** Creates a new instance of TextPainter */
    public TextPainter() {
        this("");
    }
    
    /**
     * Create a new TextPainter which will paint the specified text
     * @param text the text to paint
     */
    public TextPainter(String text) {
        this(text, null, null);
    }
    
    /**
     * Create a new TextPainter which will paint the specified text with the specified font.
     * @param text the text to paint
     * @param font the font to paint the text with
     */
    public TextPainter(String text, Font font) {
        this(text, font, null);
    }
    
    /**
     * Create a new TextPainter which will paint the specified text with the specified paint.
     * @param text the text to paint
     * @param paint the paint to paint with
     */
    public TextPainter(String text, Paint paint) {
        this(text, null, paint);
    }
    
    /**
     * Create a new TextPainter which will paint the specified text with the specified font and paint.
     * @param text the text to paint
     * @param font the font to paint the text with
     * @param paint the paint to paint with
     */
    public TextPainter(String text, Font font, Paint paint) {
        this.text = text;
        this.font = font;
        setFillPaint(paint);
    }
    
    /**
     * Set the font (and font size and style) to be used when drawing the text
     * @param f the new font
     */
    public void setFont(Font f) {
        Font old = getFont();
        this.font = f;
        setDirty(true);
        firePropertyChange("font", old, getFont());
    }
    
    /**
     * gets the font (and font size and style) to be used when drawing the text
     * @return the current font
     */
    public Font getFont() {
        return font;
    }
    
    /**
     * Sets the text to draw
     * @param text the text to draw
     */
    public void setText(String text) {
        String old = getText();
        this.text = text == null ? "" : text;
        setDirty(true);
        firePropertyChange("text", old, getText());
    }
    
    /**
     * gets the text currently used to draw
     * @return the text to be drawn
     */
    public String getText() {
        return text;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void doPaint(Graphics2D g, Object component, int width, int height) {
        Font f = calculateFont(component);
        if (f != null) {
            g.setFont(f);
        }
        
        Paint paint = getForegroundPaint(getFillPaint(), component);
        String t = calculateText(component);
        
        // get the font metrics
        FontMetrics metrics = g.getFontMetrics(g.getFont());
        //Rectangle2D rect = metrics.getStringBounds(text,g);
        
        int tw = metrics.stringWidth(t);
        int th = metrics.getHeight();
        Rectangle res = calculateLayout(tw, th, width, height);
        
        g.translate(res.x, res.y);
        
        if(isPaintStretched()) {
            paint = calculateSnappedPaint(paint, res.width, res.height);
        }
        
        if (paint != null) {
            g.setPaint(paint);
        }
        
        g.drawString(t, 0, 0 + metrics.getAscent());
        if(getAreaEffects() != null) {
            Shape shape = provideShape(g, component, width, height);
            for(AreaEffect ef : getAreaEffects()) {
                ef.apply(g, shape, width, height);
            }
        }
        g.translate(-res.x,-res.y);
    }
    
    private String calculateText(final Object component) {
        // prep the text
        String t = getText();
        //make components take priority if(text == null || text.trim().equals("")) {
        if(t != null && !t.trim().equals("")) {
            return t;
        }
        if(component instanceof JTextComponent) {
            t = ((JTextComponent)component).getText();
        }
        if(component instanceof JLabel) {
            t = ((JLabel)component).getText();
        }
        if(component instanceof AbstractButton) {
            t = ((AbstractButton)component).getText();
        }
        return t;
    }
    
    private Font calculateFont(final Object component) {
        // prep the various text attributes
        Font f = getComponentFont(getFont(), component);
        if (f == null) {
            f = new Font("Dialog", Font.PLAIN, 18);
        }
        return f;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Shape provideShape(Graphics2D g2, Object comp, int width, int height) {
        Font f = calculateFont(comp);
        String t = calculateText(comp);
        FontMetrics metrics = g2.getFontMetrics(f);
        GlyphVector vect = f.createGlyphVector(g2.getFontRenderContext(),t);
        return vect.getOutline(0f,0f+ metrics.getAscent());
    }
}
