/*
 * $Id: PainterPaint.java 4082 2011-11-15 18:39:43Z kschaefe $
 *
 * Copyright 2009 Sun Microsystems, Inc., 4150 Network Circle,
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

import org.jdesktop.swingx.util.GraphicsUtilities;

import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.Raster;

/**
 *
 * @author Karl George Schaefer
 */
public class PainterPaint<T> implements Paint {
    protected static class PainterPaintContext<T> implements PaintContext {
        private Painter<T> painter;
        private T object;
        private BufferedImage saved;
        
        public PainterPaintContext(Painter<T> painter, T object) {
            painter.getClass(); // null check
            this.painter = painter;
            this.object = object;
        }
        
        /**
         * {@inheritDoc}
         */
        @Override
        public void dispose() {
            //does nothing
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public ColorModel getColorModel() {
            if (saved == null) {
                return GraphicsUtilities.createCompatibleImage(1, 1).getColorModel();
            }
            
            return saved.getColorModel();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Raster getRaster(int x, int y, int w, int h) {
            if (saved == null || saved.getWidth() != w || saved.getHeight() != h) {
                saved = GraphicsUtilities.createCompatibleImage(w, h);
                Graphics2D g2d = saved.createGraphics();
                
                try {
                    if (painter instanceof AbstractPainter) {
                        ((AbstractPainter<?>) painter).setInPaintContext(true);
                    }
                    painter.paint(g2d, object, w, h);
                } finally {
                    g2d.dispose();
                    if (painter instanceof AbstractPainter) {
                        ((AbstractPainter<?>) painter).setInPaintContext(false);
                    }
                }
            }
            
            return saved.getData();
        }
    }
    
    private final PainterPaintContext<T> context;
    
    public PainterPaint(Painter<T> painter, T object) {
        context = new PainterPaintContext<T>(painter, object);
    }
    
    /**
     * {@inheritDoc}
     */
    @Override
    public PaintContext createContext(ColorModel cm, Rectangle deviceBounds,
            Rectangle2D userBounds, AffineTransform xform, RenderingHints hints) {
        return context;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getTransparency() {
        return Transparency.BITMASK;
    }
}
