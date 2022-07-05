/*
 * $Id: MattePainter.java 4147 2012-02-01 17:13:24Z kschaefe $
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

/**
 * A Painter implementation that uses a Paint to fill the entire background
 * area. For example, if I wanted to paint the entire background of a panel green, I would:
 * <pre><code>
 *  MattePainter p = new MattePainter(Color.GREEN);
 *  panel.setBackgroundPainter(p);
 * </code></pre></p>
 * 
 * <p>Since it accepts a Paint, it is also possible to paint a texture or use other
 * more exotic Paint implementations. To paint a BufferedImage texture as the
 * background:
 * <pre><code>
 *  TexturePaint paint = new TexturePaint(bufferedImage,
 *      new Rectangle2D.Double(0, 0, bufferedImage.getWidth(), bufferedImage.getHeight()));
 *  MattePainter p = new MattePainter(paint);
 *  panel.setBackgroundPainter(p);
 * </code></pre></p>
 * 
 * <p>If no paint is specified, then nothing is painted</p>
 * @author rbair
 */
@JavaBean
public class MattePainter extends AbstractAreaPainter<Object> {
    
    /**
     * Creates a new MattePainter with "null" as the paint used
     */
    public MattePainter() {
    }
    
    /**
     * Create a new MattePainter for the given Paint. This can be a GradientPaint
     * (the gradient will not grow when the component becomes larger unless
     * you use the paintStretched boolean property), 
     * TexturePaint, Color, or other Paint instance.
     *
     * @param paint Paint to fill with
     */
    public MattePainter(Paint paint) {
        super(paint);
    }
    
    /**
     * Create a new MattePainter for the given Paint. This can be a GradientPaint
     * (the gradient will not grow when the component becomes larger unless
     * you use the paintStretched boolean property), 
     * TexturePaint, Color, or other Paint instance.
     *
     * @param paint Paint to fill with
     * @param paintStretched indicates if the paint should be stretched
     */
    public MattePainter(Paint paint, boolean paintStretched) {
        super(paint);
        this.setPaintStretched(paintStretched);
    }
    
    /**
     * {@inheritDoc}
     */
    @Override
    protected void doPaint(Graphics2D g, Object component, int width, int height) {
        Paint p = getFillPaint();
        
        if (p != null) {
            Insets insets = getInsets();
            int w = width - insets.left - insets.right;
            int h = height - insets.top - insets.bottom;

            if (isPaintStretched()) {
                p = calculateSnappedPaint(p, w, h);
            }

            g.translate(insets.left, insets.top);
            g.setPaint(p);
            g.fill(provideShape(g, component, w, h));
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Shape provideShape(Graphics2D g, Object comp, int width, int height) {
        return new Rectangle(0,0,width,height);
    }
    
}
