/*
 * $Id: AlphaPainter.java 4147 2012-02-01 17:13:24Z kschaefe $
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
 * Applies an alpha value to an entire stack of painters.
 *
 * @author joshy
 */
@JavaBean
@SuppressWarnings("nls")
public class AlphaPainter<T> extends CompoundPainter<T> {
    private float alpha = 1.0f;

    /**
     * {@inheritDoc}
     */
    @Override
    protected void doPaint(Graphics2D g, T component, int width, int height) {
        Graphics2D g2 = (Graphics2D) g.create();
        
        try {
        if(getTransform() != null) {
            g2.setTransform(getTransform());
        }
        if(alpha < 1) {
                g2.setComposite(AlphaComposite.getInstance(
                        AlphaComposite.SRC_OVER, alpha));
        }
            
            super.doPaint(g2, component, width, height);
        } finally {
            g2.dispose();
        }
    }
    /*
    public static void main(String ... args) {
        JXPanel panel = new JXPanel();
        AlphaPainter alpha = new AlphaPainter();
        alpha.setAlpha(1f);
        alpha.setPainters(new PinstripePainter(new Color(255,255,255,125),45,20,20));
        
        panel.setBackgroundPainter(new CompoundPainter(
                new MattePainter(Color.RED),
                alpha
                ));
        
        JFrame frame = new JFrame();
        frame.add(panel);
        frame.pack();
        frame.setSize(200,200);
        frame.setVisible(true);
    }*/

    /**
     * Returns the current alpha value for this painter. This is the alpha value that will be applied 
     * to all painters set inside this painter. Alpha values will be multiplied. This means if you set an
     * alpha of 0.5 on this painter and you nest a painter inside which uses an alpha of 0.5 then the final
     * pixels drawn will have an alpha of 0.25.
     * @return the current value of alpha property
     */
    public float getAlpha() {
        return alpha;
    }

    /**
     * Sets the current alpha value for this painter. This is the alpha value that will be applied 
     * to all painters set inside this painter. Alpha values will be multiplied. This means if you set an
     * alpha of 0.5 on this painter and you nest a painter inside which uses an alpha of 0.5 then the final
     * pixels drawn will have an alpha of 0.25.
     * @param alpha the new value of the alpha property
     */
    public void setAlpha(float alpha) {
        float old = getAlpha();
        this.alpha = alpha;
        firePropertyChange("alpha", old, getAlpha());
    }
}
