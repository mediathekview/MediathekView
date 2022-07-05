/*
 * $Id: BasicMultiThumbSliderUI.java 3927 2011-02-22 16:34:11Z kleopatra $
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
package org.jdesktop.swingx.plaf.basic;

import org.jdesktop.swingx.JXMultiThumbSlider;
import org.jdesktop.swingx.multislider.ThumbRenderer;
import org.jdesktop.swingx.multislider.TrackRenderer;
import org.jdesktop.swingx.plaf.MultiThumbSliderUI;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import java.awt.*;

/**
 *
 * @author Joshua Marinacci
 */
public class BasicMultiThumbSliderUI extends MultiThumbSliderUI {
    
    protected JXMultiThumbSlider<?> slider;
    
    public static ComponentUI createUI(JComponent c) {
        return new BasicMultiThumbSliderUI();
    }
    
    @Override
    public void installUI(JComponent c) {
        slider = (JXMultiThumbSlider<?>)c;
        slider.setThumbRenderer(new BasicThumbRenderer());
        slider.setTrackRenderer(new BasicTrackRenderer());        
    }
    @Override
    public void uninstallUI(JComponent c) {
        slider = null;
    }

    private class BasicThumbRenderer extends JComponent implements ThumbRenderer {
        public BasicThumbRenderer() {
            setPreferredSize(new Dimension(14,14));
        }

        @Override
        protected void paintComponent(Graphics g) {
            g.setColor(Color.green);
            Polygon poly = new Polygon();
            JComponent thumb = this;
            poly.addPoint(thumb.getWidth()/2,0);
            poly.addPoint(0,thumb.getHeight()/2);
            poly.addPoint(thumb.getWidth()/2,thumb.getHeight());
            poly.addPoint(thumb.getWidth(),thumb.getHeight()/2);
            g.fillPolygon(poly);
        }

        @Override
        public JComponent getThumbRendererComponent(JXMultiThumbSlider slider, int index, boolean selected) {
            return this;
        }
    }

    private class BasicTrackRenderer extends JComponent implements TrackRenderer {
        private JXMultiThumbSlider<?> slider;
        @Override
        public void paintComponent(Graphics g) {
            g.setColor(slider.getBackground());
            g.fillRect(0, 0, slider.getWidth(), slider.getHeight());
            g.setColor(Color.black);
            g.drawLine(0,slider.getHeight()/2,slider.getWidth(),slider.getHeight()/2);
            g.drawLine(0,slider.getHeight()/2+1,slider.getWidth(),slider.getHeight()/2+1);
        }

        @Override
        public JComponent getRendererComponent(JXMultiThumbSlider slider) {
            this.slider = slider;
            return this;
        }
    }
}
