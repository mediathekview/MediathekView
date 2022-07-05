/*
 * $Id: RepaintManagerX.java 4249 2012-11-13 18:12:49Z kschaefe $
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

package org.jdesktop.swingx;

import javax.swing.*;
import java.awt.*;

/**
 * <p>An implementation of {@link RepaintManager} which adds support for transparency
 * in {@link JXPanel}s. <code>JXPanel</code> (which supports translucency) will 
 * replace the current RepaintManager with an instance of RepaintManagerX 
 * <em>unless</em> the current RepaintManager is tagged by the {@link TranslucentRepaintManager}
 * annotation.</p>
 *
 * @author zixle
 * @author rbair
 * @author Karl Schaefer
 */
@TranslucentRepaintManager
public class RepaintManagerX extends ForwardingRepaintManager {
    /**
     * Creates a new manager that forwards all calls to the delegate.
     * 
     * @param delegate
     *            the manager backing this {@code RepaintManagerX}
     * @throws NullPointerException
     *             if {@code delegate} is {@code null}
     */
    public RepaintManagerX(RepaintManager delegate) {
        super(delegate);
    }
    
    /**
     * {@inheritDoc}
     */
    @Override
    public void addDirtyRegion(JComponent c, int x, int y, int w, int h) {
        AlphaPaintable alphaPaintable = (AlphaPaintable) SwingUtilities.getAncestorOfClass(AlphaPaintable.class, c);
        
        if (alphaPaintable != null && alphaPaintable.getAlpha() < 1f) {
            Point p = SwingUtilities.convertPoint(c, x, y, (JComponent) alphaPaintable);
            addDirtyRegion((JComponent) alphaPaintable, p.x, p.y, w, h);
        } else {
            super.addDirtyRegion(c, x, y, w, h);
        }
    }
}
