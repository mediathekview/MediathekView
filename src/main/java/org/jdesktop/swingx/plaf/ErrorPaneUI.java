/*
 * $Id: ErrorPaneUI.java 3105 2008-10-16 21:09:43Z rah003 $
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

package org.jdesktop.swingx.plaf;

import javax.swing.*;
import javax.swing.plaf.PanelUI;
import java.awt.*;

/**
 * The ComponentUI for a JXErrorPane.
 * <p>
 * 
 * @author rbair
 */
public abstract class ErrorPaneUI extends PanelUI {
    /**
     * Creates new ErrorPane wrapped in the frame window centered at provided owner component. 
     * @param owner component to center created error frame at.
     * @return New ErrorPane instance wrapped in JFrame.
     */
    public abstract JFrame getErrorFrame(Component owner);
    
    /**
     * Creates new ErrorPane wrapped in the dialog window centered at provided owner component. 
     * @param owner component to center created error dialog at.
     * @return New ErrorPane instance wrapped in JDialog.
     */
    public abstract JDialog getErrorDialog(Component owner);
    
    /**
     * Creates new ErrorPane wrapped in the internal frame window centered at provided owner component. 
     * @param owner component to center created error frame at.
     * @return New ErrorPane instance wrapped in JInternalFrame.
     */
    public abstract JInternalFrame getErrorInternalFrame(Component owner);
    /**
     * Calculates default prefered size for JXErrorPane on given platform/LAF.
     * @return Preferred size.
     */
    public abstract Dimension calculatePreferredSize();
}
