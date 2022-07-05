/*
 * $Id: WindowsLookAndFeelAddons.java 4092 2011-11-30 18:04:36Z kschaefe $
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
package org.jdesktop.swingx.plaf.windows;

import org.jdesktop.swingx.plaf.LookAndFeelAddons;
import org.jdesktop.swingx.plaf.basic.BasicLookAndFeelAddons;
import org.jdesktop.swingx.util.OS;
import org.kohsuke.MetaInfServices;

import static javax.swing.UIManager.getLookAndFeel;
import static javax.swing.UIManager.getSystemLookAndFeelClassName;

/**
 * Adds new pluggable UI following the Windows XP look and feel.
 */
@MetaInfServices(LookAndFeelAddons.class)
public class WindowsLookAndFeelAddons extends BasicLookAndFeelAddons {

    public static final String HOMESTEAD_VISUAL_STYLE = "HomeStead";

    public static final String SILVER_VISUAL_STYLE = "Metallic";

    public static final String VISTA_VISUAL_STYLE = "NormalColor";
    
    /**
     * {@inheritDoc}
     */
    @Override
    protected boolean matches() {
        if (isSystemAddon()) {
            String laf = getLookAndFeel().getClass().getName();
            
            //special-case the jgoodies to ensure that we can match it
            return getSystemLookAndFeelClassName().equals(laf)
                    || "com.jgoodies.looks.windows.WindowsLookAndFeel".equals(laf);
        }
        
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected boolean isSystemAddon() {
        return OS.isWindows() && OS.isUsingWindowsVisualStyles();
    }

//  JW: reverting ...     
//    /**
//     * {@inheritDoc} <p>
//     * 
//     */
//    @Override
//    public void initialize() {
//        super.initialize();
//        // fix Issue #1305-swingx: wrapper for core issue #6753637
//        // set ui property to prevent eating mousePressed when closing popup
//        UIManager.put("PopupMenu.consumeEventOnClose", Boolean.FALSE);
//    }
//
//    /**
//     * {@inheritDoc} <p>
//     */
//    @Override
//    public void uninitialize() {
//        // fix Issue #1305-swingx: wrapper for core issue #6753637
//        // remove the ui property again
//        UIManager.put("PopupMenu.consumeEventOnClose", null);
//        super.uninitialize();
//    }
//    
    

}
