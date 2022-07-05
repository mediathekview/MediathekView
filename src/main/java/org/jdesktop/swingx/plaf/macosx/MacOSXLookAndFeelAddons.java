/*
 * $Id: MacOSXLookAndFeelAddons.java 4092 2011-11-30 18:04:36Z kschaefe $
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
package org.jdesktop.swingx.plaf.macosx;

import org.jdesktop.swingx.plaf.LookAndFeelAddons;
import org.jdesktop.swingx.plaf.basic.BasicLookAndFeelAddons;
import org.jdesktop.swingx.util.OS;
import org.kohsuke.MetaInfServices;

import static javax.swing.UIManager.getLookAndFeel;
import static javax.swing.UIManager.getSystemLookAndFeelClassName;

@MetaInfServices(LookAndFeelAddons.class)
public class MacOSXLookAndFeelAddons extends BasicLookAndFeelAddons {
    /**
     * {@inheritDoc}
     */
    @Override
    protected boolean matches() {
        return isSystemAddon() && getSystemLookAndFeelClassName().equals(getLookAndFeel().getClass().getName());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected boolean isSystemAddon() {
        return OS.isMacOSX();
    }
}
