/*
 * $Id: HyperlinkAddon.java 3745 2010-08-06 03:02:52Z kschaefe $
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


import org.jdesktop.swingx.JXHyperlink;

import javax.swing.plaf.ColorUIResource;

/**
 * Addon for <code>JXHyperlink</code>.<br>
 */
public class HyperlinkAddon extends AbstractComponentAddon {
    public HyperlinkAddon() {
        super("JXHyperlink");
    }

    @Override
    protected void addBasicDefaults(LookAndFeelAddons addon, DefaultsList defaults) {
        super.addBasicDefaults(addon, defaults);

        defaults.add(JXHyperlink.uiClassID, "org.jdesktop.swingx.plaf.basic.BasicHyperlinkUI");
        //using CSS pseudo classes for Color types
        defaults.add("Hyperlink.linkColor", new ColorUIResource(0, 0x33, 0xFF));
        defaults.add("Hyperlink.visitedColor", new ColorUIResource(0x99, 0, 0x99));
        defaults.add("Hyperlink.hoverColor", new ColorUIResource(0xFF, 0x33, 0));
        defaults.add("Hyperlink.activeColor", new ColorUIResource(0xFF, 0x33, 0));
    }
}
