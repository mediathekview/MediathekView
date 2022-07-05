/*
 * $Id: HeaderAddon.java 2474 2007-11-21 17:32:04Z kschaefe $
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

import org.jdesktop.swingx.JXHeader;

import javax.swing.*;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.FontUIResource;
import java.awt.*;

/**
 * Addon for <code>JXHeader</code>.<br>
 *
 */
public class HeaderAddon extends AbstractComponentAddon {

    public HeaderAddon() {
        super("JXHeader");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void addBasicDefaults(LookAndFeelAddons addon, DefaultsList defaults) {
        super.addBasicDefaults(addon, defaults);
        
        defaults.add(JXHeader.uiClassID, "org.jdesktop.swingx.plaf.basic.BasicHeaderUI");
        //TODO image is missing
        defaults.add("JXHeader.defaultIcon",
                LookAndFeel.makeIcon(HeaderAddon.class, "basic/resources/header-default.png"));
        //TODO use safe methods
        defaults.add("JXHeader.titleFont", new FontUIResource(UIManager.getFont("Label.font").deriveFont(Font.BOLD)));
        defaults.add("JXHeader.titleForeground", UIManager.getColor("Label.foreground"));
        defaults.add("JXHeader.descriptionFont", UIManager.getFont("Label.font"));
        defaults.add("JXHeader.descriptionForeground", UIManager.getColor("Label.foreground"));
        defaults.add("JXHeader.background",
                UIManagerExt.getSafeColor("control", new ColorUIResource(Color.decode("#C0C0C0"))));
        defaults.add("JXHeader.startBackground", new ColorUIResource(Color.WHITE));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void addMacDefaults(LookAndFeelAddons addon, DefaultsList defaults) {
        super.addMacDefaults(addon, defaults);
        
        defaults.add("JXHeader.background", new ColorUIResource(new Color(218, 218, 218)));
        defaults.add("JXHeader.startBackground", new ColorUIResource(new Color(235, 235, 235)));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void addNimbusDefaults(LookAndFeelAddons addon, DefaultsList defaults) {
        super.addNimbusDefaults(addon, defaults);
        
        defaults.add("JXHeader.background", new ColorUIResource(new Color(214, 217, 223, 255)));
    }
}