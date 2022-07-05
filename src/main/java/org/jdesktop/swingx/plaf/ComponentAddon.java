/*
 * $Id: ComponentAddon.java 4028 2011-06-03 19:32:19Z kschaefe $
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

/**
 * Each new component type of the library will contribute an addon to
 * the LookAndFeelAddons. A <code>ComponentAddon</code> is the
 * equivalent of a {@link javax.swing.LookAndFeel}but focused on one
 * component. <br>
 * 
 * @author <a href="mailto:fred@L2FProd.com">Frederic Lavigne</a>
 */
public interface ComponentAddon {

  /**
   * @return the name of this addon
   */
  String getName();

  /**
   * Initializes this addon (i.e register UI classes, colors, fonts,
   * borders, any UIResource used by the component class). When
   * initializing, the addon can register different resources based on
   * the addon or the current look and feel.
   * 
   * @param addon the current addon
   */
  void initialize(LookAndFeelAddons addon);

  /**
   * Uninitializes this addon.
   * 
   * @param addon
   */
  void uninitialize(LookAndFeelAddons addon);

}