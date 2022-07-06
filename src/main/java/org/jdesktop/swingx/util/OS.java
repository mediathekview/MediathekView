/*
 * $Id: OS.java 4088 2011-11-17 19:53:49Z kschaefe $
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
package org.jdesktop.swingx.util;

import org.apache.commons.lang3.SystemUtils;

import javax.swing.*;
import java.awt.*;

/**
 * Provides methods related to the runtime environment.
 */
@SuppressWarnings("nls")
public class OS {

  /**
   * @return true if the VM is running Windows and the Java
   *         application is rendered using XP Visual Styles.
   */
  public static boolean isUsingWindowsVisualStyles() {
    if (!SystemUtils.IS_OS_WINDOWS) {
      return false;
    }

    boolean xpthemeActive = Boolean.TRUE.equals(Toolkit.getDefaultToolkit()
        .getDesktopProperty("win.xpstyle.themeActive"));
    if (!xpthemeActive) {
      return false;
    } else {
      try {
        return System.getProperty("swing.noxp") == null;
      } catch (RuntimeException e) {
        return true;
      }
    }
  }

  /**
   * Returns the name of the current Windows visual style.
   * <ul>
   * <li>it looks for a property name "win.xpstyle.name" in UIManager and if not found
   * <li>it queries the win.xpstyle.colorName desktop property ({@link Toolkit#getDesktopProperty(String)})
   * </ul>
   * 
   * @return the name of the current Windows visual style if any. 
   */
  public static String getWindowsVisualStyle() {
    String style = UIManager.getString("win.xpstyle.name");
    if (style == null) {
      // guess the name of the current XPStyle
      // (win.xpstyle.colorName property found in awt_DesktopProperties.cpp in
      // JDK source)
      style = (String)Toolkit.getDefaultToolkit().getDesktopProperty(
        "win.xpstyle.colorName");
    }
    return style;
  }
  
}
