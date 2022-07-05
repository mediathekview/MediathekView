/*
 * $Id: TipLoader.java 542 2005-10-10 18:03:15Z rbair $
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
package org.jdesktop.swingx.tips;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

/**
 * Loads tips from Properties.<br>
 * 
 * @author <a href="mailto:fred@L2FProd.com">Frederic Lavigne</a>
 */
public class TipLoader {

  private TipLoader() { }
  
  /**
   * Initializes a TipOfTheDayModel from properties. Each tip is defined by two
   * properties, its name and its description:
   * 
   * <pre>
   * <code>
   * tip.1.name=First Tip
   * tip.1.description=This is the description
   *  
   * tip.2.name=Second Tip
   * tip.2.description=&lt;html&gt;This is an html description
   * 
   * ...
   * 
   * tip.10.description=No name for this tip, name is optional
   * </code>
   * </pre>
   * 
   * @param props
   * @return a TipOfTheDayModel
   * @throws IllegalArgumentException
   *           if a name is found without description
   */
  public static TipOfTheDayModel load(Properties props) {
    List<TipOfTheDayModel.Tip> tips = new ArrayList<TipOfTheDayModel.Tip>();

    int count = 1;
    while (true) {
      String nameKey = "tip." + count + ".name";
      String nameValue = props.getProperty(nameKey);

      String descriptionKey = "tip." + count + ".description";
      String descriptionValue = props.getProperty(descriptionKey);

      if (nameValue != null && descriptionValue == null) { throw new IllegalArgumentException(
        "No description for name " + nameValue); }

      if (descriptionValue == null) {
        break;
      }

      DefaultTip tip = new DefaultTip(nameValue, descriptionValue);
      tips.add(tip);

      count++;
    }

    return new DefaultTipOfTheDayModel(tips);
  }

}
