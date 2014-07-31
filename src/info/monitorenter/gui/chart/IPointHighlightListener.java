/*
 *  IPointHighlighterListener.java of project jchart2d, interface for listeners on higlighted points.
 *  Copyright (C) 2002 - 2013, Achim Westermann, created on Mar 17, 2012
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 *  If you modify or optimize the code in a useful way please let me know.
 *  Achim.Westermann@gmx.de
 *
 *
 * File   : $Source: /cvsroot/jchart2d/jchart2d/codetemplates.xml,v $
 * Date   : $Date: 2009/02/24 16:45:41 $
 * Version: $Revision: 1.2 $
 */

package info.monitorenter.gui.chart;

/**
 * Interface for listeners that get informed by {@link Chart2D} about
 * highlighted points.
 * <p>
 * 
 * @see Chart2D#addPointHighlightListener(IPointHighlightListener)
 * 
 * 
 * @author Frans Bouwmans (contribution)
 * 
 */
public interface IPointHighlightListener {

  /**
   * This function is called upon highlight of a trace point.
   * <p>
   * 
   * @param point
   *          the point which is highlighted
   */
  public abstract void highlight(ITracePoint2D point);

}