/*
 *  AccumulationStrategyByPass.java of project jchart2d, a bypass without (lots of) overhead for 
 *  skipping data accumulation. 
 *  Copyright (C) 2002 - 2013, Achim Westermann, created on Dec 11, 2011
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

package info.monitorenter.gui.chart.traces.accumulationstrategies;

import info.monitorenter.gui.chart.IAccumulationFunction;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.ITracePoint2D;

import java.util.Iterator;
/**
 * Bypass for accumulation: Just the given original <code>source</code>
 * argument is returned from it's method
 * <code>{@link #iterator(ITrace2D, int)}</code>.
 * <p>
 * 
 * @author Achim Westermann
 */
public class AccumulationStrategyByPass extends AAccumulationStrategy {
  
  
  /**
   * Constructor taking the accumulation function to use.
   * <p>
   * 
   * @param accumulationFunction
   *          the accumulation function to use.
   */
  public AccumulationStrategyByPass(final IAccumulationFunction accumulationFunction) {
    super(accumulationFunction);
  }

  /**
   * @see info.monitorenter.gui.chart.traces.accumulationstrategies.AAccumulationStrategy#iterator(info.monitorenter.gui.chart.ITrace2D, int)
   */
  public Iterator<ITracePoint2D> iterator(final ITrace2D source, final int amountOfPoints) {
    return source.iterator();
  }
}
