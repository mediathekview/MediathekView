/*
 *  AccumulationStrategyAmountOfPoints.java of project jchart2d, accumulation strategy 
 *  that will add n consecutive points not caring about an order of the data. 
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
import info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine;

import java.util.Iterator;
/**
 * This strategy will just accumulate <code>amountOfPoints</code>
 * consecutive points without caring for data density. Best use this
 * whenever you have unordered (by x value) traces.
 * <p>
 * 
 * @author Achim Westermann
 */
public class AccumulationStrategyAmountOfPoints extends AAccumulationStrategy {

  /**
   * Constructor taking the accumulation function to use.
   * <p>
   * 
   * @param accumulationFunction
   *          the accumulation function to use.
   */
  public AccumulationStrategyAmountOfPoints(IAccumulationFunction accumulationFunction) {
    super(accumulationFunction);
    
  }

  /**
   * @see info.monitorenter.gui.chart.traces.accumulationstrategies.AAccumulationStrategy#iterator(info.monitorenter.gui.chart.ITrace2D, int)
   */
  public Iterator<ITracePoint2D> iterator(final ITrace2D source, final int amountOfPoints) {
    Iterator<ITracePoint2D> result = null;
    IAccumulationControl control = new AccumulationControlConsecutivePoints();
    control.initializeControl(source.getSize(), amountOfPoints);
    if(control.isAccumulationBypassable()) {
      result = source.iterator();
    }else {
      result = new IteratorTracePointStateEngine(source, this.getAccumulationFunction(), control);
    }
    return result;
  }
}
