/*
 * AccumulationFunctionBypass.java of project jchart2d, <enterpurposehere>.
 * Copyright (C) 2002 - 2013, Achim Westermann, created on Oct 8, 2011
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version. This library is distributed in the hope that it will be
 * useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 * 
 * If you modify or optimize the code in a useful way please let me know.
 * Achim.Westermann@gmx.de
 * 
 * 
 * File : $Source: /cvsroot/jchart2d/jchart2d/codetemplates.xml,v $ Date :
 * $Date: 2009/02/24 16:45:41 $ Version: $Revision: 1.2 $
 */

package info.monitorenter.gui.chart.traces.accumulationfunctions;

import info.monitorenter.gui.chart.IAccumulationFunction;
import info.monitorenter.gui.chart.ITracePoint2D;
import info.monitorenter.gui.chart.traces.accumulationstrategies.AccumulationStrategyByPass;

/**
 * Dummy stub implementation of {@link IAccumulationFunction} that does not work
 * correctly but throws {@link UnsupportedOperationException} from every method and is
 * only intended for being used by {@link AccumulationStrategyByPass}
 * 
 * 
 * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
 * 
 */
public class AccumulationFunctionBypass implements IAccumulationFunction {

  /**
   * Throws a <code>{@link UnsupportedOperationException}</code>.
   * <p>
   * 
   * @throws UnsupportedOperationException
   * 
   * @see info.monitorenter.gui.chart.IAccumulationFunction#addPointToAccumulate(info.monitorenter.gui.chart.ITracePoint2D)
   */
  public void addPointToAccumulate(ITracePoint2D point) {
    throw new UnsupportedOperationException();
  }

  /**
   * Throws a <code>{@link UnsupportedOperationException}</code>.
   * <p>
   * 
   * @throws UnsupportedOperationException
   * 
   * @see info.monitorenter.gui.chart.IAccumulationFunction#getAccumulatedPoint()
   */
  public ITracePoint2D getAccumulatedPoint() {
    throw new UnsupportedOperationException();
  }

  /**
   * Throws a <code>{@link UnsupportedOperationException}</code>.
   * <p>
   * 
   * @throws UnsupportedOperationException
   * 
   * @see info.monitorenter.gui.chart.IAccumulationFunction#getAccumulatedPointCount()
   */
  @Override
  public int getAccumulatedPointCount() {
    throw new UnsupportedOperationException();
  }

  /**
   * Throws a <code>{@link UnsupportedOperationException}</code>.
   * <p>
   * 
   * @throws UnsupportedOperationException
   * 
   * @see info.monitorenter.gui.chart.IAccumulationFunction#getAccumulatedPointCurrent()
   */
  @Override
  public ITracePoint2D getAccumulatedPointCurrent() {
    throw new UnsupportedOperationException();
  }

}
