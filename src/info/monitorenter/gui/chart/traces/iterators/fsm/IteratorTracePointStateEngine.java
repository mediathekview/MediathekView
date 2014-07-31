/*
 *  IteratorTracePointStateEngineConsecutivePoints.java of project jchart2d, <enterpurposehere>. 
 *  Copyright (C) 2002 - 2013, Achim Westermann, created on Mar 18, 2012
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

package info.monitorenter.gui.chart.traces.iterators.fsm;

import info.monitorenter.gui.chart.IAccumulationFunction;
import info.monitorenter.gui.chart.IAccumulationStrategy;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.ITracePoint2D;

import java.util.LinkedList;
import java.util.List;
import java.util.Random;

/**
 * 
 * 
 * 
 * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
 * 
 */
public class IteratorTracePointStateEngine extends AIteratorITracePointStateEnginge {

  /**
   * The states to work with.
   * <p>
   */
  public enum STATES {
    START, BEFORE_FIRST_VISIBLE, ACCUMULATING_VISIBLE, INVISIBLE_OR_DISCONTINUATION, INVISIBLE_OR_DISCONTINUATION_CONTINUED, END
  };

  /**
   * Constructor with all that is needed for accumulating points.
   * <p>
   * 
   * @param originalTrace
   *          the iterator to decorate with the feature of accumulating points.
   * 
   * @param accumulationFunction
   *          the function to use for point - accumulation.
   * 
   * @param accumulationControl
   *          termination criteria for a single accumulation run.
   */
  public IteratorTracePointStateEngine(ITrace2D originalTrace,
      IAccumulationFunction accumulationFunction,
      final IAccumulationStrategy.IAccumulationControl accumulationControl) {
    super(originalTrace, accumulationFunction, accumulationControl);

  }

  /**
   * Transition table:
   * 
   * <table>
   * <tr>
   * <th>Transition Number</th>
   * <th>State</th>
   * <th>Condition</th>
   * <th>Action/Output</th>
   * <th>Follow State</th>
   * </tr>
   * 
   * <tr>
   * <td>1</td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#START}
   * <td>
   * <td>visible AND NOT discontinuation AND last
   * <td>
   * <td>{point}
   * <td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#END}
   * <td>
   * </tr>
   * <tr>
   * <td>2</td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#START}
   * <td>
   * <td>NOT visible OR discontinuation AND last
   * <td>
   * <td>{}
   * <td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#END}
   * <td>
   * </tr>
   * <tr>
   * <td>3</td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#START}
   * <td>
   * <td>NOT visible OR discontinuation AND NOT last
   * <td>
   * <td>{}
   * <td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#BEFORE_FIRST_VISIBLE}
   * <td>
   * </tr>
   * <tr>
   * <td>4</td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#START}
   * <td>
   * <td>visible AND NOT discontinuation AND NOT last
   * <td>
   * <td>{point}
   * <td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#ACCUMULATING_VISIBLE}
   * <td>
   * </tr>
   * <tr>
   * <td>5</td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#BEFORE_FIRST_VISIBLE}
   * <td>
   * <td>last
   * <td>
   * <td>{point}
   * <td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#END}
   * <td>
   * </tr>
   * <tr>
   * <td>6</td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#BEFORE_FIRST_VISIBLE}
   * <td>
   * <td>NOT visible OR discontinuation AND NOT last
   * <td>
   * <td>{}
   * <td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#BEFORE_FIRST_VISIBLE}
   * <td>
   * </tr>
   * <tr>
   * <td>7</td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#BEFORE_FIRST_VISIBLE}
   * <td>
   * <td>visible AND NOT discontinuation AND NOT last
   * <td>
   * <td>{previous point, point}
   * <td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#ACCUMULATING_VISIBLE}
   * <td>
   * </tr>
   * <tr>
   * <td>8</td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#ACCUMULATING_VISIBLE}
   * <td>
   * <td>last AND accumPoint != previous point
   * <td>
   * <td>{(partially) accumulated point (without point), previous point (because
   * last visible has to be shown), point}
   * <td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#END}
   * <td>
   * </tr>
   * <tr>
   * <td>9</td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#ACCUMULATING_VISIBLE}
   * <td>
   * <td>last AND accumPoint == previous point
   * <td>
   * <td>{previous point (because last visible has to be shown), point}
   * <td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#END}
   * <td>
   * </tr>
   * <tr>
   * <td>10</td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#ACCUMULATING_VISIBLE}
   * <td>
   * <td>visible AND NO discontinuation AND NOT last AND NOT accumulationDone
   * <td>
   * <td>{}, but accumulate
   * <td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#ACCUMULATING_VISIBLE}
   * <td>
   * </tr>
   * <tr>
   * <td>11</td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#ACCUMULATING_VISIBLE}
   * <td>
   * <td>visible AND NOT discontinuation AND NOT last AND accumulationDone AND accumPoint == previoiusPoint
   * <td>
   * <td>{}, but erase previous accumulation and accumulate
   * <td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#ACCUMULATING_VISIBLE}
   * <td>
   * </tr>
   * <tr>
   * <td>13</td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#ACCUMULATING_VISIBLE}
   * <td>
   * <td>visible AND NOT discontinuation AND NOT last AND accumulationDone AND accumPoint != previoiusPoint
   * <td>
   * <td>{accumPoint}, erase previous accumulation and accumulate
   * <td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#ACCUMULATING_VISIBLE}
   * <td>
   * </tr>
   * <tr>
   * <td>13</td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#ACCUMULATING_VISIBLE}
   * <td>
   * <td>NOT visible OR discontinuation AND NOT last AND accumCount > 0
   * <td>
   * <td>{(partially) accumulated point,previous point (because last visible has
   * to be shown),point}
   * <td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#INVISIBLE_OR_DISCONTINUATION}
   * <td>
   * </tr>
   * <tr>
   * <td>14</td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#ACCUMULATING_VISIBLE}
   * <td>
   * <td>NOT visible OR discontinuation AND NOT last AND accumCount == 0
   * <td>
   * <td>{(partially) accumulated point,previous point (because last visible has
   * to be shown),point}
   * <td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#INVISIBLE_OR_DISCONTINUATION}
   * <td>
   * </tr>
   * <tr>
   * <td>15</td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#INVISIBLE_OR_DISCONTINUATION}
   * <td>
   * <td>last
   * <td>
   * <td>{previous point, point}
   * <td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#END}
   * <td>
   * </tr>
   * <tr>
   * <td>16</td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#INVISIBLE_OR_DISCONTINUATION_CONTINUED}
   * <td>
   * <td>invisible OR discontinuation AND NOT last
   * <td>
   * <td>{}
   * <td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#INVISIBLE_OR_DISCONTINUATION}
   * <td>
   * </tr>
   * <tr>
   * <td>17</td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#INVISIBLE_OR_DISCONTINUATION}
   * <td>
   * <td>visible AND NOT discontinuation AND NOT last
   * <td>
   * <td>{point}
   * <td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#ACCUMULATING_VISIBLE}
   * <td>
   * </tr>
   * <tr>
   * <td>18</td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#INVISIBLE_OR_DISCONTINUATION_CONTINUED}
   * <td>amountOfPoints
   * <td>last
   * <td>
   * <td>{previous point, point}
   * <td>
   * <td>conditional
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#END}
   * <td>
   * </tr>
   * <tr>
   * <td>19</td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#INVISIBLE_OR_DISCONTINUATION}
   * <td>
   * <td>invisible OR discontinuation AND NOT last
   * <td>
   * <td>{}
   * <td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#INVISIBLE_OR_DISCONTINUATION_CONTINUED}
   * <td>
   * </tr>
   * <tr>
   * <td>20</td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#INVISIBLE_OR_DISCONTINUATION_CONTINUED}
   * <td>
   * <td>visible AND NOT discontinuation AND NOT last
   * <td>
   * <td>{previous point, point}
   * <td>
   * <td>
   * {@link info.monitorenter.gui.chart.traces.iterators.fsm.IteratorTracePointStateEngine.STATES#ACCUMULATING_VISIBLE}
   * <td>
   * </tr>
   * </table>
   * 
   * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge#initTransitionTable()
   */
  @Override
  protected List<Transition>[] initTransitionTable() {

    @SuppressWarnings("unchecked")
    List<Transition>[] transitionTable = new List[9];

    // 1: Condition
    ICondition conditionVisibleLast = new ACondition("conditionVisibleLast") {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.ICondition#isMet(info.monitorenter.gui.chart.ITracePoint2D)
       */
      @Override
      public boolean isMet(final ITracePoint2D input, final boolean isLastPoint) {
        boolean result = isLastPoint && input.isVisble() && !input.isDiscontinuation();
        return result;
      }
    };

    // 1: Action
    IAction actionOutputPoint = new AAction("actionOutputPoint") {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.IAction#computeOutput(info.monitorenter.gui.chart.ITracePoint2D,
       *      java.util.List)
       */
      @Override
      public void computeOutput(ITracePoint2D input, List<ITracePoint2D> outputTarget) {
        outputTarget.add(input);
      }
    };
    // 1: Transition
    Transition transition1 = new Transition(conditionVisibleLast, actionOutputPoint, STATES.END);

    // 1: Add to transition List for state START:
    List<Transition> transitionsSTART = new LinkedList<AIteratorITracePointStateEnginge.Transition>();
    transitionsSTART.add(transition1);
    transitionTable[STATES.START.ordinal()] = transitionsSTART;

    // 2: Condition
    ICondition conditionInvisibleLast = new ACondition("conditionInvisibleLast") {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.ICondition#isMet(info.monitorenter.gui.chart.ITracePoint2D)
       */
      @Override
      public boolean isMet(final ITracePoint2D input, final boolean isLastPoint) {
        boolean result;
        boolean visible = input.isVisble();
        boolean discontinuation = input.isDiscontinuation();
        result = (!visible || discontinuation) && isLastPoint;
        return result;
      }
    };

    // 2: Action
    IAction actionNoOutput = new AAction("actionNoOutput") {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.IAction#computeOutput(info.monitorenter.gui.chart.ITracePoint2D,
       *      java.util.List)
       */
      @Override
      public void computeOutput(ITracePoint2D input, List<ITracePoint2D> outputTarget) {
        // NOP
      }

    };

    // 2: Transition
    Transition transition2 = new Transition(conditionInvisibleLast, actionNoOutput, STATES.END);

    // 2: Add to transition List for state START:
    transitionsSTART.add(transition2);

    // 3: Condition
    ICondition conditionInvisibleNotLast = new ACondition("conditionInvisibleNotLast") {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.ICondition#isMet(info.monitorenter.gui.chart.ITracePoint2D)
       */
      @Override
      public boolean isMet(final ITracePoint2D input, final boolean isLastPoint) {
        boolean result = (!input.isVisble() || input.isDiscontinuation()) && !isLastPoint;
        return result;
      }
    };

    // 3: Action - it's actionNoOutput

    // 3: Transition
    Transition transition3 = new Transition(conditionInvisibleNotLast, actionNoOutput,
        STATES.BEFORE_FIRST_VISIBLE);

    // 3: Add to transition List for state START:
    transitionsSTART.add(transition3);

    // 4: Condition
    ICondition conditionVisibleNotLast = new ACondition("conditionVisibleNotLast") {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.ICondition#isMet(info.monitorenter.gui.chart.ITracePoint2D)
       */
      @Override
      public boolean isMet(final ITracePoint2D input, final boolean isLastPoint) {
        boolean result = (input.isVisble() && !input.isDiscontinuation()) && !isLastPoint;
        return result;
      }
    };

    // 4: Action - it's actionOutputPoint

    // 4: Transition
    Transition transition4 = new Transition(conditionVisibleNotLast, actionOutputPoint,
        STATES.ACCUMULATING_VISIBLE);

    // 4: Add to transition List for state START:
    transitionsSTART.add(transition4);

    // 5: Condition
    ICondition conditionLast = new ACondition("conditionLast") {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.ICondition#isMet(info.monitorenter.gui.chart.ITracePoint2D)
       */
      @Override
      public boolean isMet(final ITracePoint2D input, final boolean isLastPoint) {
        boolean result = isLastPoint;
        return result;
      }
    };
    // 5: Action - it's actionOutputPoint

    // 5: Transition
    Transition transition5 = new Transition(conditionLast, actionOutputPoint, STATES.END);

    // 5: Add to transition List for state BEFORE_FIRST_VISIBLE:
    List<Transition> transitionsBEFORE_FIRST_VISIBLE = new LinkedList<AIteratorITracePointStateEnginge.Transition>();
    transitionsBEFORE_FIRST_VISIBLE.add(transition5);
    transitionTable[STATES.BEFORE_FIRST_VISIBLE.ordinal()] = transitionsBEFORE_FIRST_VISIBLE;

    // 6: Condition - it's conditionInvisibleNotLast

    // 6: Action - it's actionNoOutput

    // 6: Transition
    Transition transition6 = new Transition(conditionInvisibleNotLast, actionNoOutput,
        STATES.BEFORE_FIRST_VISIBLE);

    // 6: Add to transition List for state BEFORE_FIRST_VISIBLE:
    transitionsBEFORE_FIRST_VISIBLE.add(transition6);

    // 7: Condition - it's conditionVisibleNotLast

    // 7: Action
    IAction actionOutputPreviousPointAndPoint = new IAction() {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.IAction#computeOutput(info.monitorenter.gui.chart.ITracePoint2D,
       *      java.util.List)
       */
      @Override
      public void computeOutput(ITracePoint2D input, List<ITracePoint2D> outputTarget) {
        outputTarget.add(IteratorTracePointStateEngine.this.getPreviousPoint());
        outputTarget.add(input);
      }
    };
  
    // 7: Transition
    Transition transition7 = new Transition(conditionVisibleNotLast,
        actionOutputPreviousPointAndPoint, STATES.ACCUMULATING_VISIBLE);

    // 7: Add to transition List for state BEFORE_FIRST_VISIBLE:
    transitionsBEFORE_FIRST_VISIBLE.add(transition7);
    
    // 8: Condition
    ICondition conditionLastAccumulatedNothing = new ACondition("conditionLastAccumulatedNothing") {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.ICondition#isMet(info.monitorenter.gui.chart.ITracePoint2D)
       */
      @Override
      public boolean isMet(final ITracePoint2D input, final boolean isLastPoint) {
        boolean result;
        IAccumulationFunction function = IteratorTracePointStateEngine.this
            .getAccumulationFunction();
        boolean accumulated;
        accumulated = function.getAccumulatedPointCount() > 0;
        result = !accumulated && isLastPoint;
        return result;
      }
    };

    // 8: Action - it's actionOutputPoint

    // 8: Transition
    Transition transition8 = new Transition(conditionLastAccumulatedNothing,
        actionOutputPoint, STATES.END);

    // 8: Add to transition List for state ACCUMULATING_VISIBLE:
    List<Transition> transitionsACCUMULATING_VISIBLE = new LinkedList<AIteratorITracePointStateEnginge.Transition>();
    transitionsACCUMULATING_VISIBLE.add(transition8);

    // 9: Condition
    ICondition conditionLastAccumulatedOnlyOnePoint = new ACondition("conditionLastAccumulatedOnlyOnePoint") {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.ICondition#isMet(info.monitorenter.gui.chart.ITracePoint2D)
       */
      @Override
      public boolean isMet(final ITracePoint2D input, final boolean isLastPoint) {
        boolean result;
        IAccumulationFunction function = IteratorTracePointStateEngine.this
            .getAccumulationFunction();
        ITracePoint2D accumPoint = function.getAccumulatedPointCurrent();
        ITracePoint2D previousPoint = IteratorTracePointStateEngine.this.getPreviousPoint();
        boolean accumulated;
        accumulated = function.getAccumulatedPointCount() > 0;
        boolean accumulatedEqualsPreviousPoint;
        accumulatedEqualsPreviousPoint = previousPoint != null && accumPoint != null && accumPoint.equals(previousPoint);
        result = accumulated && isLastPoint && accumulatedEqualsPreviousPoint;
        return result;
      }
    };

    // 9: Action
    IAction actionOutputAcuumPointAndPoint = new AAction(
        "actionOutputAcuumPointAndPoint") {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.IAction#computeOutput(info.monitorenter.gui.chart.ITracePoint2D,
       *      java.util.List)
       */
      @Override
      public void computeOutput(ITracePoint2D input, List<ITracePoint2D> outputTarget) {

        IAccumulationFunction function = IteratorTracePointStateEngine.this
            .getAccumulationFunction();
        ITracePoint2D accumulatedPoint = function.getAccumulatedPoint();
        outputTarget.add(accumulatedPoint);
        outputTarget.add(input);
      }
    };

    // 9: Transition
    Transition transition9 = new Transition(conditionLastAccumulatedOnlyOnePoint,
        actionOutputAcuumPointAndPoint, STATES.END);

    // 9: Add to transition List for state ACCUMULATING_VISIBLE:
    transitionsACCUMULATING_VISIBLE.add(transition9);
    transitionTable[STATES.ACCUMULATING_VISIBLE.ordinal()] = transitionsACCUMULATING_VISIBLE;
    
    // 10: Condition
    ICondition conditionLastAccumulatedSomething = new ACondition("conditionLastAccumulatedSomething") {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.ICondition#isMet(info.monitorenter.gui.chart.ITracePoint2D)
       */
      @Override
      public boolean isMet(final ITracePoint2D input, final boolean isLastPoint) {
        boolean result;
        IAccumulationFunction function = IteratorTracePointStateEngine.this
            .getAccumulationFunction();
        ITracePoint2D accumPoint = function.getAccumulatedPointCurrent();
        ITracePoint2D previousPoint = IteratorTracePointStateEngine.this.getPreviousPoint();
        boolean accumulated;
        accumulated = function.getAccumulatedPointCount() > 0;
        boolean accumulatedEqualsPreviousPoint;
        accumulatedEqualsPreviousPoint = previousPoint != null && accumPoint!= null && accumPoint.equals(previousPoint);
        result = accumulated && isLastPoint && ! accumulatedEqualsPreviousPoint;
        return result;
      }
    };
    
    // 10: Action 
    IAction actionOutputAccumulatedPointAndPreviousAndCurrent = new AAction(
        "actionOutputAccumulatedPointAndPreviousAndCurrent") {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.IAction#computeOutput(info.monitorenter.gui.chart.ITracePoint2D,
       *      java.util.List)
       */
      @Override
      public void computeOutput(ITracePoint2D input, List<ITracePoint2D> outputTarget) {

        IAccumulationFunction function = IteratorTracePointStateEngine.this
            .getAccumulationFunction();
        outputTarget.add(function.getAccumulatedPoint());
        outputTarget.add(IteratorTracePointStateEngine.this.getPreviousPoint());
        outputTarget.add(input);
      }
    };

    
    // 10: Transition
    Transition transition10 = new Transition(conditionLastAccumulatedSomething,
        actionOutputAccumulatedPointAndPreviousAndCurrent, STATES.END);
    transitionsACCUMULATING_VISIBLE.add(transition10);
    
    // 11: Condition
    ICondition conditionVisibleNotLastAndNotAccumulationDone = new ACondition(
        "conditionVisibleNotLastAndNotAccumulationDone") {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.ICondition#isMet(info.monitorenter.gui.chart.ITracePoint2D)
       */
      @Override
      public boolean isMet(final ITracePoint2D input, final boolean isLastPoint) {
        boolean result;
        IAccumulationStrategy.IAccumulationControl accumulationControl = IteratorTracePointStateEngine.this
            .getAccumulationControl();
        boolean accumulationDone = accumulationControl.isAccumulationDone();
        boolean visible = input.isVisble();
        boolean discontinuation = input.isDiscontinuation();
        result = (visible && !discontinuation) && !accumulationDone && !isLastPoint;
        return result;
      }
    };

    // 11: Action
    IAction actionAccumulatePointNoOutput = new AAction("actionAccumulatePointNoOutput") {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.IAction#computeOutput(info.monitorenter.gui.chart.ITracePoint2D,
       *      java.util.List)
       */
      @Override
      public void computeOutput(ITracePoint2D input, List<ITracePoint2D> outputTarget) {

        IAccumulationStrategy.IAccumulationControl accumulationControl = IteratorTracePointStateEngine.this
            .getAccumulationControl();
        // consumes the accumulation and implants input into next accumulation:
        ITracePoint2D accumulatedPoint = accumulationControl.getAccumulatedPointIfAccumulationDone(
            IteratorTracePointStateEngine.this.getAccumulationFunction(), input);
        if (accumulatedPoint != null) {
          throw new RuntimeException(
              "Programming error: Not expected accumulation to be ready. This action should only be invoked if the condition of the wrapping transition returned true.");
        }
      }
    };

    // 11: Transition
    Transition transition11 = new Transition(conditionVisibleNotLastAndNotAccumulationDone,
        actionAccumulatePointNoOutput, STATES.ACCUMULATING_VISIBLE);

    // 11: Add to transition List for state ACCUMULATING_VISIBLE:
    transitionsACCUMULATING_VISIBLE.add(transition11);

    // 12: Condition
    ICondition conditionVisibleNotLastAndAccumulationDoneAndAccumPointIsPreviousPoint = new ACondition(
        "conditionVisibleNotLastAndAccumulationDoneAndAccumPointIsPreviousPoint") {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.ICondition#isMet(info.monitorenter.gui.chart.ITracePoint2D)
       */
      @Override
      public boolean isMet(final ITracePoint2D input, final boolean isLastPoint) {
        boolean result;
        IAccumulationStrategy.IAccumulationControl accumulationControl = IteratorTracePointStateEngine.this
            .getAccumulationControl();
        boolean accumulationDone = accumulationControl.isAccumulationDone();
        boolean visible = input.isVisble();
        boolean discontinuation = input.isDiscontinuation();
        IAccumulationFunction function= IteratorTracePointStateEngine.this.getAccumulationFunction();
        ITracePoint2D accumulatedPoint = function.getAccumulatedPointCurrent();
        boolean accumulatedPreviousPoint = accumulatedPoint != null
            && !IteratorTracePointStateEngine.this.getPreviousPoint().equals(accumulatedPoint);
        result = (visible && !discontinuation) && accumulationDone && !isLastPoint && !accumulatedPreviousPoint;
        return result;
      }
    };

    // 12: Action
    IAction actionEraseAccumulatePoint = new AAction("actionEraseAccumulatePoint") {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.IAction#computeOutput(info.monitorenter.gui.chart.ITracePoint2D,
       *      java.util.List)
       */
      @Override
      public void computeOutput(ITracePoint2D input, List<ITracePoint2D> outputTarget) {

        IAccumulationFunction function = IteratorTracePointStateEngine.this.getAccumulationFunction();
        // erase: 
        ITracePoint2D accumPoint = function.getAccumulatedPoint();
        // accumulate current
        function.addPointToAccumulate(input);
      }
    };

    // 12: Transition
    Transition transition12 = new Transition(conditionVisibleNotLastAndAccumulationDoneAndAccumPointIsPreviousPoint,
        actionEraseAccumulatePoint, STATES.ACCUMULATING_VISIBLE);

    // 12: Add to transition List for state ACCUMULATING_VISIBLE:
    transitionsACCUMULATING_VISIBLE.add(transition12);

    
    // 13: Condition
    ICondition conditionVisibleNotLastAndAccumulationDoneAndAccumPointNotPreviousPoint = new ACondition(
        "conditionVisibleNotLastAndAccumulationDoneAndAccumPointNotPreviousPoint") {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.ICondition#isMet(info.monitorenter.gui.chart.ITracePoint2D)
       */
      @Override
      public boolean isMet(final ITracePoint2D input, final boolean isLastPoint) {
        boolean result;
        IAccumulationStrategy.IAccumulationControl accumulationControl = IteratorTracePointStateEngine.this
            .getAccumulationControl();
        boolean accumulationDone = accumulationControl.isAccumulationDone();
        boolean visible = input.isVisble();
        boolean discontinuation = input.isDiscontinuation();
        IAccumulationFunction function= IteratorTracePointStateEngine.this.getAccumulationFunction();
        ITracePoint2D accumulatedPoint = function.getAccumulatedPointCurrent();
        boolean accumulatedPreviousPoint = accumulatedPoint != null
            && IteratorTracePointStateEngine.this.getPreviousPoint().equals(accumulatedPoint);
        result = (visible && !discontinuation) && accumulationDone && !isLastPoint && !accumulatedPreviousPoint;
        return result;
      }
    };

    // 13: Action
    IAction actionAccumulatePointOutput = new AAction("actionAccumulatePointOutput") {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.IAction#computeOutput(info.monitorenter.gui.chart.ITracePoint2D,
       *      java.util.List)
       */
      @Override
      public void computeOutput(ITracePoint2D input, List<ITracePoint2D> outputTarget) {

        IAccumulationStrategy.IAccumulationControl accumulationControl = IteratorTracePointStateEngine.this
            .getAccumulationControl();
        // consumes the accumulation and implants input into next accumulation:
        ITracePoint2D accumulatedPoint = accumulationControl.getAccumulatedPointIfAccumulationDone(
            IteratorTracePointStateEngine.this.getAccumulationFunction(), input);
        if (accumulatedPoint == null) {
          throw new RuntimeException(
              "Programming error: Expected accumulation to be ready. This action should only be invoked if the condition of the wrapping transition returned true.");
        }
        outputTarget.add(accumulatedPoint);
      }
    };

    // 13: Transition
    Transition transition13 = new Transition(conditionVisibleNotLastAndAccumulationDoneAndAccumPointNotPreviousPoint,
        actionAccumulatePointOutput, STATES.ACCUMULATING_VISIBLE);

    // 13: Add to transition List for state ACCUMULATING_VISIBLE:
    transitionsACCUMULATING_VISIBLE.add(transition13);
    
    
    
    
    
    
    
    
    
    
    
    
    // 14: Condition
    ICondition conditionInvisibleNotLastAndAccumulated = new ACondition(
        "conditionInvisibleNotLastAndAccumulated") {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.ICondition#isMet(info.monitorenter.gui.chart.ITracePoint2D)
       */
      @Override
      public boolean isMet(final ITracePoint2D input, final boolean isLastPoint) {
        boolean result;
        IAccumulationFunction function = IteratorTracePointStateEngine.this
            .getAccumulationFunction();
        ITracePoint2D accumPoint = function.getAccumulatedPointCurrent();
        ITracePoint2D previousPoint = IteratorTracePointStateEngine.this.getPreviousPoint();
        boolean accumulated = accumPoint!= null && !accumPoint.equals(previousPoint);
        boolean visible = input.isVisble();
        result = !visible && accumulated && !isLastPoint;
        return result;
      }
    };
    // 14: Action - it's actionOutputAccumulatedPointAndPreviousAndCurrent


    // 14: Transition
    Transition transition14 = new Transition(conditionInvisibleNotLastAndAccumulated,
        actionOutputAccumulatedPointAndPreviousAndCurrent, STATES.INVISIBLE_OR_DISCONTINUATION);

    // 14: Add to transition List for state ACCUMULATING_VISIBLE:
    transitionsACCUMULATING_VISIBLE.add(transition14);

    // 15: Condition 
    ICondition conditionInvisibleNotLastAndAccumulated1Point   = new ACondition(
        "conditionInvisibleNotLastAndAccumulated1Point") {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.ICondition#isMet(info.monitorenter.gui.chart.ITracePoint2D)
       */
      @Override
      public boolean isMet(final ITracePoint2D input, final boolean isLastPoint) {
        boolean result;
        IAccumulationFunction function = IteratorTracePointStateEngine.this
            .getAccumulationFunction();
        ITracePoint2D accumPoint = function.getAccumulatedPointCurrent();
        ITracePoint2D previousPoint = IteratorTracePointStateEngine.this.getPreviousPoint();
        boolean accumulatedOnePoint = accumPoint!= null && accumPoint.equals(previousPoint);
        boolean visible = input.isVisble();
        result = !visible && accumulatedOnePoint && !isLastPoint;
        return result;
      }
    };
    
    // 15: Action - it's actionOutputPreviousPointAndPoint
    
    // 15: Transition
    Transition transition15 = new Transition(conditionInvisibleNotLastAndAccumulated1Point,
        actionOutputPreviousPointAndPoint, STATES.INVISIBLE_OR_DISCONTINUATION);
    
    // 15: Add to transition List for state ACCUMULATING_VISIBLE:
    transitionsACCUMULATING_VISIBLE.add(transition15);
    
    // 16: Condition
    ICondition conditionInvisibleNotLastAndNotAccumulated = new ACondition(
        "conditionInvisibleNotLastAndNotAccumulated") {

      /**
       * @see info.monitorenter.gui.chart.traces.iterators.fsm.AIteratorITracePointStateEnginge.ICondition#isMet(info.monitorenter.gui.chart.ITracePoint2D)
       */
      @Override
      public boolean isMet(final ITracePoint2D input, final boolean isLastPoint) {
        boolean result;
        IAccumulationFunction function = IteratorTracePointStateEngine.this
            .getAccumulationFunction();
        ITracePoint2D accumPoint = function.getAccumulatedPointCurrent();
        ITracePoint2D previousPoint = IteratorTracePointStateEngine.this.getPreviousPoint();
        boolean accumulated = accumPoint!= null && !accumPoint.equals(previousPoint);
        boolean visible = input.isVisble();
        result = !visible && !accumulated && !isLastPoint;
        return result;
      }
    };

    // 16: Action - it's actionOutputPoint

    // 16: Transition
    Transition transition16 = new Transition(conditionInvisibleNotLastAndNotAccumulated,
        actionOutputPoint, STATES.INVISIBLE_OR_DISCONTINUATION);

    // 16: Add to transition List for state ACCUMULATING_VISIBLE:
    transitionsACCUMULATING_VISIBLE.add(transition16);

    // 17: Condition - it's conditionLast

    // 17: Action - it's actionOutputPoint

    // 17: Transition
    Transition transition17 = new Transition(conditionLast, actionOutputPoint, STATES.END);

    // 17: Add to transition List for state INVISIBLE_OR_DISCONTINUATION:
    List<Transition> transitionsINVISIBLE_OR_DISCONTINUATION = new LinkedList<AIteratorITracePointStateEnginge.Transition>();
    transitionsINVISIBLE_OR_DISCONTINUATION.add(transition17);
    transitionTable[STATES.INVISIBLE_OR_DISCONTINUATION.ordinal()] = transitionsINVISIBLE_OR_DISCONTINUATION;

    // 18: Condition - it's conditionInvisibleNotLast

    // 18: Action - it's actionNoOutput

    // 18: Transition
    Transition transition18 = new Transition(conditionInvisibleNotLast, actionNoOutput,
        STATES.INVISIBLE_OR_DISCONTINUATION_CONTINUED);

    // 18: Add to transition List for state INVISIBLE_OR_DISCONTINUATION:
    transitionsINVISIBLE_OR_DISCONTINUATION.add(transition18);

    // 19: Condition - it's conditionVisibleNotLast

    // 19: Action - it's actionOutputPoint
   
    // 19: Transition
    Transition transition19 = new Transition(conditionVisibleNotLast, actionOutputPoint,
        STATES.ACCUMULATING_VISIBLE);

    // 19: Add to transition List for state INVISIBLE_OR_DISCONTINUATION:
    transitionsINVISIBLE_OR_DISCONTINUATION.add(transition19);

    // 20: Condition - it's conditionLast

    // 20: Action - it's actionOutputPreviousPointAndPoint

    // 20: Transition
    Transition transition20 = new Transition(conditionLast, actionOutputPreviousPointAndPoint,
        STATES.END);

    // 20: Add to transition List for state
    // INVISIBLE_OR_DISCONTINUATION_CONTINUED:
    List<Transition> transitionsINVISIBLE_OR_DISCONTINUATION_CONTINUED = new LinkedList<AIteratorITracePointStateEnginge.Transition>();
    transitionsINVISIBLE_OR_DISCONTINUATION_CONTINUED.add(transition20);
    transitionTable[STATES.INVISIBLE_OR_DISCONTINUATION_CONTINUED.ordinal()] = transitionsINVISIBLE_OR_DISCONTINUATION_CONTINUED;

    // 21: Condition - it's conditionInvisibleNotLast

    // 21: Action - it's actionNoOutput

    // 21: Transition
    Transition transition21 = new Transition(conditionInvisibleNotLast, actionNoOutput,
        STATES.INVISIBLE_OR_DISCONTINUATION_CONTINUED);

    // 21: Add to transition List for state
    // INVISIBLE_OR_DISCONTINUATION_CONTINUED:
    transitionsINVISIBLE_OR_DISCONTINUATION_CONTINUED.add(transition21);

    // 22: Condition - it's conditionVisibleNotLast

    // 22: Action - it's actionOutputPreviousPointAndPoint  
  
    // 22: Transition
    Transition transition22 = new Transition(conditionVisibleNotLast,
        actionOutputPreviousPointAndPoint, STATES.ACCUMULATING_VISIBLE);

    // 22: Add to transition List for state START:
    transitionsINVISIBLE_OR_DISCONTINUATION_CONTINUED.add(transition22);

    /*
     * END state has no transitions. At least leave an empty list for it at the
     * transition table.
     */
    List<Transition> transitionsEND = new LinkedList<AIteratorITracePointStateEnginge.Transition>();
    transitionTable[STATES.END.ordinal()] = transitionsEND;

    // set the start state:
    this.setStartState(STATES.START);

    /*
     * uff: done.
     * 
     * Note: I would avoid writing methods with so many lines of code. But in
     * this case all the documentation required for understanding the mealy
     * state machine with guard condition as a whole is assigned to the method
     * and breaking it down to smaller methods (that e.g. each set up the
     * transitions from a single state) would perhaps go into depths in which a
     * method-reader would find a hard time understanding what is going on.
     */
    return transitionTable;

  }
}
