/*
 *  IteratorITracePointStateEnginge.java of project jchart2d, <enterpurposehere>. 
 *  Copyright (C) 2002 - 2013, Achim Westermann, created on Jan 22, 2012
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
import info.monitorenter.gui.chart.traces.iterators.AAccumulationIterator;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * An iterator over trace points that works as a Finite State Machine by
 * utilizing different state instances.
 * <p>
 * While this approach may look a bit over-designed I decided to use it after a
 * plain-forward implementation of accumulation - iterators grew into
 * spaghetti-code.
 * <p>
 * 
 * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
 * 
 */
public abstract class AIteratorITracePointStateEnginge extends AAccumulationIterator {

  /**
   * Internal previous point from the underlying iterator. Needed in case a
   * transition from invisible/discontinuation is taken to visible.
   */
  private ITracePoint2D m_previousPoint;

  /**
   * Returns the previous point read from the underlying iterator.
   * <p>
   * 
   * @return the previousPoint encountered
   */
  protected ITracePoint2D getPreviousPoint() {
    return this.m_previousPoint;
  }

  /**
   * Interface for a condition that folds the mealy input ({@link ITracePoint2D}
   * ) into a boolean.
   * <p>
   * Note: For every input trace point all conditions of a certain state must
   * exclude a result of {@link Boolean#TRUE} amongst each other. There must not
   * be two or more states related to transitions of a state that return
   * {@link Boolean#TRUE} for the same input trace point.
   * <p>
   * 
   * 
   * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
   * 
   */
  public interface ICondition {
    /**
     * Evaluate if the condition (for the corresponding transition) is met.
     * <p>
     * 
     * @param input
     *          the current point read from the underlying iterator to
     *          accumulate from.
     * 
     * @param isLastPoint
     *          true only if the last point is seen in the underlying iterator.
     * 
     * @return true if the condition (for the corresponding transition) is met
     *         and thus should be taken.
     */
    public boolean isMet(final ITracePoint2D input, boolean isLastPoint);
  }

  /**
   * Overrides {@link #toString()} to make this class-cluster a bit more
   * debuggable.
   * <p>
   * 
   * 
   * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
   * 
   */
  public abstract class ACondition implements ICondition {
    /** The name. */
    private String m_name;

    /**
     * Constructor taking a name that will be used for {@link #toString()}.
     * <p>
     * 
     * Prefer using distinct names for all conditions of a state engine for
     * allowing unambiguous debugging.
     * <p>
     * 
     * @param name
     *          a name that will be used for {@link #toString()}.
     */
    public ACondition(final String name) {
      this.m_name = name;
    }

    /**
     * @see java.lang.Object#toString()
     */
    public String toString() {
      return this.m_name;
    }
  }

  /**
   * Interface for an action (upon a transition condition of a state) that knows
   * how to compute the output given for the input.
   * <p>
   * 
   * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
   * 
   */
  public interface IAction {
    /**
     * Compute the output from the given input (and potential owned state
     * information) and append it to the given list for output.
     * <p>
     * Note that this callback will be invoked by an owning {@link Transition}
     * when it's proper {@link ICondition} fell true. So you can avoid handling
     * different cases of input but just use it for the output action (No
     * condition double-checking needed).
     * <p>
     * 
     * @param input
     *          the input trace point.
     * 
     * @param outputTarget
     *          append the output to this list. This design (signature) was
     *          chosen as a single input may cause multiple output trace points.
     */
    public void computeOutput(final ITracePoint2D input, final List<ITracePoint2D> outputTarget);
  }
  
  /**
   * Overrides {@link #toString()} to make this class-cluster a bit more
   * debuggable.
   * <p>
   * 
   * 
   * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
   * 
   */
  public abstract class AAction implements IAction {
    /** The name. */
    private String m_name;

    /**
     * Constructor taking a name that will be used for {@link #toString()}.
     * <p>
     * 
     * Prefer using distinct names for all conditions of a state engine for
     * allowing unambiguous debugging.
     * <p>
     * 
     * @param name
     *          a name that will be used for {@link #toString()}.
     */
    public AAction(final String name) {
      this.m_name = name;
    }

    /**
     * @see java.lang.Object#toString()
     */
    public String toString() {
      return this.m_name;
    }
  }

  // public abstract class AStateWithPreviousPoint implements IState {
  // /** The previous point encountered. */
  // private ITracePoint2D m_previousPoint;
  //
  // /**
  // * Returns the previousPoint encountered.
  // * <p>
  // *
  // * @return the previousPoint encountered
  // */
  // protected ITracePoint2D getPreviousPoint() {
  // return this.m_previousPoint;
  // }
  //
  // /**
  // * Sets the previousPoint encountered.
  // * <p>
  // *
  // * @param previousPoint
  // * the previousPoint to set.
  // */
  // protected void setPreviousPoint(ITracePoint2D previousPoint) {
  // this.m_previousPoint = previousPoint;
  // }
  //
  // }

  /**
   * Implementation of a transition. It is a compound of an {@link ICondition}
   * and the {@link Enum} that follows from the input (in case the condition is
   * true).
   * <p>
   * 
   * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
   * 
   */
  public final class Transition {
    /**
     * The condition (computed from the input {@link ITracePoint2D}) that has to
     * be met to jump to the state of this transition.
     */
    private final ICondition m_condition;

    /**
     * The action to invoke when this transition is taken.
     */
    private final IAction m_action;

    /**
     * Returns the action to invoke when this transition is taken.
     * <p>
     * 
     * @return the action to invoke when this transition is taken.
     */
    public IAction getAction() {
      return this.m_action;
    }

    /**
     * The state that will be the next current state if the condition is met.
     */
    private final Enum< ? > m_stateFollowing;

    /**
     * Creates a transition with a condition and the next state (in case
     * condition falls true).
     * <p>
     * 
     * @param condition
     *          the condition (computed from the input {@link ITracePoint2D})
     *          that has to be met to jump to the state of this transition.
     * 
     * @param action
     *          the action to execute when this transition is taken.
     * 
     * @param stateFollowing
     *          the state that will be the next current state if the condition
     *          is met.
     */
    public Transition(final ICondition condition, final IAction action,
        final Enum< ? > stateFollowing) {
      this.m_condition = condition;
      this.m_action = action;
      this.m_stateFollowing = stateFollowing;
    }

    /**
     * Returns the condition (computed from the input {@link ITracePoint2D})
     * that has to be met to jump to the state of this transition.
     * <p>
     * 
     * @return the condition (computed from the input {@link ITracePoint2D})
     *         that has to be met to jump to the state of this transition.
     */
    public ICondition getCondition() {
      return this.m_condition;
    }

    /**
     * Returns the state that will be the next current state if the condition is
     * met.
     * <p>
     * 
     * @return the state that will be the next current state if the condition is
     *         met.
     */
    public Enum< ? > getStateFollowing() {
      return this.m_stateFollowing;
    }
  }

  /**
   * The central data structure to control the state transition flow.
   * <p>
   * Access to the list of transitions of a state is done by using the
   * {@link Enum#ordinal()} of the concrete {@link Enum} constants in subclasses
   * (performance).
   * <p>
   */
  private List<Transition>[] m_transitionTable;

  /**
   * Returns the transitionTable.
   * <p>
   * 
   * @return the transitionTable
   */
  protected List<Transition>[] getTransitionTable() {
    return this.m_transitionTable;
  }

  /**
   * The current state.
   */
  private Enum< ? > m_currentState;

  /**
   * The start state. Needed e.g. for resetting.
   */
  private Enum< ? > m_startState;

  /**
   * Sets the startState.
   * <p>
   * 
   * @param startState
   *          the startState to set
   */
  protected void setStartState(Enum< ? > startState) {
    this.m_startState = startState;
  }

  /**
   * Output buffer given to the states to allow them to return more than one
   * output for a single input.
   */
  private LinkedList<ITracePoint2D> m_outputBuffer = new LinkedList<ITracePoint2D>();

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
  public AIteratorITracePointStateEnginge(ITrace2D originalTrace,
      IAccumulationFunction accumulationFunction,
      final IAccumulationStrategy.IAccumulationControl accumulationControl) {
    super(originalTrace, accumulationFunction, accumulationControl);
    this.m_transitionTable = this.initTransitionTable();
    this.m_currentState = this.m_startState;
  }

  /**
   * Allows to reuse this instance (by resetting to the initial state) with the
   * given trace.
   * <p>
   * This skips setting up the internal transition table (see
   * {@link #initTransitionTable()} anew and therefore bypasses unnecessary
   * computations.
   * <p>
   * 
   * @param trace
   *          the new trace this iterator will work for.
   */
  public void reset(final ITrace2D trace) {
    this.setOriginalTrace(trace);
    this.m_currentState = this.m_startState;
  }

  /**
   * Template method to initialize your transition table here. The start state
   * has to be at position 0.
   * <p>
   * You also have to {@link #setStartState(Enum)} to the first enum in your
   * STATES enum.
   * <p>
   * After this subsequent calls to {@link #next()} should be OK.
   * <p>
   * 
   * @return transitionTable the filled transition table.
   */
  protected abstract List<Transition>[] initTransitionTable();

  /**
   * @see java.util.Iterator#hasNext()
   */
  @Override
  public boolean hasNext() {

    boolean result = false;
    result = !this.m_outputBuffer.isEmpty() || this.getOriginalIterator().hasNext();
    return result;
  }

  /**
   * @see java.util.Iterator#next()
   */
  @Override
  public ITracePoint2D next() {

    ITracePoint2D result;
    /*
     * 1. Clean the output buffer.
     * 
     * A transition's action could put up to two points into that buffer.
     */
    if (!this.m_outputBuffer.isEmpty()) {
      // remove HEAD / FIRST
      result = this.m_outputBuffer.remove();
    } else {

      /*
       * 2. If output buffer was empty fill it again and return the first filled
       * point.
       */
      Iterator<ITracePoint2D> iterator = this.getOriginalIterator();
      ITracePoint2D point;

      List<Transition> potentialTransitions = this.m_transitionTable[this.m_currentState.ordinal()];
      ICondition condition;
      boolean isLastPoint = false;

      while (this.m_outputBuffer.isEmpty() && iterator.hasNext()) {
        point = iterator.next();
        isLastPoint = !iterator.hasNext();
        /*
         * Search for a transition to take:
         */
        transitionLoop: for (Transition transition : potentialTransitions) {
          condition = transition.getCondition();
          /*
           * First one wins: But by definition there cannot be two transition
           * conditions that fall true (minimized fsm).
           */
          if (condition.isMet(point, isLastPoint)) {
            transition.getAction().computeOutput(point, this.m_outputBuffer);
            this.m_currentState = transition.getStateFollowing();
            // uncommon style but explicitly naming the iteration to break;
            break transitionLoop;
          }
        }
        this.m_previousPoint = point;
        /*
         * Don't forget to update this: We might have taken a transition without any output!
         */
        potentialTransitions = this.m_transitionTable[this.m_currentState.ordinal()];

      }
      // remove HEAD / FIRST.
      result = this.m_outputBuffer.remove();
    }
    return result;
  }
}
