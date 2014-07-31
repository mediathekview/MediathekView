package info.monitorenter.gui.chart.traces.iterators;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.IAccumulationFunction;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.ITracePoint2D;
import info.monitorenter.gui.util.IteratorITracePoint2DUtil;
import info.monitorenter.gui.util.IteratorITracePoint2DUtil.SkipResult;
import info.monitorenter.util.IStopWatch;
import info.monitorenter.util.StopWatchSimple;

import java.util.Iterator;

/**
 * An implementation that assumes that x values in the underlying iterator are
 * ascending. This allows to skip leading and tailing invisible points (e.g. in
 * zoom mode).
 * <p>
 * A constructor-given amount of consecutive points will be accumulated into one
 * (vs.: density based accumulation).
 * <p>
 * Points having a {@link ITracePoint2D#getX()} value of {@link Double#NaN} will
 * not be accumulated. Those are discontinuations that have to be preserved.
 * However consecutive <code>NaN</code> x-values will be accumulated into one
 * which decreases the possibility that no points get accumulated. However in
 * the worst case no accumulations may be performed: In the case that
 * alternating <code>NaN</code> x-values and non- <code>NaN</code> x values
 * occur.
 * <p>
 * After a discontinuation ({@link ITracePoint2D#isDiscontinuation()}) has been
 * returned the next visible point must not be accumulated but returned as-is to
 * prevent showing a bigger gap than actually exists!
 * <p>
 * 
 * 
 * @author <a href="mailto:Achim.Westermann@gmx.de">Achim Westermann </a>
 * 
 */
public class AccumulatingIteratorConsecutivePointsOrderedXValues extends AAccumulationIterator {

  /**
   * Computed points to accumulate per {@link #next()}. Kept as a member to save
   * computations proportional O(n) (n are amount of source points).
   */
  private int m_countPerNext;

  /**
   * Return the first invisible point.
   */
  private ITracePoint2D m_firstInvisiblePoint = null;

  /**
   * Return the first visible point.
   */
  private ITracePoint2D m_firstVisiblePoint = null;

  /**
   * Flag to remember that in previous call to {@link #next()} a discontinuation
   * ({@link ITracePoint2D#isDiscontinuation()}) was returned. In that case the
   * next visible point has to be returned without accumulation to prevent
   * showing a bigger gap than actually exists.
   */
  private boolean m_previousNaNWasReturned;

  /**
   * Store the last available point in case we found it but first have to return
   * an accumulation result.
   */
  private ITracePoint2D m_lastPoint;

  /**
   * If true this iterator is considered empty as no points visible will follow
   * (due to ordered x values constraint).
   */
  private boolean m_hasReachedVisibleXUpperBound = false;

  /**
   * Internal needed to store if we had to return an accumulated point while
   * seeing NaN which we then have to return next time.
   */
  private ITracePoint2D m_previousNaN;

  /**
   * Constructor with all that is needed for accumulating points.
   * <p>
   * 
   * @param originalTrace
   *          the trace to decorate with the feature of accumulating points.
   * 
   * @param accumulationFunction
   *          the function to use for point - accumulation.
   * 
   * @param amountOfVisiblePoints
   *          The amount of visible points to accumulate. This will be used in
   *          case we do accumulate <code>amountOfPoints</code> consecutive
   *          points without caring for data density. In case of density - based
   *          accumulation this is used to compute the range segments to
   *          accumulate values within. However then the total amount of
   *          returned points may be smaller than this value as some segments
   *          might not contain any point.
   */
  public AccumulatingIteratorConsecutivePointsOrderedXValues(final ITrace2D originalTrace,
      final IAccumulationFunction accumulationFunction, final int amountOfVisiblePoints) {
    super(originalTrace, accumulationFunction);

    IStopWatch stopWatch = null;
    /*
     * Skip invisible points:
     */
    if (Chart2D.DEBUG_DATA_ACCUMULATION) {
      stopWatch = new StopWatchSimple();
      stopWatch.start();
    }
    SkipResult skipResult = IteratorITracePoint2DUtil.scrollToFirstVisibleXValue(this
        .getOriginalIterator());
    if (Chart2D.DEBUG_DATA_ACCUMULATION) {
      System.out.println(this.getClass().getName() + " skipped " + skipResult.getSkipCount()
          + " leading invisible points.");
    }
    
    int skipCount = skipResult.getSkipCount();
    if (skipCount == 0) {
      this.m_firstInvisiblePoint = null;
      this.m_firstVisiblePoint = skipResult.getFirstVisible();

    } else {
      this.m_firstInvisiblePoint = skipResult.getLastInvisible();
      this.m_firstVisiblePoint = skipResult.getFirstVisible();
    }

    if (Chart2D.DEBUG_DATA_ACCUMULATION) {
      stopWatch.stop();
      System.out.println(this.getClass().getName() + " took " + stopWatch.snapShot()
          + " ms to scroll to first visbible point. ");
    }

    Iterator<ITracePoint2D> backwardIterator = originalTrace.descendingIterator();

    if (Chart2D.DEBUG_DATA_ACCUMULATION) {
      stopWatch.reset();
      stopWatch.start();
    }

    SkipResult skipResultBackwards = IteratorITracePoint2DUtil
        .scrollToFirstVisibleXValue(backwardIterator);
    if (Chart2D.DEBUG_DATA_ACCUMULATION) {
      System.out.println(this.getClass().getName() + " skipped "
          + skipResultBackwards.getSkipCount() + " tailing invisible points.");
      stopWatch.stop();
      System.out.println(this.getClass().getName() + " took " + stopWatch.snapShot()
          + " ms to scroll out tailing invisible points");
    }
    int sourceCount = originalTrace.getSize() - skipCount - skipResultBackwards.getSkipCount();
    if (Chart2D.DEBUG_DATA_ACCUMULATION) {
      System.out.println(this.getClass().getName() + " this leaves " + sourceCount
          + " point to accumulate. ");
    }
    /*
     * Compute the amount of points per next() to accumulate:
     */
    int targetCount = amountOfVisiblePoints;

    if ((targetCount != 0) && (sourceCount != 0)) {
      // -2 is for first and last point
      if ((sourceCount > 2) && (targetCount > 2)) {
        this.m_countPerNext = (int) Math.ceil((sourceCount - 2.0) / (targetCount - 2.0));
      } else {
        this.m_countPerNext = 1;
      }
    }
    if (this.m_countPerNext == 0) {
      if (Chart2D.DEBUG_DATA_ACCUMULATION) {
        System.out.println("Count per next is zero! Defaulting to 1.");
      }
      this.m_countPerNext = 1;
    }

    if (Chart2D.DEBUG_DATA_ACCUMULATION) {
      System.out.println(this.getClass().getName() + " accumulating " + this.m_countPerNext
          + " point into one");
    }
  }

  /**
   * @see java.util.Iterator#hasNext()
   */
  public boolean hasNext() {
    return ((!this.m_hasReachedVisibleXUpperBound) && (this.getOriginalIterator().hasNext())
        || (this.m_lastPoint != null) || (this.m_firstInvisiblePoint != null) || (this.m_firstVisiblePoint != null));
  }

  /**
   * @see java.util.Iterator#next()
   */
  public ITracePoint2D next() {
    /*
     * Work with the accumulation function here:
     */
    ITracePoint2D result = null;
    IAccumulationFunction accumulate = this.getAccumulationFunction();
    Iterator<ITracePoint2D> iterator = this.getOriginalIterator();
    ITracePoint2D point;
    int amountToAccumulate = this.m_countPerNext;
    if (this.m_firstInvisiblePoint != null) {
      /*
       * Return the latest invisible skipped point.
       */
      result = this.m_firstInvisiblePoint;
      this.m_firstInvisiblePoint = null;
    } else if (this.m_firstVisiblePoint != null) {
      /*
       * Return the first visible point skipped to.
       */
      result = this.m_firstVisiblePoint;
      this.m_firstVisiblePoint = null;
    } else {

      if (!iterator.hasNext()) {
        /*
         * This is the case if in previous call [a] was hit. hasNext of this
         * implementation returns still true to have the stored last point be
         * returned unchanged.
         */
        result = this.m_lastPoint;
        this.m_lastPoint = null;
      } else {
        while ((amountToAccumulate > 0) && (iterator.hasNext())) {
          if (this.m_previousNaN != null) {
            /*
             * In case our previous call next returned an accumulated point but
             * kept in mind that NaN (discontinuation) was found: immediately
             * return the discontinuation.
             */
            result = this.m_previousNaN;
            this.m_previousNaN = null;
            this.m_previousNaNWasReturned = true;
            break;
          } else {

            point = iterator.next();

            // CODE IN QUESTION START
            if (point.getScaledX() > 1.0) {
              this.m_hasReachedVisibleXUpperBound = true;
              result = accumulate.getAccumulatedPoint();
              if (result == null) {
                result = point;
              }
              break;
            }
            // CODE IN QUESTION END

            if (!point.isDiscontinuation()) {
              if (this.m_previousNaNWasReturned) {
                /*
                 * Don't accumulate anything but return unaccumulated point as
                 * we do not want to increase the gap of the previous
                 * discontinuation by accumulation!
                 */
                result = point;
                // reset!
                this.m_previousNaNWasReturned = false;
                break;
              }else if (iterator.hasNext()) {
                /*
                 * [a] We must not blindly add this point to the accumulation
                 * result: The contract is that the last point has to be returned
                 * unchanged (as this implementation does not make use of the
                 * given ranges). In case this is the last point: Store it for the
                 * next invocation of next.
                 */
accumulate.addPointToAccumulate(point);
              } else {
                // [a]
                this.m_lastPoint = point;
              }
              // wipe out potential previous NaN!
              this.m_previousNaN = null;
              amountToAccumulate--;
            } else {
              // point is NaN!
              if (this.m_previousNaN != null) {
                /*
                 * Don't care: our previous point was also NaN. So skip that NaN
                 * discontinuation.
                 */
              } else {

                result = accumulate.getAccumulatedPoint();
                if (result == null) {
                  /*
                   * We're good as this NaN point was the first to accumulate:
                   * Just return it!
                   */
                  result = point;
                  this.m_previousNaNWasReturned = true;
                  break;
                } else {
                  /*
                   * Bad luck: We found a NaN but also a previous accumulation
                   * result. We have to return the previous accumulation result
                   * but keep in mind that we next have a NaN to return.
                   */
                  this.m_previousNaN = point;
                  break;
                }
              }
            }
          }
        }
      }

      /*
       * We left the loop for several reasons:
       * 
       * 1. amountToAccumulate == 0 or iterator empty: get accumulated point and
       * return it.
       * 
       * 2. NaN found with no current accumulation result: return NaN
       * 
       * 3. NaN found but with current accumulation results: Return the
       * accumulated point, store NaN result.
       */

      // case 1:
      if (result == null) {
        result = accumulate.getAccumulatedPoint();
        if (result == null) {
          /*
           * special case: just after our accumulated point was cleared/returned
           * in previous call to next we found the last point (why our
           * accumulated point is null):
           */
          if (this.m_lastPoint != null) {
            result = this.m_lastPoint;
            this.m_lastPoint = null;
          } else {
            throw new IllegalStateException("This seems a programming error or unexpected state!");
          }
        }
      } else {
        // case 2 and 3.
      }
    }
    return result;

  }



}