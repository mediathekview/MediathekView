/**
 * Copyright (c) 2005-2006, Sun Microsystems, Inc
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above
 *     copyright notice, this list of conditions and the following 
 *     disclaimer in the documentation and/or other materials provided 
 *     with the distribution.
 *   * Neither the name of the TimingFramework project nor the names of its
 *     contributors may be used to endorse or promote products derived 
 *     from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */


package org.jdesktop.animation.timing;

import javax.swing.Timer;
import java.awt.event.*;
import java.util.ArrayList;
import org.jdesktop.animation.timing.interpolation.Interpolator;
import org.jdesktop.animation.timing.interpolation.LinearInterpolator;

/**
 * This class controls animations.  Its constructors and various
 * set methods control the parameters under which animations are run,
 * and the other methods support starting and stopping the animation.
 * The parameters of this class use the concepts of a "cycle" (the base
 * animation) and an "envelope" that controls how the cycle is started,
 * ended, and repeated.
 * <p>
 * Most of the methods here are simple getters/setters for the properties
 * used by Animator.  Typical animations will simply use one of the 
 * two constructors (depending on whether you are constructing a repeating
 * animation), optionally call any of the <code>set*</code> methods to alter
 * any of the other parameters, and then call start() to run the animation.
 * For example, this animation will run for 1 second, calling your
 * {@link TimingTarget} with timing events when the animation is started,
 * running, and stopped:
 * <pre>
 *  Animator animator = new Animator(1000, myTarget);
 *  animator.start();
 * </pre>
 * The following variation will run a half-second animation 4 times, 
 * reversing direction each time:
 * <pre>
 *  Animator animator = new Animator(500, 4, RepeatBehavior.REVERSE, myTarget);
 *  animator.start();
 * </pre>
 * More complex animations can be created through using the properties
 * in Animator, such as {@link Animator#setAcceleration acceleration} and {@link
 * Animator#setDeceleration}. More automated animations can be created and run
 * using the {@link org.jdesktop.animation.timing.triggers triggers}
 * package to control animations through events and {@link
 * org.jdesktop.animation.timing.interpolation.PropertySetter} to 
 * handle animating object properties.
 */
public final class Animator {

    private TimingSource timer;    // Currently uses Swing timer.  This could change
			    // in the future to use a more general mechanism
			    // (and one of better timing resolution). An 
                            // important advantage to the Swing timer is that
                            // it ensures that we receive and send our timing
                            // events on the Event Dispatch Thread, which makes
                            // it easier to use the framework for GUI
                            // animations.
    private TimingSource swingTimer;
    private TimingSourceTarget timingSourceTarget;
    
    private ArrayList<TimingTarget> targets = new ArrayList<TimingTarget>();    // Animators may have 
                                                    // multiple targets

    private long startTime;	    // Tracks original Animator start time
    private long currentStartTime;  // Tracks start time of current cycle
    private boolean intRepeatCount = true;  // for typical cases
                                            // of repeated cycles
    private boolean timeToStop = false;     // This gets triggered during
                                            // fraction calculation
    private boolean hasBegun = false;
    private long pauseBeginTime = 0;        // Used for pause/resume
    private boolean running = false;        // Used for isRunning()
    
    // Private variables to hold the internal "envelope" values that control
    // how the cycle is started, ended, and repeated.
    private double repeatCount = 1.0;
    private int startDelay;
    private RepeatBehavior repeatBehavior = RepeatBehavior.REVERSE;
    private EndBehavior endBehavior = EndBehavior.HOLD;
    
    // Private variables to hold the internal values of the base
    // animation (the cycle)
    private int duration;
    private int resolution = 20;    
    private float acceleration = 0;
    private float deceleration = 0.0f;
    private float startFraction = 0.0f;
    private Direction startDirection = Direction.FORWARD; // Direction of each cycle
    private Direction direction;
    private Interpolator interpolator = LinearInterpolator.getInstance();
    
    /**
     * EndBehavior determines what happens at the end of the animation.
     * @see #setEndBehavior
     */
    public static enum EndBehavior {
        /** Timing sequence will maintain its final value at the end */
	HOLD,
        /** Timing sequence should reset to the initial value at the end */
	RESET,
    };

    /**
     * Direction is used to set the initial direction in which the
     * animation starts.
     * 
     * @see #setStartDirection
     */
    public static enum Direction {
        /**
         * cycle proceeds forward
         */
	FORWARD,
        /** cycle proceeds backward */
	BACKWARD,
    };
    
    /**
     * RepeatBehavior determines how each successive cycle will flow.
     * @see #setRepeatBehavior
     */
    public static enum RepeatBehavior {
        /** 
         * Each repeated cycle proceeds in the same direction as the 
         * previous one 
         */
	LOOP,
        /** 
         * Each cycle proceeds in the opposite direction as the 
         * previous one
         */
	REVERSE
    };
    
    /**
     * Used to specify unending duration or repeatCount
     * @see #setDuration
     * @see #setRepeatCount
     * */
    public static final int INFINITE = -1;

    private void validateRepeatCount(double repeatCount) {
        if (repeatCount < 1 && repeatCount != INFINITE) {
            throw new IllegalArgumentException("repeatCount (" + repeatCount + 
                    ") cannot be <= 0");
        }
    }

    /**
     * Constructor: this is a utility constructor
     * for a simple timing sequence that will run for 
     * <code>duration</code> length of time.  This variant takes no
     * TimingTarget, and is equivalent to calling {@link #Animator(int, 
     * TimingTarget)} with a TimingTarget of <code>null</code>.
     * 
     * @param duration The length of time that this will run, in milliseconds.
     */

    public Animator(int duration) {
        this(duration, null);
    }
    
    /**
     * Constructor: this is a utility constructor
     * for a simple timing sequence that will run for 
     * <code>duration</code> length of time.
     * 
     * @param duration The length of time that this will run, in milliseconds.
     * @param target TimingTarget object that will be called with
     * all timing events.  Null is acceptable, but no timingEvents will be
     * sent to any targets without future calls to {@link #addTarget}.
     */
    public Animator(int duration, TimingTarget target) {
        this.duration = duration;
        addTarget(target);

    /**
	 * hack workaround for starting the Toolkit thread before any Timer stuff
	 * javax.swing.Timer uses the Event Dispatch Thread, which is not
	 * created until the Toolkit thread starts up.  Using the Swing
	 * Timer before starting this stuff starts up may get unexpected
	 * results (such as taking a long time before the first timer
	 * event).
	 */
	java.awt.Toolkit.getDefaultToolkit();	

	// Create internal Timer object
        swingTimer = new SwingTimingSource();
        timer = swingTimer;
    }
    
    /**
     * Constructor that sets the most common properties of a 
     * repeating animation.
     * @param duration the length of each animation cycle, in milliseconds.
     * This value can also be {@link #INFINITE} for animations that have no
     * end.  Note that fractions sent out with such unending animations will
     * be undefined since there is no fraction of an infinitely long cycle.
     * @param repeatCount the number of times the animation cycle will repeat.
     * This is a positive value, which allows a non-integral number
     * of repetitions (allowing an animation to stop mid-cycle, for example).
     * This value can also be {@link #INFINITE}, indicating that the animation
     * will continue repeating forever, or until manually stopped.
     * @param repeatBehavior {@link RepeatBehavior} of each successive
     * cycle.  A value of null is equivalent to RepeatBehavior.REVERSE.
     * @param target TimingTarget object that will be called with
     * all timing events.  Null is acceptable, but no timingEvents will be
     * sent to any targets without future calls to {@link #addTarget}.
     * @throws IllegalArgumentException if any parameters have invalid
     * values
     * @see Animator#INFINITE
     * @see Direction
     * @see EndBehavior
     */
    public Animator(int duration, double repeatCount, 
            RepeatBehavior repeatBehavior, TimingTarget target) {
        this(duration, target);
	// First, check for bad parameters
        validateRepeatCount(repeatCount);
        this.repeatCount = repeatCount;
        this.repeatBehavior = (repeatBehavior != null) ? 
            repeatBehavior : RepeatBehavior.REVERSE;

        // Set convenience variable: do we have an integer number of cycles?
	intRepeatCount = (Math.rint(repeatCount) == repeatCount);
    }    
    
    /**
     * Returns the initial direction for the animation.
     * @return direction that the initial animation cycle will be moving
     */
    public Direction getStartDirection() {
        return startDirection;
    }
    
    /**
     * Sets the startDirection for the initial animation cycle.  The default 
     * startDirection is {@link Direction#FORWARD FORWARD}.
     * 
     * @param startDirection initial animation cycle direction
     * @see #isRunning()
     * @throws IllegalStateException if animation is already running; this
     * parameter may only be changed prior to starting the animation or 
     * after the animation has ended
     */
    public void setStartDirection(Direction startDirection) {
        throwExceptionIfRunning();
        this.startDirection = startDirection;
    }
    
    /**
     * Returns the interpolator for the animation.
     * @return interpolator that the initial animation cycle uses
     */
    public Interpolator getInterpolator() {
        return interpolator;
    }
    
    /**
     * Sets the interpolator for the animation cycle.  The default 
     * interpolator is {@link LinearInterpolator}.
     * @param interpolator the interpolation to use each animation cycle
     * @throws IllegalStateException if animation is already running; this
     * parameter may only be changed prior to starting the animation or 
     * after the animation has ended
     * @see #isRunning()
     */
    public void setInterpolator(Interpolator interpolator) {
        throwExceptionIfRunning();
        this.interpolator = interpolator;
    }
    
    /**
     * Sets the fraction of the timing cycle that will be spent accelerating
     * at the beginning. The default acceleration value is 0 (no acceleration).
     * @param acceleration value from 0 to 1
     * @throws IllegalArgumentException acceleration value must be between 0 and
     * 1, inclusive. 
     * @throws IllegalArgumentException acceleration cannot be greater than
     * (1 - deceleration)
     * @throws IllegalStateException if animation is already running; this
     * parameter may only be changed prior to starting the animation or 
     * after the animation has ended
     * @see #isRunning()
     * @see #setDeceleration(float)
     */
    public void setAcceleration(float acceleration) {
        throwExceptionIfRunning();
        if (acceleration < 0 || acceleration > 1.0f) {
            throw new IllegalArgumentException("Acceleration value cannot lie" +
                    " outside [0,1] range");
        }
        if (acceleration > (1.0f - deceleration)) {
            throw new IllegalArgumentException("Acceleration value cannot be" +
                    " greater than (1 - deceleration)");
        }
        this.acceleration = acceleration;
    }
    
    /**
     * Sets the fraction of the timing cycle that will be spent decelerating
     * at the end. The default deceleration value is 0 (no deceleration).
     * @param deceleration value from 0 to 1
     * @throws IllegalArgumentException deceleration value must be between 0 and
     * 1, inclusive. 
     * @throws IllegalArgumentException deceleration cannot be greater than
     * (1 - acceleration)
     * @throws IllegalStateException if animation is already running; this
     * parameter may only be changed prior to starting the animation or 
     * after the animation has ended
     * @see #isRunning()
     * @see #setAcceleration(float)
     */
    public void setDeceleration(float deceleration) {
        throwExceptionIfRunning();
        if (deceleration < 0 || deceleration > 1.0f) {
            throw new IllegalArgumentException("Deceleration value cannot lie" +
                    " outside [0,1] range");
        }
        if (deceleration > (1.0f - acceleration)) {
            throw new IllegalArgumentException("Deceleration value cannot be" +
                    " greater than (1 - acceleration)");
        }
        this.deceleration = deceleration;
    }
    
    /**
     * Returns the current value of acceleration property
     * @return acceleration value
     */
    public float getAcceleration() {
        return acceleration;
    }
    
    /**
     * Returns the current value of deceleration property
     * @return deceleration value
     */
    public float getDeceleration() {
        return deceleration;
    }
    
    /**
     * Adds a TimingTarget to the list of targets that get notified of each
     * timingEvent.  This can be done at any time before, during, or after the
     * animation has started or completed; the new target will begin
     * having its TimingTarget methods called as soon as it is added.
     * If <code>target</code> is already on the list of targets in this Animator, it
     * is not added again (there will be only one instance of any given
     * target in any Animator's list of targets).
     * @param target TimingTarget to be added to the list of targets that
     * get notified by this Animator of all timing events. Target cannot
     * be null.
     */
    public void addTarget(TimingTarget target) {
        if (target != null) {
            synchronized (targets) {
                if (!targets.contains(target)) {
                    targets.add(target);
                }
            }
        }
    }
    
    /**
     * Removes the specified TimingTarget from the list of targets that get
     * notified of each timingEvent.  This can be done at any time before,
     * during, or after the animation has started or completed; the 
     * target will cease having its TimingTarget methods called as soon
     * as it is removed.
     * @param target TimingTarget to be removed from the list of targets that
     * get notified by this Animator of all timing events.
     */
    public void removeTarget(TimingTarget target) {
        synchronized (targets) {
            targets.remove(target);
        }
    }
    
    /**
     * Private utility to throw an exception if the animation is running.  This
     * is used by all of the property-setting methods to ensure that the
     * properties are not being changed mid-stream.
     */
    private void throwExceptionIfRunning() {
        if (isRunning()) {
            throw new IllegalStateException("Cannot perform this operation " +
                    "while Animator is running");
        }
    }
    
    /**
     * Returns the current resolution of the animation. This helps 
     * determine the maximum frame rate at which the animation will run.
     * @return the resolution, in milliseconds, of the timer
     */
    public int getResolution() {
	return resolution;
    }
    
    /**
     * Sets the resolution of the animation
     * @param resolution the amount of time between timing events of the
     * animation, in milliseconds.  Note that the actual resolution may vary,
     * according to the resolution of the timer used by the framework as well
     * as system load and configuration; this value should be seen more as a
     * minimum resolution than a guaranteed resolution.
     * @throws IllegalArgumentException resolution must be >= 0
     * @throws IllegalStateException if animation is already running; this
     * parameter may only be changed prior to starting the animation or 
     * after the animation has ended
     * @see #isRunning()
     */
    public void setResolution(int resolution) {
        if (resolution < 0) {
            throw new IllegalArgumentException("resolution must be >= 0");
        }
        throwExceptionIfRunning();
        this.resolution = resolution;
        timer.setResolution(resolution);
    }
    
    /**
     * Returns the duration of the animation.
     * @return the length of the animation, in milliseconds. A
     * return value of -1 indicates an {@link #INFINITE} duration.
     */
    public int getDuration() {
	return duration;
    }
    
    /**
     * Sets the duration for the animation
     * @param duration the length of the animation, in milliseconds.  This
     * value can also be {@link #INFINITE}, meaning the animation will run
     * until manually stopped.
     * @throws IllegalStateException if animation is already running; this
     * parameter may only be changed prior to starting the animation or 
     * after the animation has ended
     * @see #isRunning()
     * @see #stop()
     */
    public void setDuration(int duration) {
        throwExceptionIfRunning();
        this.duration = duration;
    }

    /**
     * Returns the number of times the animation cycle will repeat.
     * @return the number of times the animation cycle will repeat.
     */
    public double getRepeatCount() {
	return repeatCount;
    }
    
    /**
     * Sets the number of times the animation cycle will repeat. The default
     * value is 1.
     * @param repeatCount Number of times the animation cycle will repeat.
     * This value may be >= 1 or {@link #INFINITE} for animations that repeat 
     * indefinitely.  The value may be fractional if the animation should
     * stop at some fractional point.
     * @throws IllegalArgumentException if repeatCount is not >=1 or 
     * INFINITE.
     * @throws IllegalStateException if animation is already running; this
     * parameter may only be changed prior to starting the animation or 
     * after the animation has ended
     * @see #isRunning()
     */
    public void setRepeatCount(double repeatCount) {
        validateRepeatCount(repeatCount);
        throwExceptionIfRunning();
        this.repeatCount = repeatCount;
    }

    /**
     * Returns the amount of delay prior to starting the first animation
     * cycle after the call to {@link #start}.
     * @return the duration, in milliseconds, between the call
     * to start the animation and the first animation cycle actually 
     * starting.
     * @see #start
     */
    public int getStartDelay() {
	return startDelay;
    }

    /**
     * Sets the duration of the initial delay between calling {@link #start}
     * and the start of the first animation cycle. The default value is 0 (no 
     * delay).
     * @param startDelay the duration, in milliseconds, between the call
     * to start the animation and the first animation cycle actually 
     * starting. This value must be >= 0.
     * @throws IllegalArgumentException if startDelay is < 0
     * @throws IllegalStateException if animation is already running; this
     * parameter may only be changed prior to starting the animation or 
     * after the animation has ended
     * @see #isRunning()
     */
    public void setStartDelay(int startDelay) {
        if (startDelay < 0) {
            throw new IllegalArgumentException("startDelay (" + startDelay + 
                    ") cannot be < 0");
        }
        throwExceptionIfRunning();
        this.startDelay = startDelay;
        timer.setStartDelay(startDelay);
    }

    /**
     * Returns the {@link RepeatBehavior} of the animation. The default
     * behavior is REVERSE, meaning that the animation will reverse direction
     * at the end of each cycle.
     * @return whether the animation will repeat in the same
     * direction or will reverse direction each time.
     */
    public RepeatBehavior getRepeatBehavior() {
	return repeatBehavior;
    }
    
    /**
     * Sets the {@link RepeatBehavior} of the animation.
     * @param repeatBehavior the behavior for each successive cycle in the
     * animation.  A null behavior is equivalent to specifying the default:
     * REVERSE.  The default behaviors is HOLD.
     * @throws IllegalStateException if animation is already running; this
     * parameter may only be changed prior to starting the animation or 
     * after the animation has ended
     * @see #isRunning()
     */
    public void setRepeatBehavior(RepeatBehavior repeatBehavior) {
        throwExceptionIfRunning();
        this.repeatBehavior = (repeatBehavior != null) ? 
            repeatBehavior : RepeatBehavior.REVERSE;
    }

    /**
     * Returns the {@link EndBehavior} of the animation, either HOLD to 
     * retain the final value or RESET to take on the initial value. The 
     * default behavior is HOLD.
     * @return the behavior at the end of the animation
     */
    public EndBehavior getEndBehavior() {
	return endBehavior;
    }
    
    /**
     * Sets the behavior at the end of the animation.
     * @param endBehavior the behavior at the end of the animation, either
     * HOLD or RESET.  A null value is equivalent to the default value of
     * HOLD.
     * @throws IllegalStateException if animation is already running; this
     * parameter may only be changed prior to starting the animation or 
     * after the animation has ended
     * @see #isRunning
     */
    public void setEndBehavior(EndBehavior endBehavior) {
        throwExceptionIfRunning();
        this.endBehavior = endBehavior;
    }
    
    /**
     * Returns the fraction that the first cycle will start at.
     * @return fraction between 0 and 1 at which the first cycle will start.
     */
    public float getStartFraction() {
        return startFraction;
    }
    
    /**
     * Sets the initial fraction at which the first animation cycle will
     * begin.  The default value is 0.
     * @param startFraction
     * @see #isRunning()
     * @throws IllegalArgumentException if startFraction is less than 0
     * or greater than 1
     * @throws IllegalStateException if animation is already running; this
     * parameter may only be changed prior to starting the animation or 
     * after the animation has ended
     */
    public void setStartFraction(float startFraction) {
        if (startFraction < 0 || startFraction > 1.0f) {
            throw new IllegalArgumentException("initialFraction must be " +
                    "between 0 and 1");
        }
        throwExceptionIfRunning();
        this.startFraction = startFraction;
    }
    
    /**
     * Starts the animation
     * @throws IllegalStateException if animation is already running; this
     * command may only be run prior to starting the animation or 
     * after the animation has ended
     */
    public void start() {
        throwExceptionIfRunning();
        hasBegun = false;
        running = true;
        direction = startDirection;
        // Initialize start time variables to current time
        startTime = (System.nanoTime() / 1000000) + getStartDelay();
        if (duration != INFINITE &&
                ((direction == Direction.FORWARD && startFraction > 0.0f) ||
                 (direction == Direction.BACKWARD && startFraction < 1.0f))) {
            float offsetFraction = (direction == Direction.FORWARD) ?
                startFraction : (1.0f - startFraction);
            long startDelta = (long)(duration * offsetFraction);
            startTime -= startDelta;
        }
	currentStartTime = startTime;
	timer.start();
    }

    /**
     * Returns whether this Animator object is currently running
     */
    public boolean isRunning() {
	return running;
    }

    /**
     * This method is optional; animations will always stop on their own
     * if Animator is provided with appropriate values for
     * duration and repeatCount in the constructor.  But if the application 
     * wants to stop the timer mid-stream, this is the method to call.
     * This call will result in calls to the <code>end()</code> method
     * of all TimingTargets of this Animator.
     * @see #cancel()
     */
    public void stop() {
	timer.stop();
        end();
        timeToStop = false;
        running = false;
        pauseBeginTime = 0;
    }

    /**
     * This method is like the {@link #stop} method, only this one will
     * not result in a calls to the <code>end()</code> method in all 
     * TimingTargets of this Animation; it simply cancels the Animator
     * immediately.
     * @see #stop()
     */
    public void cancel() {
        timer.stop();
        timeToStop = false;
        running = false;
        pauseBeginTime = 0;
    }
    
    /**
     * This method pauses a running animation.  No further events are sent to
     * TimingTargets. A paused animation may be d again by calling the
     * {@link #resume} method.  Pausing a non-running animation has no effect.
     * 
     * @see #resume()
     * @see #isRunning()
     */
    public void pause() {
        if (isRunning()) {
            pauseBeginTime = System.nanoTime();
            running = false;
            timer.stop();
        }
    }
    
    /**
     * This method resumes a paused animation.  Resuming an animation that
     * is not paused has no effect.
     *
     * @see #pause()
     */
    public void resume() {
        if (pauseBeginTime > 0) {
            long pauseDelta = (System.nanoTime() - pauseBeginTime) / 1000000;
            startTime += pauseDelta;
            currentStartTime += pauseDelta;
            timer.start();
            pauseBeginTime = 0;
            running = true;
        }
    }
    
    //
    // TimingTarget implementations
    // Note that Animator does not actually implement TimingTarget directly;
    // it does not want to make public methods of these events.  But it uses
    // the same methods internally to propagate the events to all of the
    // Animator's targets.
    //

    /**
     * Internal timingEvent method that sends out the event to all targets
     */
    private void timingEvent(float fraction) {
        synchronized (targets) {
            for (int i = 0; i < targets.size(); ++i) {
                TimingTarget target = targets.get(i);
                target.timingEvent(fraction);
            }
        }
        if (timeToStop) {
            stop();
        }
    }
    
    /**
     * Internal begin event that sends out the event to all targets
     */
    private void begin() {
        synchronized (targets) {
            for (int i = 0; i < targets.size(); ++i) {
                TimingTarget target = targets.get(i);
                target.begin();
            }
        }
    }
    
    /**
     * Internal end event that sends out the event to all targets
     */
    private void end() {
        synchronized (targets) {
            for (int i = 0; i < targets.size(); ++i) {
                TimingTarget target = targets.get(i);
                target.end();
            }
        }
    }
    
    /**
     * Internal repeat event that sends out the event to all targets
     */
    private void repeat() {
        synchronized (targets) {
            for (int i = 0; i < targets.size(); ++i) {
                TimingTarget target = targets.get(i);
                target.repeat();
            }
        }
    }
    
    /**
     * This method calculates a new fraction value based on the
     * acceleration and deceleration settings of Animator.  It then
     * passes this value through the interpolator (by default, 
     * a LinearInterpolator) before returning it to the caller (who
     * will then call the timingEvent() methods in the TimingTargets
     * with this fraction).
     */
    private float timingEventPreprocessor(float fraction) {
        // First, take care of acceleration/deceleration factors
        if (acceleration != 0 || deceleration != 0.0f) {
            // See the SMIL 2.0 specification for details on this
            // calculation
            float runRate = 1.0f / (1.0f - acceleration/2.0f - 
                    deceleration/2.0f);
            if (fraction < acceleration) {
                float averageRunRate = runRate * (fraction / acceleration) / 2;
                fraction *= averageRunRate;
            } else if (fraction > (1.0f - deceleration)) {
                // time spent in deceleration portion
                float tdec = fraction - (1.0f - deceleration);
                // proportion of tdec to total deceleration time
                float pdec  = tdec / deceleration;
                fraction = runRate * (1.0f - ( acceleration / 2) -
                        deceleration + tdec * (2 - pdec) / 2);
            } else {
                fraction = runRate * (fraction - (acceleration / 2));
            }
            // clamp fraction to [0,1] since above calculations may
            // cause rounding errors
            if (fraction < 0) {
                fraction = 0;
            } else if (fraction > 1.0f) {
                fraction = 1.0f;
            }
        }
        // run the result through the current interpolator
        return interpolator.interpolate(fraction);
    }
    
    /**
     * Returns the total elapsed time for the current animation.
     * @param currentTime value of current time to use in calculating
     * elapsed time.
     * @return the total time elapsed between the time
     * the Animator started and the supplied currentTime.
     */
    public long getTotalElapsedTime(long currentTime) {
        return (currentTime - startTime);
    }

    /**
     * Returns the total elapsed time for the current animation.  Calculates
     * current time.
     * @return the total time elapsed between the time
     * the Animator started and the current time.
     */
    public long getTotalElapsedTime() {
        long currentTime = System.nanoTime() / 1000000;
        return getTotalElapsedTime(currentTime);
    }
    
    /**
     * Returns the elapsed time for the current animation cycle.
     * @param currentTime value of current time to use in calculating
     * elapsed time.
     * @return the time elapsed between the time
     * this cycle started and the supplied currentTime.
     */
    public long getCycleElapsedTime(long currentTime) {
        return (currentTime - currentStartTime);
    }

    /**
     * Returns the elapsed time for the current animation cycle. Calculates
     * current time.
     * @return the time elapsed between the time
     * this cycle started and the current time.
     */
    public long getCycleElapsedTime() {
        long currentTime = System.nanoTime() / 1000000;
        return getCycleElapsedTime(currentTime);
    }
    
    /**
     * This method calculates and returns the fraction elapsed of the current
     * cycle based on the current time
     * @return fraction elapsed of the current animation cycle
     */
    public float getTimingFraction() {
        long currentTime = System.nanoTime() / 1000000;
        long cycleElapsedTime = getCycleElapsedTime(currentTime);
        long totalElapsedTime = getTotalElapsedTime(currentTime);
        double currentCycle = (double)totalElapsedTime / duration;
        float fraction;

        if (!hasBegun) {
            // Call begin() first time after calling start()
            begin();
            hasBegun = true;
        }
        if ((duration != INFINITE) && (repeatCount != INFINITE) && 
                (currentCycle >= repeatCount)) {
            // Envelope done: stop based on end behavior
            switch (endBehavior) {
            case HOLD:
                // Make sure we send a final end value
                if (intRepeatCount) {
                    // If supposed to run integer number of cycles, hold
                    // on integer boundary
                    if (direction == Direction.BACKWARD) {
                        // If we were traveling backward, hold on 0
                        fraction = 0.0f;
                    } else {
                        fraction = 1.0f;
                    }
                } else {
                    // hold on final value instead
                    fraction = Math.min(1.0f, 
                        ((float)cycleElapsedTime / duration));
                }
                break;
            case RESET:
                // RESET requires setting the final value to the start value
                fraction = 0.0f;
                break;
            default:
                fraction = 0.0f;
                // should not reach here
                break;
            }
            timeToStop = true;
        } else if ((duration != INFINITE) && (cycleElapsedTime > duration)) {
            // Cycle end: Time to stop or change the behavior of the timer
            long actualCycleTime = cycleElapsedTime % duration;
            fraction = (float)actualCycleTime / duration;
            // Set new start time for this cycle
            currentStartTime = currentTime - actualCycleTime;

            if (repeatBehavior == RepeatBehavior.REVERSE) {
                boolean oddCycles = 
                        ((int)(cycleElapsedTime / duration) % 2)
                        > 0;
                if (oddCycles) {
                    // reverse the direction
                    direction = (direction == Direction.FORWARD) ? 
                            Direction.BACKWARD :
                            Direction.FORWARD;
                }
                if (direction == Direction.BACKWARD) {
                    fraction = 1.0f - fraction;
                }
            }
            repeat();
        } else {
            // mid-stream: calculate fraction of animation between
            // start and end times and send fraction to target
            fraction = 0.0f;
            if (duration != INFINITE) {
                // Only limited duration animations need a fraction
                fraction = (float)cycleElapsedTime / duration;
                if (direction == Direction.BACKWARD) {
                    // If this is a reversing cycle, want to know inverse
                    // fraction; how much from start to finish, not 
                    // finish to start
                    fraction = (1.0f - fraction);
                }
                // Clamp fraction in case timing mechanism caused out of 
                // bounds value
                fraction = Math.min(fraction, 1.0f);
                fraction = Math.max(fraction, 0.0f);
            }
        }
        return timingEventPreprocessor(fraction);
    }
    
    /**
     * Sets a new TimingSource that will supply the timing 
     * events to this Animator. Animator uses an internal
     * TimingSource by default and most developers will probably not
     * need to change this default behavior. But for those wishing to
     * supply their own timer, this method can be called to
     * tell Animator to use a different TimingSource instead. Setting a
     * new TimingSource implicitly removes this Animator as a listener
     * to any previously-set TimingSource object.
     * 
     * @param timer the object that will provide the
     * timing events to Animator. A value of <code>null</code> is
     * equivalent to telling Animator to use its default internal
     * TimingSource object.
     * @throws IllegalStateException if animation is already running; this
     * parameter may only be changed prior to starting the animation or 
     * after the animation has ended.
     */
    public synchronized void setTimer(TimingSource timer) {
        throwExceptionIfRunning();
        if (this.timer != swingTimer) {
            // Remove this Animator from any previously-set external timer
            this.timer.removeEventListener(timingSourceTarget);
        }
        if (timer == null) {
            this.timer = swingTimer;
        } else {
            this.timer = timer;
            if (timingSourceTarget == null) {
                timingSourceTarget = new TimingSourceTarget();
            }
            timer.addEventListener(timingSourceTarget);
        }
        // sync this new timer with existing timer properties
        this.timer.setResolution(resolution);
        this.timer.setStartDelay(startDelay);
    }
        
    /**
     * This package-private class will be called by TimingSource.timingEvent()
     * when a timer sends in timing events to this Animator.
     */
    class TimingSourceTarget implements TimingEventListener {
        public void timingSourceEvent(TimingSource timingSource) {
            // Make sure that we are being called by the current timer
            // and that the animation is actually running
            if ((timer == timingSource) && running) {
                timingEvent(getTimingFraction());
            }
        }
    }
    
    /**
     * Implementation of internal timer, which uses the Swing Timer class.
     * Note that we do not bother going through the TimingSource.timingEvent()
     * class with our timing events; they go through the TimerTarget
     * ActionListener implementation and then directly to timingEvent(fraction).
     */
    private class SwingTimingSource extends TimingSource {
        Timer timer; // Swing timer
        
        public SwingTimingSource() {
            timer = new Timer(resolution, new TimerTarget());
            timer.setInitialDelay(0);
        }
        
        public void start() {
            timer.start();
        }
        
        public void stop() {
            timer.stop();
        }
        
        public void setResolution(int resolution) {
            timer.setDelay(resolution);
        }
        
        public void setStartDelay(int delay) {
            timer.setInitialDelay(delay);
        }
    }
    
    /**
     * Internal implementation detail: we happen to use javax.swing.Timer
     * currently, which sends its timing events to an ActionListener.
     * This internal private class is our ActionListener that traps
     * these calls and forwards them to the Animator.timingEvent(fraction)
     * method.
     */
	private class TimerTarget implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			timingEvent(getTimingFraction());
		}
	}
}
