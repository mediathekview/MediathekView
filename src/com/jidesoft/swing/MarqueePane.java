/*
 * @(#)MarqueePane.java 6/9/2009
 *
 * Copyright 2002 - 2009 JIDE Software Inc. All rights reserved.
 *
 */

package com.jidesoft.swing;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * <code>MarqueePane</code> is a subclass of <code>JScrollPane</code> with automation of scrolling. In
 * <code>MarqueePane</code>, you can define the direction you want the component inside the <code>MarqueePane</code> to
 * scroll to and the scrolling speed.
 */
public class MarqueePane extends JScrollPane {
    private int _scrollDelay = 100;
    private int _stayDelay = 2000;
    private int _scrollAmount = 2;
    private int _scrollDirection = SCROLL_DIRECTION_LEFT;
    private int _stayPosition = -1;

    private Timer _scrollTimer = null;
    private int _scrollTimes;
    private boolean _reachStayPosition = false;

    public static final int SCROLL_DIRECTION_LEFT = 0;
    public static final int SCROLL_DIRECTION_RIGHT = 1;
    public static final int SCROLL_DIRECTION_UP = 2;
    public static final int SCROLL_DIRECTION_DOWN = 3;

    public MarqueePane(Component view) {
        super(view);
        startAutoScrolling();
    }

    public MarqueePane() {
        super();
        startAutoScrolling();
    }

    @Override
    public void updateUI() {
        super.updateUI();
        setViewportBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
        setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
        setVerticalScrollBarPolicy(VERTICAL_SCROLLBAR_NEVER);
        setHorizontalScrollBarPolicy(HORIZONTAL_SCROLLBAR_NEVER);
        setWheelScrollingEnabled(false);
    }

    /**
     * Get the scroll frequency which indicating how frequent the Marquee will get repaint.
     * <p/>
     * The default value is 100ms.
     *
     * @return the scroll frequency.
     */
    public int getScrollDelay() {
        return _scrollDelay;
    }

    /**
     * Set the scroll frequency which indicating how frequent the Marquee will get repaint.
     *
     * @param scrollDelay the scroll frequency
     */
    public void setScrollDelay(int scrollDelay) {
        _scrollDelay = scrollDelay;
    }

    /**
     * Get the scroll amount between two repaints.
     * <p/>
     * The default value is 2.
     *
     * @return the step size.
     */
    public int getScrollAmount() {
        return _scrollAmount;
    }

    /**
     * Set the scroll amount between two repaints.
     *
     * @param scrollAmount the step size
     */
    public void setScrollAmount(int scrollAmount) {
        _scrollAmount = scrollAmount;
    }

    /**
     * Get the scroll direction.
     * <p/>
     * The value could be <code>SCROLL_LEFT</code>, <code>SCROLL_RIGHT</code>, <code>SCROLL_UP</code>,
     * <code>SCROLL_DOWN</code>
     * <p/>
     * The default value is <code>SCROLL_LEFT</code>.
     *
     * @return the scroll direction.
     */
    public int getScrollDirection() {
        return _scrollDirection;
    }

    /**
     * set the scroll direction.
     *
     * @param scrollDirection the scroll direction
     */
    public void setScrollDirection(int scrollDirection) {
        _scrollDirection = scrollDirection;
    }

    /**
     * Gets delay time when it reaches a stay position.
     * <p/>
     * The default value is 500ms.
     *
     * @return the delay time when it reaches a stay position..
     */
    public int getStayDelay() {
        return _stayDelay;
    }

    /**
     * Sets stay delay time when it reaches a stay position.
     *
     * @param stayDelay the delay time when it reaches a stay position.
     */
    public void setStayDelay(int stayDelay) {
        _stayDelay = stayDelay;
    }

    /**
     * Stop auto scrolling. The view will stay where it is.
     */
    public void stopAutoScrolling() {
        if (_scrollTimer != null) {
            if (_scrollTimer.isRunning()) {
                _scrollTimer.stop();
            }
            _scrollTimer = null;
        }
    }

    /**
     * Start auto scrolling.
     */
    public void startAutoScrolling() {
        stopAutoScrolling();
        _scrollTimer = new Timer(getScrollDelay(), new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                BoundedRangeModel rangeModel;
                if (getScrollDirection() == SCROLL_DIRECTION_LEFT || getScrollDirection() == SCROLL_DIRECTION_RIGHT) {
                    rangeModel = getHorizontalScrollBar().getModel();
                }
                else {
                    rangeModel = getVerticalScrollBar().getModel();
                }
                int value = rangeModel.getValue();

                if (getScrollDirection() == SCROLL_DIRECTION_LEFT || getScrollDirection() == SCROLL_DIRECTION_UP) {
                    if (value + rangeModel.getExtent() == rangeModel.getMaximum()) {
                        rangeModel.setValue(rangeModel.getMinimum());
                        _scrollTimes = 0;
                    }
                    else if (value + getScrollAmount() + rangeModel.getExtent() > rangeModel.getMaximum()) {
                        rangeModel.setValue(rangeModel.getMaximum() - rangeModel.getExtent());
                    }
                    else {
                        rangeModel.setValue(value + getScrollAmount());
                        _scrollTimes ++;
                    }
                    _reachStayPosition = rangeModel.getValue() == rangeModel.getMinimum();
                }
                else {
                    if (value == rangeModel.getMinimum()) {
                        int maximum = rangeModel.getMaximum();
                        int extent = rangeModel.getExtent();
                        rangeModel.setValue(rangeModel.getMaximum() - rangeModel.getExtent());
                        // In the very beginning, range model need to be reset so we need adjust the value with it.
                        if (maximum != rangeModel.getMaximum() || extent != rangeModel.getExtent()) {
                            rangeModel.setValue(rangeModel.getMaximum() - rangeModel.getExtent());
                        }
                        _scrollTimes = 0;
                    }
                    else if (value - getScrollAmount() < rangeModel.getMinimum()) {
                        rangeModel.setValue(rangeModel.getMinimum());
                    }
                    else {
                        rangeModel.setValue(value - getScrollAmount());
                        _scrollTimes ++;
                    }
                    _reachStayPosition = rangeModel.getValue() == rangeModel.getMinimum() || rangeModel.getValue() == rangeModel.getMaximum();
                }
                if (!_reachStayPosition && getStayPosition() >= 0) {
                    if (_scrollTimes % getStayPosition() == 0) {
                        _reachStayPosition = true;
                    }
                }
                if (_scrollTimer != null) {
                    _scrollTimer.setDelay(_reachStayPosition ? getStayDelay() : getScrollDelay());
                }
            }
        });
        _scrollTimer.start();
    }

    /**
     * Get stay position.
     * <p/>
     * With this field, you can let the scrolling stop for <code>getStayTime</code> after scrolling <code>getStayPosition</code> times.
     * <p/>
     * The default value is -1, which means no stay position in the middle.
     *
     * @return the stay position.
     */
    public int getStayPosition() {
        return _stayPosition;
    }

    /**
     * Set stay position.
     *
     * @param stayPosition the stay position
     */
    public void setStayPosition(int stayPosition) {
        _stayPosition = stayPosition;
    }
}
