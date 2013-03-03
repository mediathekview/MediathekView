package com.explodingpixels.widgets.plaf;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;

import javax.swing.BoundedRangeModel;
import javax.swing.JComponent;
import javax.swing.JScrollBar;
import javax.swing.SwingUtilities;
import javax.swing.plaf.basic.BasicScrollBarUI;

import com.explodingpixels.widgets.WindowUtils;

/**
 * An implementation of {@link javax.swing.plaf.ScrollBarUI} that supports dynamic skinning.
 * painting is delegated to a {@link ScrollBarSkin}.
 */
public class SkinnableScrollBarUI extends BasicScrollBarUI {

    private ScrollBarSkin fSkin;
    private ScrollBarOrientation fOrientation;
    private final ScrollBarSkinProvider fScrollBarSkinProvider;

    /**
     * Creates a {@code SkinnableScrollBarUI} that query the given {@link ScrollBarSkinProvider} in
     * order to get the {@link ScrollBarSkin} during the installation of this UI delegate.
     *
     * @param scrollBarSkinProvider the provider of the {@code ScrollBarSkin}.
     */
    public SkinnableScrollBarUI(ScrollBarSkinProvider scrollBarSkinProvider) {
        fScrollBarSkinProvider = scrollBarSkinProvider;
    }

    @Override
    public void installUI(JComponent c) {
        JScrollBar scrollBar = (JScrollBar) c;
        // convert the Swing scroll bar orientation to the type-safe ScrollBarOrientation.
        fOrientation = ScrollBarOrientation.getOrientation(scrollBar.getOrientation());
        fSkin = fScrollBarSkinProvider.provideSkin(fOrientation);
        super.installUI(c);
    }

    @Override
    protected void uninstallComponents() {
        if (incrButton != null)
            scrollbar.remove(incrButton);
        if (decrButton != null)
            scrollbar.remove(decrButton);
    }

    @Override
    protected void installComponents() {
        // delegate to the ScrollBarSkin.
        fSkin.installComponents(scrollbar);
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        // give the ScrollBarSkin the decrement and increment MouseListeners so that it may attach
        // them to the appropriate components.
        fSkin.installMouseListenersOnButtons(new CustomArrowButtonListener(-1), new CustomArrowButtonListener(1));
        // repaint the scrollbar when the focus state of the parent window changes.
        WindowUtils.installJComponentRepainterOnWindowFocusChanged(scrollbar);
    }

    @Override
    public void layoutContainer(Container scrollbarContainer) {
        if (isDragging) {
            // do nothing.
        } else if (isAllContentVisible(scrollbar)) {
            // if all the content is visible, and thus no scrollbar is necssary, tell the
            // ScrollBarSkin to layout only the track.
            fSkin.layoutTrackOnly(scrollbar, fOrientation);
            updateThumbBoundsFromScrollBarValue();
        } else {
            // tell the ScrollBarSkin to layout the entire scrollbar. once that's complete, update
            // the bounds of the visible scroll thumb from the models value.
            fSkin.layoutEverything(scrollbar, fOrientation);
            updateThumbBoundsFromScrollBarValue();
        }
    }

    @Override
    protected Dimension getMinimumThumbSize() {
        // delegate to the ScrollBarSkin.
        return fSkin.getMinimumThumbSize();
    }

    @Override
    public Dimension getPreferredSize(JComponent c) {
        // delegate to the ScrollBarSkin.
        return fSkin.getPreferredSize();
    }

    @Override
    protected Rectangle getThumbBounds() {
        // delegate to the ScrollBarSkin.
        return fSkin.getScrollThumbBounds();
    }

    /**
     * Convienence method that simply breaks apart the given Rectangle into its primatives and
     * calls {@link #setThumbBounds(int, int, int, int)}.
     */
    private void setThumbBounds(Rectangle thumbBounds) {
        fSkin.setScrollThumbBounds(thumbBounds);
    }

    @Override
    protected void setThumbBounds(int x, int y, int width, int height) {
        setThumbBounds(new Rectangle(x, y, width, height));
    }

    @Override
    protected Rectangle getTrackBounds() {
        // delegate to the ScrollBarSkin.
        return fSkin.getTrackBounds();
    }

    @Override
    protected void paintIncreaseHighlight(Graphics g) {
        // do nothing - not supported.
    }

    @Override
    protected void paintDecreaseHighlight(Graphics g) {
        // do nothing - not supported.
    }

    /**
     * Sets the scroll thumb bounds based on the track size, the total size of viewable area and the
     * amount of content that is currently visible.
     */
    private void updateThumbBoundsFromScrollBarValue() {
        // most of the below logic was lifted from BasicScrollBarUI. the logic here has been
        // greatly simplified here through the use of the ScrollBarOrientation.

        float min = scrollbar.getMinimum();
        float extent = scrollbar.getVisibleAmount();
        float range = scrollbar.getMaximum() - min;
        float value = scrollbar.getValue();

        int trackSize = fOrientation.getLength(fSkin.getTrackBounds().getSize());
        int thumbLength = (int) (trackSize * (extent / range));

        int minimumThumbLength = fOrientation.getLength(getMinimumThumbSize());
        thumbLength = Math.max(thumbLength, minimumThumbLength);

        float thumbRange = trackSize - thumbLength;
        int thumbPosition = (int) (0.5f + (thumbRange * ((value - min) / (range - extent))));

        // tell the ScrollBarSkin how big the scroll thumb should be.
        fSkin.setScrollThumbBounds(
                fOrientation.createBounds(scrollbar, thumbPosition, thumbLength));
    }

    /**
     * Figures out where the scroll thumb should be based on the given MouseEvent and moves the
     * thumb to that new location.
     */
    private void updateThumbBoundsAndScrollBarValueFromMouseEvent(MouseEvent event, int offset) {
        int mouseLocation = adjustMousePosition(event.getPoint(), offset);
        updateThumbBoundsFromMouseLocation(mouseLocation);
        updateScrollBarValueFromMouseLocation(mouseLocation);
    }

    /**
     * Moves the visible scroll thumb to the given location.
     */
    private void updateThumbBoundsFromMouseLocation(int mouseLocation) {
        Dimension thumbSize = getThumbBounds().getSize();
        Dimension trackSize = getTrackBounds().getSize();

        // The mouseLocation is relative to the scrollbar but we need it
        // relative to the track.
        mouseLocation -= fOrientation.getPosition(getTrackBounds().getLocation());

        // set the visible thumb bounds. this smoothly tracks where the user has the mouse.
        // when they release the mouse, the actual scroll thumb position will be updated to
        // reflect the exact scroll view window.
        int thumbMaxPossiblePosition =
                fOrientation.getLength(trackSize) - fOrientation.getLength(thumbSize);
        int thumbPosition = Math.min(thumbMaxPossiblePosition, Math.max(0, mouseLocation));
        // update the scroll thumb's position.
        setThumbBounds(fOrientation.updateBoundsPosition(getThumbBounds(), thumbPosition));
    }

    /**
     * Updaates the scrollbar model based on the given mouse location.
     */
    private void updateScrollBarValueFromMouseLocation(int mouseLocation) {
        // most of the below logic was lifted from BasicScrollBarUI. the logic here has been
        // greatly simplified here through the use of the ScrollBarOrientation.

        BoundedRangeModel model = scrollbar.getModel();
        Rectangle thumbBounds = getThumbBounds();
        Rectangle trackBounds = getTrackBounds();

        // calculate what the value of the scrollbar should be.
        int minimumPossibleThumbPosition = fOrientation.getPosition(trackBounds.getLocation());
        int maximumPossibleThumbPosition = getMaximumPossibleThumbPosition(trackBounds, thumbBounds);
        int actualThumbPosition = Math.min(maximumPossibleThumbPosition,
                Math.max(minimumPossibleThumbPosition, mouseLocation));

        // calculate the new value for the scroll bar (the top of the scroll thumb) based
        // on the dragged location.
        float valueMax = model.getMaximum() - model.getExtent();
        float valueRange = valueMax - model.getMinimum();
        float thumbValue = actualThumbPosition - minimumPossibleThumbPosition;
        float thumbRange = maximumPossibleThumbPosition - minimumPossibleThumbPosition;
        int value = (int) Math.ceil((thumbValue / thumbRange) * valueRange);

        scrollbar.setValue(value + model.getMinimum());
    }

    /**
     * Gets the maximum possible thumb position.
     */
    private int getMaximumPossibleThumbPosition(Rectangle trackBounds, Rectangle thumbBounds) {
        int trackStartPosition = fOrientation.getPosition(trackBounds.getLocation());
        int trackLength = fOrientation.getLength(trackBounds.getSize());
        int thumbLength = fOrientation.getLength(thumbBounds.getSize());
        return trackStartPosition + trackLength - thumbLength;
    }

    private int adjustMousePosition(Point mousePoint, int offset) {
        return fOrientation.getPosition(mousePoint) - offset;
    }

    /**
     * True if the given point is before the start of the scroll thumb.
     */
    private boolean isPointBeforeScrollThumb(Point point) {
        int mousePosition = fOrientation.getPosition(point);
        int thumbPosition = fOrientation.getPosition(getThumbBounds().getLocation());
        return mousePosition < thumbPosition;
    }

    /**
     * True if the given point is after the end of the scroll thumb.
     */
    private boolean isPointAfterScrollThumb(Point point) {
        int mousePosition = fOrientation.getPosition(point);
        int thumbPosition = fOrientation.getPosition(getThumbBounds().getLocation())
                + fOrientation.getLength(getThumbBounds().getSize());
        return mousePosition > thumbPosition;
    }

    private int getDirectionToMoveThumb(Point mousePoint) {
        return isPointBeforeScrollThumb(mousePoint) ? -1 : 1;
    }

    @Override
    protected TrackListener createTrackListener() {
        return new SkinnableTrackListener();
    }

    /**
     * True if the all the content that the scrollbar is scrolling for is currently visible.
     */
    private static boolean isAllContentVisible(JScrollBar scrollBar) {
        float extent = scrollBar.getVisibleAmount();
        float range = scrollBar.getMaximum() - scrollBar.getMinimum();
        return extent == 0.0 || extent / range == 1.0;
    }

    // SkinnableTrackListener implementation. ////////////////////////////////////////////////////

    private class SkinnableTrackListener extends TrackListener {

        private Point iMousePoint = new Point();

        @Override
        public void mousePressed(MouseEvent event) {
            if (shouldHandleMousePressed(event)) {
                doMousePressed(event);
            }
        }

        @Override
        public void mouseDragged(MouseEvent event) {
            if (shouldHandleMouseDragged(event)) {
                doMouseDragged(event);
            }
        }

        private void startScrollTimerIfNecessary() {
            if (isPointBeforeScrollThumb(iMousePoint) || isPointAfterScrollThumb(iMousePoint)) {
                scrollTimer.start();
            }
        }

        private void captureCurrentMousePosition(MouseEvent event) {
            assert event.getSource() == scrollbar
                    : "The listener should be registered with the scrollbar for mouse events.";
            currentMouseX = event.getX();
            currentMouseY = event.getY();
            // The mouse position is relative to the scrollbar but we need it
            // relative to the track.
            int trackPos = fOrientation.getPosition(getTrackBounds().getLocation());
            if (fOrientation == ScrollBarOrientation.HORIZONTAL) {
                currentMouseX -= trackPos;
            } else {
                currentMouseY -= trackPos;
            }
            iMousePoint.x = currentMouseX;
            iMousePoint.y = currentMouseY;
        }

        private boolean isIgnorableMiddleMousePress(MouseEvent event) {
            return SwingUtilities.isMiddleMouseButton(event) && !getSupportsAbsolutePositioning();
        }

        private boolean shouldHandleMousePressed(MouseEvent event) {
            return !isIgnorableMiddleMousePress(event) && !SwingUtilities.isRightMouseButton(event)
                    && scrollbar.isEnabled();
        }

        private boolean shouldHandleMouseDragged(MouseEvent event) {
            return !isIgnorableMiddleMousePress(event) && !SwingUtilities.isRightMouseButton(event)
                    && scrollbar.isEnabled() && !getThumbBounds().isEmpty();
        }

        private void doMousePressed(MouseEvent event) {
            scrollbar.setValueIsAdjusting(true);
            captureCurrentMousePosition(event);

            if (getThumbBounds().contains(iMousePoint)) {
                doMousePressedOnThumb();
            } else if (getSupportsAbsolutePositioning() && SwingUtilities.isMiddleMouseButton(event)) {
                doMiddleMouseButtonPressedOnTrack(event);
            } else if (getTrackBounds().contains(iMousePoint)) {
                doMousePressedOnTrack();
            }
        }

        private void doMousePressedOnThumb() {
            offset = fOrientation.getPosition(iMousePoint)
                    - fOrientation.getPosition(getThumbBounds().getLocation());
            isDragging = true;
        }

        private void doMiddleMouseButtonPressedOnTrack(MouseEvent event) {
            offset = fOrientation.getLength(getThumbBounds().getSize()) / 2;
            isDragging = true;
            updateThumbBoundsAndScrollBarValueFromMouseEvent(event, offset);
        }

        private void doMousePressedOnTrack() {
            isDragging = false;
            int direction = getDirectionToMoveThumb(iMousePoint);
            scrollByBlock(direction);
            scrollTimer.stop();
            scrollListener.setDirection(direction);
            scrollListener.setScrollByBlock(true);
            startScrollTimerIfNecessary();
        }

        private void doMouseDragged(MouseEvent event) {
            if (isDragging) {
                updateThumbBoundsAndScrollBarValueFromMouseEvent(event, offset);
            } else {
                captureCurrentMousePosition(event);
                startScrollTimerIfNecessary();
            }
        }
    }

    // IncrementButtonListener implementation. ////////////////////////////////////////////////////

    protected class CustomArrowButtonListener extends ArrowButtonListener {

        private final int iScrollDirection;

        private CustomArrowButtonListener(int scrollDirection) {
            iScrollDirection = scrollDirection;
        }

        public void mousePressed(MouseEvent e) {
            if (scrollbar.isEnabled() && SwingUtilities.isLeftMouseButton(e)) {
                scrollByUnit(iScrollDirection);
                scrollTimer.stop();
                scrollListener.setDirection(iScrollDirection);
                scrollListener.setScrollByBlock(false);
                scrollTimer.start();
            }
        }

        public void mouseReleased(MouseEvent e) {
            scrollTimer.stop();
            scrollbar.setValueIsAdjusting(false);
        }
    }

    // An interface for providing a ScrollBarSkin. ////////////////////////////////////////////////

    public interface ScrollBarSkinProvider {
        ScrollBarSkin provideSkin(ScrollBarOrientation orientation);
    }

}
