package com.explodingpixels.widgets.plaf;

import java.awt.Dimension;
import java.awt.Rectangle;
import java.awt.event.MouseListener;

import javax.swing.JScrollBar;

/**
 * An interface that allows implementors to control the appearance of a {@link JScrollBar}.
 */
public interface ScrollBarSkin {

    /**
     * Called once at the begining of the skin's life cycle. Implementors should add components
     * that will later be controlled in {@link #layoutTrackOnly(JScrollBar, ScrollBarOrientation)}
     * and {@link #layoutEverything(JScrollBar, ScrollBarOrientation)}.
     *
     * @param scrollBar the {@link JScrollBar} that the skin will be painting.
     */
    void installComponents(JScrollBar scrollBar);

    /**
     * Called once at the begining of the skin's life cycle. Implementors should attach these
     * mouse listners to the controls that decrement and increment the scroll bar's value.
     *
     * @param decrementMoustListener the {@link MouseListener} to be notified when a control is
     *                               pressed that should result in the scroll bar's value decrementing.
     * @param incrementMouseListener the {@link MouseListener} to be notified when a control is
     *                               pressed that should result in the scroll bar's value incrementing.
     */
    void installMouseListenersOnButtons(MouseListener decrementMoustListener,
                                        MouseListener incrementMouseListener);

    /**
     * Called when only the track should be laid out by the skin. This occurs when a
     * {@link JScrollBar} has been set to {@link javax.swing.JScrollPane#VERTICAL_SCROLLBAR_ALWAYS}
     * or {@link javax.swing.JScrollPane#HORIZONTAL_SCROLLBAR_ALWAYS} and the corresponding view is
     * showing all the content. Note that there are, in fact, no restrictions on what this method
     * lays out. That is, if this skin wishes to layout more than just an empty track when there is
     * no content to scroll, it may do so.
     *
     * @param scrollBar   the {@link JScrollBar} that the skin is painting.
     * @param orientation the orientation of the scroll bar.
     */
    void layoutTrackOnly(JScrollBar scrollBar, ScrollBarOrientation orientation);

    /**
     * Called when scroll bar should be laid out by the skin.
     *
     * @param scrollBar   the {@link JScrollBar} that the skin is painting.
     * @param orientation the orientation of the scroll bar.
     */
    void layoutEverything(JScrollBar scrollBar, ScrollBarOrientation orientation);

    /**
     * The smallest size that the scroll thumb can be.
     *
     * @return the mimimum size of the scroll thumb.
     */
    Dimension getMinimumThumbSize();

    /**
     * The preferred size of the painter, which will control the preferred size of the associated
     * {@link JScrollBar}. For vertical scroll bars, this value will drive the width. For horiztonal
     * scroll bars, this value will drive the height.
     *
     * @return the preferred size of this painter, and thus the corresponding {@code JScrollBar}.
     */
    Dimension getPreferredSize();

    /**
     * Gets the current bounds of the scroll thumb, which are controlled by the layout provided by
     * this skin.
     *
     * @return the current bounds of the scroll thumb.
     */
    Rectangle getScrollThumbBounds();

    /**
     * Sets the bounds of the scroll thumb. This method will be called, for example, when the
     * associated {@link JScrollBar}'s {@link javax.swing.BoundedRangeModel} is updated.
     *
     * @param bounds the new bounds of the scroll thumb.
     */
    void setScrollThumbBounds(Rectangle bounds);

    /**
     * Gets the current bounds of the track, which are controlled by the layout provided by
     * this skin. Note that the bounds returned by this method should be the actual scrollable
     * bounds that the scroll thumb can move in. That is, this value should not just return the
     * bounds of the associated {@link JScrollBar}, but only the bounds that are valid for the
     * scroll thumb to exist in.
     *
     * @return the current bounds of the track.
     */
    Rectangle getTrackBounds();

}
