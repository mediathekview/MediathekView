package com.explodingpixels.widgets.plaf;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;

import javax.swing.SwingConstants;

/**
 * An orientation corresponding to a {@link javax.swing.JScrollBar}. The methods in this enumeration
 * allow for orentation-agnostic calculations, which can be used when laying out a scroll bar. A
 * scroll bar, regargless of it's orientation, has a length and thickness. These values correspond
 * to different dimensions (x or y) depending on the orientation.
 */
public enum ScrollBarOrientation {

    HORIZONTAL {

        int getThickness(Dimension size) {
            return size.height;
        }

        int getLength(Dimension size) {
            return size.width;
        }

        int getPosition(Point point) {
            return point.x;
        }

        Rectangle updateBoundsPosition(Rectangle bounds, int newPosition) {
            bounds.setLocation(newPosition, bounds.y);
            return bounds;
        }

        Rectangle createBounds(Component container, int position, int length) {
            return new Rectangle(position, 0, length, container.getHeight());
        }

        Rectangle createCenteredBounds(Component container, int position, int thickness, int length) {
            int y = container.getHeight() / 2 - thickness / 2;
            return new Rectangle(position, y, length, thickness);
        }
    },

    VERTICAL {

        int getThickness(Dimension size) {
            return size.width;
        }

        int getLength(Dimension size) {
            return size.height;
        }

        int getPosition(Point point) {
            return point.y;
        }

        Rectangle updateBoundsPosition(Rectangle bounds, int newPosition) {
            bounds.setLocation(bounds.x, newPosition);
            return bounds;
        }

        Rectangle createBounds(Component container, int position, int length) {
            return new Rectangle(0, position, container.getWidth(), length);
        }

        Rectangle createCenteredBounds(Component container, int position, int thickness, int length) {
            int x = container.getWidth() / 2 - thickness / 2;
            return new Rectangle(x, position, thickness, length);
        }
    };

    /**
     * Converts a Swing scroll bar orientation (either {@link SwingConstants#HORIZONTAL} or
     * {@link SwingConstants#VERTICAL} to a {@code ScrollBarOrientation}.
     *
     * @param swingScrollBarOrientation the Swing scroll bar orientation, either
     *                                  {@link SwingConstants#HORIZONTAL} or {@link SwingConstants#VERTICAL}
     * @return the {@code ScrollBarOrientation} to the corresponding Swing scroll bar orientation.
     * @throws IllegalArgumentException if the given Swing scroll bar orientation is not valid.
     */
    public static ScrollBarOrientation getOrientation(int swingScrollBarOrientation) {
        if (swingScrollBarOrientation != SwingConstants.HORIZONTAL
                && swingScrollBarOrientation != SwingConstants.VERTICAL) {
            throw new IllegalArgumentException("The given value is not a valid scroll bar orientation.");
        }
        return swingScrollBarOrientation == SwingConstants.HORIZONTAL ? HORIZONTAL : VERTICAL;
    }

    /**
     * Get's the thickness of the given size. Thickness corresponds to the dimension that does not
     * vary in size. That is, a horizontal scroll bar's thickness corresponds to the y dimension,
     * while a vertical scroll bar's thickness corresponds to the x dimension.
     *
     * @param size the 2-dimensional size to extract the thickness from.
     * @return the thickness of the given size.
     */
    abstract int getThickness(Dimension size);

    /**
     * Get's the length of the given size. Length corresponds to the dimension that varies in size.
     * That is, a horizontal scroll bar's length corresponds to the x dimension, while a vertical
     * scroll bar's length corresponds to the y dimension.
     *
     * @param size the 2-dimensional size to extract the length from.
     * @return the length of the given size.
     */
    abstract int getLength(Dimension size);

    /**
     * Get's the position from the given {@link Point}. Position refers to the dimension of a point
     * on which the scroll bar scrolls. That is, a horiztonal scroll bar's position corresponds to
     * the x dimension, while a vertical scroll bar's position corresponds to the y dimension.
     *
     * @param point the {@code Point} from which to extrac the position from.
     * @return the position value of the given {@code Point}.
     */
    abstract int getPosition(Point point);

    /**
     * Moves the given bounds to the given position. For a horiztonal scroll bar this translates
     * into {@code bounds.x = newPosition}, while for a vertical scroll bar this translates into
     * {@code bounds.y = newPosition}.
     *
     * @param bounds      the bounds to update with the new position.
     * @param newPosition the new position to set the bounds to.
     * @return the updated bounds.
     */
    abstract Rectangle updateBoundsPosition(Rectangle bounds, int newPosition);

    /**
     * <p>
     * Creates bounds based on the given {@link Component}, position and length. The supplied
     * component will be used to determine the thickness of the bounds. The position will be used
     * to locate the bounds along the scrollable axis. The length will be used to determine the
     * length of the bounds along the scrollable axis.
     * </p><p>
     * Horizontal scroll bars, the bounds will be derived like this:
     * <pre>new Rectangle(position, 0, length, container.getHeigth())</pre>
     * </p><p>
     * Vertical scroll bar bounds will be derived like this:
     * <pre>new Rectangle(0, container.getWidth(), position, length)</pre>
     * </p>
     *
     * @param container the {@code Component} to use to determine the thickness of the bounds.
     * @param position  the position of the bounds.
     * @param length    the length of the bounds.
     * @return the created bounds.
     */
    abstract Rectangle createBounds(Component container, int position, int length);

    /**
     * <p>
     * Creates bounds centered in the given {@link Component} located at the given position, with
     * the given thickness and length.
     * </p><p>
     * Horizontal scroll bars, the bounds will be derived like this:
     * <pre>new Rectangle(position, container.getHeight()/2 - thickness/2, length, thickness)</pre>
     * </p><p>
     * Vertical scroll bars, the bounds will be derived like this:
     * <pre>new Rectangle(container.getWidth()/2 - thickness/2, position, thickness, length)</pre>
     *
     * @param container the {@code Component} to use to determine the thickness of the bounds.
     * @param position  the position of the bounds.
     * @param thickness the thickness of the given bounds.
     * @param length    the length of the bounds.
     * @return the created bounds.
     */
    abstract Rectangle createCenteredBounds(Component container, int position, int thickness, int length);

}
