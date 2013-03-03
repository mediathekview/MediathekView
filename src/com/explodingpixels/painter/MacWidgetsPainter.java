package com.explodingpixels.painter;

import java.awt.Graphics2D;

/**
 * An interface that allows painting to be delegated. The implementation of this interface will be called during the
 * painting process of the given {@code objectToPaint}.
 */
public interface MacWidgetsPainter<T> {

    /**
     * Renders to the given {@link Graphics2D}. The supplied graphics context may be modified - it's
     * state need not be restored upon completion of painting.
     *
     * @param graphics the graphics context to paint into. It's state need not be restored. Will not
     *                 be null.
     * @param objectToPaint the object to be painted.
     * @param width the width within the object to paint.
     * @param height the height within the object to paint.
     */
    void paint(Graphics2D graphics, T objectToPaint, int width, int height);

}
