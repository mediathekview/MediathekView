package com.explodingpixels.painter;

import java.awt.Graphics2D;

/**
 * An implementation of {@link MacWidgetsPainter} that calls a series of {@code Painter}s in succession. {@code ComponentPainter}
 * does not do any painting itself.
 */
public class CompoundPainter<T> implements MacWidgetsPainter<T> {

    private MacWidgetsPainter<T>[] fPainters;

    /**
     * Creates a {@link MacWidgetsPainter} that calls the given {@code Painter}s in the order they are supplied when
     * {@link #paint(java.awt.Graphics2D, Object, int, int)} is called.
     * @param painters the {@code Painter}s to delegate to.
     */
    public CompoundPainter(MacWidgetsPainter<T>... painters) {
        fPainters = painters;
    }

    public void paint(Graphics2D graphics, T objectToPaint, int width, int height) {
        for (MacWidgetsPainter<T> painter : fPainters) {
            Graphics2D graphicsCopy = (Graphics2D) graphics.create();
            painter.paint(graphicsCopy, objectToPaint, width, height);
            graphicsCopy.dispose();
        }
    }
}
