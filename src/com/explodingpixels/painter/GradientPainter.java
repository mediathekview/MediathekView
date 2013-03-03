package com.explodingpixels.painter;

import java.awt.Color;
import java.awt.Component;
import java.awt.GradientPaint;
import java.awt.Graphics2D;

public class GradientPainter implements MacWidgetsPainter<Component> {

    private final Color fTopColor;

    private final Color fBottomColor;

    public GradientPainter(Color topColor, Color bottomColor) {
        fTopColor = topColor;
        fBottomColor = bottomColor;
    }

    public void paint(Graphics2D g, Component object, int width, int height) {
        // create the paint - start the gradient one pixel from the top
        // of the component and finish the gradient one pixel from the
        // bottom.
        GradientPaint paint = new GradientPaint(
                0, 1, fTopColor,
                0, height, fBottomColor);
        // install the paint and fill a rectangle with it.
        g.setPaint(paint);
        g.fillRect(0, 0, width, height);
    }

}
