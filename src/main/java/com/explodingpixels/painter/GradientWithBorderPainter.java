package com.explodingpixels.painter;

import java.awt.Color;
import java.awt.Component;
import java.awt.GradientPaint;
import java.awt.Graphics2D;

public class GradientWithBorderPainter implements MacWidgetsPainter<Component> {

    private final Color fTopLineColor;

    private final Color fBottomLineColor;

    private final Color fTopGradientColor;

    private final Color fBottomGradientColor;

    public GradientWithBorderPainter(Color topLineColor, Color bottomLineColor,
                                     Color topGradientColor, Color bottomGradientColor) {
        fTopLineColor = topLineColor;
        fBottomLineColor = bottomLineColor;
        fTopGradientColor = topGradientColor;
        fBottomGradientColor = bottomGradientColor;
    }

    public void paint(Graphics2D g, Component object, int width, int height) {
        // create the paint - start the gradient one pixel from the top
        // of the component and finish the gradient one pixel from the
        // bottom.
        GradientPaint paint = new GradientPaint(
                0, 1, fTopGradientColor,
                0, height - 2, fBottomGradientColor);
        // install the paint and fill a rectangle with it.
        g.setPaint(paint);
        g.fillRect(0, 0, width, height);
        // set the graphics color and draw a line across the top of the
        // component.
        g.setColor(fTopLineColor);
        g.drawLine(0, 0, width, 0);
        // set the graphics color and draw a line across the bottom of the
        // component.
        g.setColor(fBottomLineColor);
        g.drawLine(0, height - 1, width, height - 1);
    }
}
