package com.explodingpixels.macwidgets.plaf;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.RoundRectangle2D;

import javax.swing.JComponent;
import javax.swing.JSlider;
import javax.swing.plaf.basic.BasicSliderUI;

/**
 * Creates a Heads Up Display (HUD) style slider, similar to that seen in various iApps
 * (e.g. iPhoto).
 * <br>
 * <img src="../../../../../graphics/HUDSliderUI-round.png">
 * <br/>
 * <img src="../../../../../graphics/HUDSliderUI-pointy.png">
 */
public class HudSliderUI extends BasicSliderUI {

    private static final int SLIDER_KNOB_WIDTH = 11;
    private static final int SLIDER_KNOB_HEIGHT_NO_TICKS = 11;
    private static final int SLIDER_KNOB_HEIGHT_WITH_TICKS = 13;
    private static final int TRACK_HEIGHT = 4;

    private static final Color TRACK_BACKGROUND_COLOR = new Color(143, 147, 144, 100);
    private static final Color TRACK_BORDER_COLOR = new Color(255, 255, 255, 200);

    private static final Color TOP_SLIDER_KNOB_COLOR = new Color(0x555555);
    private static final Color BOTTOM_SLIDER_KNOB_COLOR = new Color(0x393939);
    private static final Color TOP_SLIDER_KNOB_PRESSED_COLOR = new Color(0xb0b2b6);
    private static final Color BOTTOM_SLIDER_KNOB_PRESSED_COLOR = new Color(0x86888b);

    private static final HudPaintingUtils.ShapeProvider NO_TICKS_SHAPE_PROVIDER =
            createCircularSliderKnobShapeProvider();

    private static final HudPaintingUtils.ShapeProvider TICKS_SHAPE_PROVIDER =
            createPointedSliderKnobShapeProvider();

    public HudSliderUI(JSlider b) {
        super(b);
    }

    @Override
    protected void installDefaults(JSlider slider) {
        super.installDefaults(slider);
        slider.setOpaque(false);
    }

    @Override
    protected Dimension getThumbSize() {
        int sliderKnobHeight = slider.getPaintTicks()
                ? SLIDER_KNOB_HEIGHT_WITH_TICKS : SLIDER_KNOB_HEIGHT_NO_TICKS;
        return new Dimension(SLIDER_KNOB_WIDTH, sliderKnobHeight);
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        HudPaintingUtils.updateGraphicsToPaintDisabledControlIfNecessary((Graphics2D) g, c);
        super.paint(g, c);
    }

    @Override
    public void paintThumb(Graphics graphics) {
        Paint paint = createSliderKnobButtonPaint(isDragging(), thumbRect.height);
        HudPaintingUtils.ShapeProvider shapeProvider = slider.getPaintTicks()
                ? TICKS_SHAPE_PROVIDER : NO_TICKS_SHAPE_PROVIDER;
        HudPaintingUtils.paintHudControlBackground((Graphics2D) graphics, thumbRect, shapeProvider,
                paint);
    }

    @Override
    public void paintTrack(Graphics graphics) {
        Graphics2D graphics2d = (Graphics2D) graphics;
        graphics2d.setRenderingHint(
                RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        double trackY = slider.getHeight() / 2.0 - TRACK_HEIGHT / 2.0;
        RoundRectangle2D track = new RoundRectangle2D.Double(
                0, trackY, slider.getWidth() - 1, TRACK_HEIGHT - 1, 4, 2);

        graphics.setColor(TRACK_BACKGROUND_COLOR);
        graphics2d.fill(track);
        graphics2d.setColor(TRACK_BORDER_COLOR);
        graphics2d.draw(track);
    }

    @Override
    protected int getTickLength() {
        return 5;
    }

    @Override
    protected void calculateThumbLocation() {
        super.calculateThumbLocation();

        if (slider.getOrientation() == JSlider.HORIZONTAL
                && slider.getPaintTicks()) {
            // shift the thumb down three pixels if we're drawing the pointy style thumb.
            thumbRect.y += 3;
        } else {
            // TODO handle vertical slider.
        }
    }

    @Override
    protected void calculateTickRect() {
        super.calculateTickRect();

        if (slider.getOrientation() == JSlider.HORIZONTAL) {
            // shift the ticks down by two pixels so they aren't right up agains the track.
            tickRect.y += 1;
        } else {
            // TODO handle vertical slider.
        }
    }

    @Override
    protected void paintMajorTickForHorizSlider(Graphics g, Rectangle tickBounds, int x) {
        g.setColor(Color.WHITE);
        super.paintMajorTickForHorizSlider(g, tickBounds, x);
    }

    @Override
    public void setThumbLocation(int x, int y) {
        super.setThumbLocation(x, y);
        // repaint the whole slider -- it's easier than trying to figure out whats dirty, especially
        // since the thumb will be drawn outside of the thumbRect (the shadow part).
        slider.repaint();
    }

    @Override
    public void paintFocus(Graphics g) {
        // don't paint focus.
    }

    private static Paint createSliderKnobButtonPaint(boolean isPressed, int height) {
        Color topColor = isPressed ? TOP_SLIDER_KNOB_PRESSED_COLOR : TOP_SLIDER_KNOB_COLOR;
        Color bottomColor = isPressed ? BOTTOM_SLIDER_KNOB_PRESSED_COLOR : BOTTOM_SLIDER_KNOB_COLOR;
        // compenstate for the two pixel shadow drawn below the slider thumb.
        int bottomY = height - 2;
        return new GradientPaint(0, 0, topColor, 0, bottomY, bottomColor);
    }

    /**
     * Creates a simple circle.
     */
    private static HudPaintingUtils.ShapeProvider createCircularSliderKnobShapeProvider() {
        return new HudPaintingUtils.ShapeProvider() {
            public Shape createShape(double x, double y, double width, double height) {
                return new Ellipse2D.Double(x, y, width, height);
            }
        };
    }

    /**
     * Cerates a pointy slider thumb:
     * <pre>
     *     +----+
     *    /      \
     *    +      +
     *    |      |
     *    +      +
     *      \  /
     *       \/
     * </pre>
     */
    private static HudPaintingUtils.ShapeProvider createPointedSliderKnobShapeProvider() {
        return new HudPaintingUtils.ShapeProvider() {
            public Shape createShape(double x, double y, double width, double height) {
                float xFloat = (float) x;
                float yFloat = (float) y;
                float widthFloat = (float) width;
                float heightFloat = (float) height;

                // draw the thumb shape based on the given height and width.
                GeneralPath path = new GeneralPath();
                // move in two pixels so that we can curve down to the next point.
                path.moveTo(xFloat + 2.0f, yFloat);
                // curve down to the second point.
                path.curveTo(xFloat + 0.25f, yFloat + 0.25f, xFloat - 0.25f,
                        yFloat + 2.0f, xFloat, yFloat + 2.0f);
                // move straight down to the next point.
                path.lineTo(xFloat, yFloat + heightFloat / 1.60f);
                // move down and right to form the left half of the pointy section.
                path.lineTo(xFloat + widthFloat / 2, yFloat + heightFloat);
                // move up and right to form the right half of the pointy section.
                path.lineTo(xFloat + widthFloat, yFloat + heightFloat / 1.60f);
                // move straight up to the point to curve from.
                path.lineTo(xFloat + widthFloat, yFloat + 2.0f);
                // curve up and right to the top of the thumb.
                path.curveTo(xFloat + widthFloat - 0.25f, yFloat + 2.0f,
                        xFloat + widthFloat - 0.25f, yFloat + 0.25f,
                        xFloat + widthFloat - 2.0f, yFloat);
                path.closePath();

                return path;
            }
        };
    }
}
