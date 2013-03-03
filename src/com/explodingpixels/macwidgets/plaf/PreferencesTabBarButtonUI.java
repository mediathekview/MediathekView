package com.explodingpixels.macwidgets.plaf;

import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Paint;

import javax.swing.AbstractButton;
import javax.swing.BorderFactory;
import javax.swing.ButtonModel;
import javax.swing.JComponent;

import com.explodingpixels.widgets.WindowUtils;

public class PreferencesTabBarButtonUI extends UnifiedToolbarButtonUI {

    /* a fully transparent color to use when fading colors out.*/
    private static final Color TRANSPARENT_COLOR =
            new Color(0,0,0,0);

    /* colors to use when the button's window doesn't have focus. */
    private static final Color UNFOCUSED_BACKGROUND_CENTER_COLOR =
            new Color(0,0,0,29);
    private static final Color UNFOCUSED_INNER_BORDER_COLOR =
            new Color(0,0,0,38);
    private static final Color UNFOCUSED_OUTER_BORDER_COLOR =
            new Color(0,0,0,63);

    /* colors to use when the button's window does have focus. */
    private static final Color FOCUSED_BACKGROUND_CENTER_COLOR =
            new Color(0,0,0,56);
    private static final Color FOCUSED_INNER_BORDER_COLOR =
            new Color(0,0,0,80);
    private static final Color FOCUSED_OUTER_BORDER_COLOR =
            new Color(0,0,0,130);

    @Override
    protected void installDefaults(AbstractButton b) {
        super.installDefaults(b);
        // TODO save original values.
        b.setBorder(BorderFactory.createEmptyBorder(5,5,4,5));
        b.setOpaque(false);
    }

    @Override
    protected void uninstallDefaults(AbstractButton b) {
        super.uninstallDefaults(b);
        // TODO restore original values.
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        AbstractButton b = (AbstractButton) c;
        ButtonModel model = b.getModel();

        // if the button is selected, draw the selection background.
        if (model.isSelected()) {
            Graphics2D graphics = (Graphics2D) g.create();
            paintSelectedButtonBackground(b, graphics);
            graphics.dispose();
        }

        super.paint(g, c);
    }

    private static void paintSelectedButtonBackground(AbstractButton button,
                                                      Graphics2D graphics) {
        // determine if the containing window has focus.
        boolean isButtonsWindowFocused =
                WindowUtils.isParentWindowFocused(button);
        // get center graident colors, based on the focus state of the
        // containing window.
        Color centerColor = isButtonsWindowFocused
                ? FOCUSED_BACKGROUND_CENTER_COLOR
                : UNFOCUSED_BACKGROUND_CENTER_COLOR;
        Color innerBorderColor = isButtonsWindowFocused
                ? FOCUSED_INNER_BORDER_COLOR
                : UNFOCUSED_INNER_BORDER_COLOR;
        Color outterBorderColor = isButtonsWindowFocused
                ? FOCUSED_OUTER_BORDER_COLOR
                : UNFOCUSED_OUTER_BORDER_COLOR;

        // calculate the first gradient's stop y position, and the second
        // gradient's start y position. thesve values, shouldn't overlap, as
        // transparent colors are addative, and would thus result in
        // bleed-through.
        int topMiddleY = button.getHeight()/2;
        int bottomMiddleY = button.getHeight()/2+1;

        // create the top and bottom fill paint.
        Paint topCenterPaint = new GradientPaint(
                0f,0f,TRANSPARENT_COLOR,1f,topMiddleY,centerColor);
        Paint bottomCenterPaint = new GradientPaint(
                0f,bottomMiddleY,centerColor,1f,button.getHeight(),TRANSPARENT_COLOR);

        // draw the selection background gradient. note that we don't want to
        // draw where the border is as tranparent colors will bleed together.
        graphics.setPaint(topCenterPaint);
        int borderWidth = 2;
        int fillWidth = button.getWidth() - borderWidth * 2;
        graphics.fillRect(borderWidth,0,fillWidth,topMiddleY);
        graphics.setPaint(bottomCenterPaint);
        graphics.fillRect(borderWidth,topMiddleY,fillWidth,button.getHeight());

        // create the outter border top and bottom paint.
        Paint topOuterBorderPaint = new GradientPaint(
                0f,0f,TRANSPARENT_COLOR,1f,topMiddleY,outterBorderColor);
        Paint bottomOuterBorderPaint = new GradientPaint(
                0f,bottomMiddleY,outterBorderColor,1f,button.getHeight(),TRANSPARENT_COLOR);

        // draw the outter border line.
        graphics.setPaint(topOuterBorderPaint);
        int outterLeftBorderX = 0;
        int outterRightBorderX = button.getWidth() - 1;
        graphics.drawLine(outterLeftBorderX,0,outterLeftBorderX,topMiddleY);
        graphics.drawLine(outterRightBorderX,0,outterRightBorderX,topMiddleY);
        graphics.setPaint(bottomOuterBorderPaint);
        graphics.drawLine(outterLeftBorderX,bottomMiddleY,outterLeftBorderX,button.getHeight());
        graphics.drawLine(outterRightBorderX,bottomMiddleY,outterRightBorderX,button.getHeight());

        // create the inner border top and bottom paint.
        Paint topInnerBorderPaint = new GradientPaint(
                0f,0f,TRANSPARENT_COLOR,1f,topMiddleY,innerBorderColor);
        Paint bottomInnerBorderPaint = new GradientPaint(
                0f,bottomMiddleY,innerBorderColor,1f,button.getHeight(),TRANSPARENT_COLOR);

        // draw the inner border line.
        graphics.setPaint(topInnerBorderPaint);
        int innerLeftBorderX = 1;
        int innerRightBorderX = button.getWidth() - 2;
        graphics.drawLine(innerLeftBorderX,0,innerLeftBorderX,topMiddleY);
        graphics.drawLine(innerRightBorderX,0,innerRightBorderX,topMiddleY);
        graphics.setPaint(bottomInnerBorderPaint);
        graphics.drawLine(innerLeftBorderX,bottomMiddleY,innerLeftBorderX,button.getHeight());
        graphics.drawLine(innerRightBorderX,bottomMiddleY,innerRightBorderX,button.getHeight());
    }
}
