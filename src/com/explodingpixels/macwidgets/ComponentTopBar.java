package com.explodingpixels.macwidgets;

import java.awt.Color;

import com.explodingpixels.painter.GradientWithBorderPainter;

public class ComponentTopBar extends TriAreaComponent {
    
    private static final Color TOP_BORDER_COLOR =
            new Color(212, 212, 212);

    private static final Color SOFT_TOP_GRADIENT_COLOR =
            new Color(248, 248, 248);

    private static final Color SOFT_BOTTOM_GRADIENT_COLOR =
            new Color(212, 212, 212);

    ComponentTopBar() {
        setBackgroundPainter(new GradientWithBorderPainter(
                TOP_BORDER_COLOR, SOFT_BOTTOM_GRADIENT_COLOR,
                SOFT_TOP_GRADIENT_COLOR, SOFT_BOTTOM_GRADIENT_COLOR));
    }
}
