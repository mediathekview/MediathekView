package com.explodingpixels.swingx;

import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JToggleButton;

import com.explodingpixels.painter.MacWidgetsPainter;

public class EPToggleButton extends JToggleButton {

    private MacWidgetsPainter<AbstractButton> fBackgroundPainter;

    public EPToggleButton() {
        super();
    }

    public EPToggleButton(Icon icon) {
        super(icon);
    }

    public EPToggleButton(Icon icon, boolean selected) {
        super(icon, selected);
    }

    public EPToggleButton(String text) {
        super(text);
    }

    public EPToggleButton(String text, boolean selected) {
        super(text, selected);
    }

    public EPToggleButton(Action a) {
        super(a);
    }

    public EPToggleButton(String text, Icon icon) {
        super(text, icon);
    }

    public EPToggleButton(String text, Icon icon, boolean selected) {
        super(text, icon, selected);
    }

    @Override
    protected void init(String text, Icon icon) {
        super.init(text, icon);
        setOpaque(false);
    }

    public void setBackgroundPainter(MacWidgetsPainter<AbstractButton> painter) {
        fBackgroundPainter = painter;
    }

    @Override
    protected void paintComponent(Graphics g) {

        if (fBackgroundPainter != null) {
            Graphics2D graphics = (Graphics2D) g.create();
            fBackgroundPainter.paint(graphics, this, getWidth(), getHeight());
            graphics.dispose();
        }

        super.paintComponent(g);
    }
}
