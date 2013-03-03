package com.explodingpixels.swingx;

import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JButton;

import com.explodingpixels.painter.MacWidgetsPainter;

public class EPButton extends JButton {

    private MacWidgetsPainter<AbstractButton> fBackgroundPainter;

    public EPButton() {
        super();
    }

    public EPButton(Icon icon) {
        super(icon);
    }

    public EPButton(String text) {
        super(text);
    }

    public EPButton(Action a) {
        super(a);
    }

    public EPButton(String text, Icon icon) {
        super(text, icon);
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
