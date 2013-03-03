package com.explodingpixels.swingx;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.swing.JPanel;

import com.explodingpixels.painter.MacWidgetsPainter;

public class EPPanel extends JPanel {

    private MacWidgetsPainter<Component> fBackgroundPainter;

    public EPPanel() {
        super();
        init();
    }

    private void init() {
        setOpaque(false);
    }

    public void setBackgroundPainter(MacWidgetsPainter<Component> painter) {
        fBackgroundPainter = painter;
    }

    @Override
    protected void paintComponent(Graphics g) {
        if (fBackgroundPainter != null) {
            Graphics2D graphics2D = (Graphics2D) g.create();
            fBackgroundPainter.paint(graphics2D, this, getWidth(), getHeight());
            graphics2D.dispose();
        }

        // TODO see if we can get rid of this call to super.paintComponent.
        super.paintComponent(g);
    }


}
