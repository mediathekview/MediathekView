package com.explodingpixels.widgets;

import java.awt.Dimension;
import java.awt.Image;

import com.explodingpixels.painter.ImagePainter;
import com.explodingpixels.swingx.EPPanel;

public class ImageBasedJComponent extends EPPanel {

    private final ImagePainter fPainter;

    public ImageBasedJComponent(Image image) {
        fPainter = new ImagePainter(image);
        setBackgroundPainter(fPainter);
    }

    @Override
    public Dimension getPreferredSize() {
        return new Dimension(fPainter.getImage().getWidth(null),
                fPainter.getImage().getHeight(null));
    }
}
