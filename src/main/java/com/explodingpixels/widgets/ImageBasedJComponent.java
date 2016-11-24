package com.explodingpixels.widgets;

import com.explodingpixels.painter.ImagePainter;
import com.explodingpixels.swingx.EPPanel;

import java.awt.*;

@SuppressWarnings("serial")
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
