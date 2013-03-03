package com.explodingpixels.widgets;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.image.BufferedImage;

/**
 * A utility class for working with {@link Image}s.
 */
public class ImageUtils {

    private ImageUtils() {
        // utility class - no constructor needed.
    }

    public static BufferedImage getHorizontalSubImage(Image image, int x, int width) {
        return getSubImage(image, x, 0, width, image.getHeight(null));
    }

    public static BufferedImage getVerticalSubImage(Image image, int y, int height) {
        return getSubImage(image, 0, y, image.getWidth(null), height);
    }

    public static BufferedImage getSubImage(Image image, int x, int y, int width, int height) {
        if (x >= image.getWidth(null)) {
            throw new IllegalArgumentException("The given x, " + x + ", must be less than the image width, " + width + ".");
        }
        if (y >= image.getHeight(null)) {
            throw new IllegalArgumentException("The given y, " + y + ", must be less than the image height, " + height + ".");
        }
        if (x + width > image.getWidth(null)) {
            throw new IllegalArgumentException(
                    "The given width must be less than or equal to the image width - x.");
        }
        if (y + height > image.getHeight(null)) {
            throw new IllegalArgumentException(
                    "The given height must be less than or equal to the image height - y.");
        }
        BufferedImage subImage = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR);
        Graphics graphics = subImage.getGraphics();
        graphics.drawImage(image, 0, 0, subImage.getWidth(), subImage.getHeight(),
                x, y, x + width, y + height, null);
        graphics.dispose();
        return subImage;
    }

}
