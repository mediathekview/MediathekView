package raven.toast.util;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;

public class ShadowRenderer {

    private int size = 5;
    private float opacity = 0.5f;
    private Color color = Color.BLACK;

    public ShadowRenderer() {
        this(5, 0.5f, Color.BLACK);
    }

    public ShadowRenderer(final int size, final float opacity, final Color color) {
        this.size = size;
        this.opacity = opacity;
        this.color = color;
    }

    public Color getColor() {
        return color;
    }

    public float getOpacity() {
        return opacity;
    }

    public int getSize() {
        return size;
    }

    public BufferedImage createShadow(Shape shape) {
        Rectangle rec = shape.getBounds();
        BufferedImage img = new BufferedImage(rec.width, rec.height, BufferedImage.TYPE_INT_ARGB);
        Graphics2D g2 = img.createGraphics();
        g2.setColor(Color.BLACK);
        g2.translate(-rec.x, -rec.y);
        g2.fill(shape);
        g2.dispose();
        return createShadow(img);
    }

    public BufferedImage createShadow(final BufferedImage image) {
        int shadowSize = size * 2;
        int srcWidth = image.getWidth();
        int srcHeight = image.getHeight();
        int dstWidth = srcWidth + shadowSize;
        int dstHeight = srcHeight + shadowSize;
        int left = size;
        int right = shadowSize - left;
        int yStop = dstHeight - right;
        int shadowRgb = color.getRGB() & 0x00FFFFFF;
        int[] aHistory = new int[shadowSize];
        int historyIdx;
        int aSum;
        BufferedImage dst = new BufferedImage(dstWidth, dstHeight,
                BufferedImage.TYPE_INT_ARGB);
        int[] dstBuffer = new int[dstWidth * dstHeight];
        int[] srcBuffer = new int[srcWidth * srcHeight];
        getPixels(image, 0, 0, srcWidth, srcHeight, srcBuffer);
        int lastPixelOffset = right * dstWidth;
        float hSumDivider = 1.0f / shadowSize;
        float vSumDivider = opacity / shadowSize;
        int[] hSumLookup = new int[256 * shadowSize];
        for (int i = 0; i < hSumLookup.length; i++) {
            hSumLookup[i] = (int) (i * hSumDivider);
        }
        int[] vSumLookup = new int[256 * shadowSize];
        for (int i = 0; i < vSumLookup.length; i++) {
            vSumLookup[i] = (int) (i * vSumDivider);
        }
        int srcOffset;
        for (int srcY = 0, dstOffset = left * dstWidth; srcY < srcHeight; srcY++) {
            for (historyIdx = 0; historyIdx < shadowSize; ) {
                aHistory[historyIdx++] = 0;
            }
            aSum = 0;
            historyIdx = 0;
            srcOffset = srcY * srcWidth;
            for (int srcX = 0; srcX < srcWidth; srcX++) {
                int a = hSumLookup[aSum];
                dstBuffer[dstOffset++] = a << 24;
                aSum -= aHistory[historyIdx];
                a = srcBuffer[srcOffset + srcX] >>> 24;
                aHistory[historyIdx] = a;
                aSum += a;
                if (++historyIdx >= shadowSize) {
                    historyIdx -= shadowSize;
                }
            }
            for (int i = 0; i < shadowSize; i++) {
                int a = hSumLookup[aSum];
                dstBuffer[dstOffset++] = a << 24;
                aSum -= aHistory[historyIdx];
                if (++historyIdx >= shadowSize) {
                    historyIdx -= shadowSize;
                }
            }
        }

        for (int x = 0, bufferOffset = 0; x < dstWidth; x++, bufferOffset = x) {
            aSum = 0;
            for (historyIdx = 0; historyIdx < left; ) {
                aHistory[historyIdx++] = 0;
            }
            for (int y = 0; y < right; y++, bufferOffset += dstWidth) {
                int a = dstBuffer[bufferOffset] >>> 24;
                aHistory[historyIdx++] = a;
                aSum += a;
            }
            bufferOffset = x;
            historyIdx = 0;
            for (int y = 0; y < yStop; y++, bufferOffset += dstWidth) {
                int a = vSumLookup[aSum];
                dstBuffer[bufferOffset] = a << 24 | shadowRgb;
                aSum -= aHistory[historyIdx];
                a = dstBuffer[bufferOffset + lastPixelOffset] >>> 24;
                aHistory[historyIdx] = a;
                aSum += a;
                if (++historyIdx >= shadowSize) {
                    historyIdx -= shadowSize;
                }
            }
            for (int y = yStop; y < dstHeight; y++, bufferOffset += dstWidth) {
                int a = vSumLookup[aSum];
                dstBuffer[bufferOffset] = a << 24 | shadowRgb;
                aSum -= aHistory[historyIdx];
                if (++historyIdx >= shadowSize) {
                    historyIdx -= shadowSize;
                }
            }
        }
        setPixels(dst, 0, 0, dstWidth, dstHeight, dstBuffer);
        return dst;
    }

    private int[] getPixels(BufferedImage img, int x, int y, int w, int h, int[] pixels) {
        if (w == 0 || h == 0) {
            return new int[0];
        }
        if (pixels == null) {
            pixels = new int[w * h];
        } else if (pixels.length < w * h) {
            throw new IllegalArgumentException("pixels array must have a length" + " >= w*h");
        }
        int imageType = img.getType();
        if (imageType == BufferedImage.TYPE_INT_ARGB || imageType == BufferedImage.TYPE_INT_RGB) {
            Raster raster = img.getRaster();
            return (int[]) raster.getDataElements(x, y, w, h, pixels);
        }
        return img.getRGB(x, y, w, h, pixels, 0, w);
    }

    private void setPixels(BufferedImage img, int x, int y, int w, int h, int[] pixels) {
        if (pixels == null || w == 0 || h == 0) {
            return;
        } else if (pixels.length < w * h) {
            throw new IllegalArgumentException("pixels array must have a length" + " >= w*h");
        }
        int imageType = img.getType();
        if (imageType == BufferedImage.TYPE_INT_ARGB || imageType == BufferedImage.TYPE_INT_RGB) {
            WritableRaster raster = img.getRaster();
            raster.setDataElements(x, y, w, h, pixels);
        } else {
            img.setRGB(x, y, w, h, pixels, 0, w);
        }
    }
}