package com.explodingpixels.widgets.plaf;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;

import com.explodingpixels.painter.MacWidgetsPainter;
import com.explodingpixels.widgets.WindowUtils;

/**
 * An implementation of {@link com.explodingpixels.painter.MacWidgetsPainter} that paints a scroll thumb using images.
 */
public class ScrollThumbImagePainter implements MacWidgetsPainter<Component> {

    private final ScrollBarOrientation fOrientation;
    private final ImageSet fDisabledImageSet;
    private final ImageSet fInactiveImageSet;
    private final ImageSet fActiveImageSet;

    private ScrollThumbImagePainter(
            ScrollBarOrientation orientation,
            Image disabledLowerBound, Image disabledMiddle, Image disabledUpperBound,
            Image inactiveLowerBound, Image inactiveMiddle, Image inactiveUpperBound,
            Image activeLowerBound, Image activeMiddle, Image activeUpperBound) {

        // TODO add null checking.

        fOrientation = orientation;
        fDisabledImageSet = new ImageSet(disabledLowerBound, disabledMiddle, disabledUpperBound);
        fInactiveImageSet = new ImageSet(inactiveLowerBound, inactiveMiddle, inactiveUpperBound);
        fActiveImageSet = new ImageSet(activeLowerBound, activeMiddle, activeUpperBound);

    }

    /**
     * Creates an image-based vertical scroll thumb painter using the given images. Lower bound
     * refers to the area closest to the minimum value (top or left). Upper bound refers to the area
     * closest to the maximum value (bottom or right).
     *
     * @param disabledLowerBound the lower bound disabled image.
     * @param disabledMiddle     the middle disabled image.
     * @param disabledUpperBound the upper bound disabled image.
     * @param inactiveLowerBound the lower bound inactiveimage
     * @param inactiveMiddle     the middle inactive image
     * @param inactiveUpperBound the upper bound inactive image.
     * @param activeLowerBound   the lower bound active image.
     * @param activeMiddle       the middle active image.
     * @param activeUpperBound   the upper bound active image.
     * @return a state based (disabled or inactive or active) scroll thumb painter.
     */
    public static ScrollThumbImagePainter createVerticalScrollThumbImagePainter(
            Image disabledLowerBound, Image disabledMiddle, Image disabledUpperBound,
            Image inactiveLowerBound, Image inactiveMiddle, Image inactiveUpperBound,
            Image activeLowerBound, Image activeMiddle, Image activeUpperBound) {
        return new ScrollThumbImagePainter(ScrollBarOrientation.VERTICAL, disabledLowerBound,
                disabledMiddle, disabledUpperBound, inactiveLowerBound, inactiveMiddle,
                inactiveUpperBound, activeLowerBound, activeMiddle, activeUpperBound);
    }

    /**
     * Creates an image-based horizontal scroll thumb painter using the given images. Lower bound
     * refers to the area closest to the minimum value (top or left). Upper bound refers to the area
     * closest to the maximum value (bottom or right).
     *
     * @param disabledLowerBound the lower bound disabled image.
     * @param disabledMiddle     the middle disabled image.
     * @param disabledUpperBound the upper bound disabled image.
     * @param inactiveLowerBound the lower bound inactiveimage
     * @param inactiveMiddle     the middle inactive image
     * @param inactiveUpperBound the upper bound inactive image.
     * @param activeLowerBound   the lower bound active image.
     * @param activeMiddle       the middle active image.
     * @param activeUpperBound   the upper bound active image.
     * @return a state based (disabled or inactive or active) scroll thumb painter.
     */
    public static ScrollThumbImagePainter createHorizontalScrollThumbImagePainter(
            Image disabledLowerBound, Image disabledMiddle, Image disabledUpperBound,
            Image inactiveLowerBound, Image inactiveMiddle, Image inactiveUpperBound,
            Image activeLowerBound, Image activeMiddle, Image activeUpperBound) {
        return new ScrollThumbImagePainter(ScrollBarOrientation.HORIZONTAL, disabledLowerBound,
                disabledMiddle, disabledUpperBound, inactiveLowerBound, inactiveMiddle,
                inactiveUpperBound, activeLowerBound, activeMiddle, activeUpperBound);
    }

    private ImageSet getImageSet(Component objectToPaint) {
        ImageSet retVal;

        if (!objectToPaint.isEnabled()) {
            retVal = fDisabledImageSet;
        } else if (WindowUtils.isParentWindowFocused(objectToPaint)) {
            retVal = fActiveImageSet;
        } else {
            retVal = fInactiveImageSet;
        }

        return retVal;
    }

    public void paint(Graphics2D graphics, Component objectToPaint, int width, int height) {

        ImageSet imageSet = getImageSet(objectToPaint);

        // grab the size of the scroll bar in dimension agnostic terms.
        Dimension scrollerSize = new Dimension(width, height);
        int scrollerLength = fOrientation.getLength(scrollerSize);
        int scrollerThickness = fOrientation.getThickness(imageSet.getLowerBoundImageSize());

        // calculate the starting point of the lower bound image in dimension agnostic terms, such
        // that the image is draw centered in the containing component. note that we pass in -1 for
        // the length of the resultant bounds rectangle as we don't care about creating a second
        // coordinate.
        int lowerPosition = 0;
        Point lowerPoint = fOrientation.createCenteredBounds(
                objectToPaint, lowerPosition, scrollerThickness, -1).getLocation();
        graphics.drawImage(imageSet.getLowerBoundImage(), lowerPoint.x, lowerPoint.y, null);

        // calculate the starting point of the middle bound image in dimension agnostic terms. start
        // by determing the position (the value in the scrolling dimension) and length to draw the
        // image. then create bounds from that position and length.
        int upperLength = fOrientation.getLength(imageSet.getUpperBoundImageSize());
        int middleStartPosition = fOrientation.getLength(imageSet.getLowerBoundImageSize());
        int middleEndPosition = scrollerLength - upperLength;
        int middleLength = middleEndPosition - middleStartPosition;
        Rectangle middleBounds = fOrientation.createCenteredBounds(
                objectToPaint, middleStartPosition, scrollerThickness, middleLength);

        Image middleImage = imageSet.getMiddleImage();
        graphics.drawImage(middleImage, middleBounds.x, middleBounds.y,
                middleBounds.x + middleBounds.width, middleBounds.y + middleBounds.height,
                0, 0, middleImage.getWidth(null), middleImage.getHeight(null), null);

        // calculate the position (the value in the scrolling dimension) of the upper bound image,
        // such that the image is draw centered in the containing component. note that we pass in -1
        // for the length of the resultant bounds rectangle as we don't care about creating a second
        // coordinate.
        int upperPosition = scrollerLength - upperLength;
        Point upperPoint = fOrientation.createCenteredBounds(
                objectToPaint, upperPosition, scrollerThickness, -1).getLocation();
        graphics.drawImage(imageSet.getUpperBoundImage(), upperPoint.x, upperPoint.y, null);

    }

    // ImageSet implementation. ///////////////////////////////////////////////////////////////////

    private static class ImageSet {

        private final Image fLowerBoundImage;
        private final Image fMiddleImage;
        private final Image fUpperBoundImage;

        private Dimension fLowerBoundImageSize;
        private Dimension fMiddleBoundImageSize;
        private Dimension fUppoerBoundImageSize;

        private ImageSet(Image lowerBoundImage, Image middleImage, Image upperBoundImage) {
            fLowerBoundImage = lowerBoundImage;
            fMiddleImage = middleImage;
            fUpperBoundImage = upperBoundImage;

            fLowerBoundImageSize = createImageSize(fLowerBoundImage);
            fMiddleBoundImageSize = createImageSize(fMiddleImage);
            fUppoerBoundImageSize = createImageSize(fUpperBoundImage);
        }

        public Image getLowerBoundImage() {
            return fLowerBoundImage;
        }

        public Image getMiddleImage() {
            return fMiddleImage;
        }

        public Image getUpperBoundImage() {
            return fUpperBoundImage;
        }

        public Dimension getLowerBoundImageSize() {
            return fLowerBoundImageSize;
        }

        public Dimension getMiddleBoundImageSize() {
            return fMiddleBoundImageSize;
        }

        public Dimension getUpperBoundImageSize() {
            return fUppoerBoundImageSize;
        }

        private static Dimension createImageSize(Image image) {
            return new Dimension(image.getWidth(null), image.getHeight(null));
        }

    }
}
