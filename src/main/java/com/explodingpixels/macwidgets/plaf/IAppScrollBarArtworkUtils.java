package com.explodingpixels.macwidgets.plaf;

import java.awt.Dimension;
import java.awt.Image;

import javax.swing.AbstractButton;
import javax.swing.ImageIcon;

import com.explodingpixels.widgets.ImageButton;
import com.explodingpixels.widgets.ImageUtils;
import com.explodingpixels.widgets.plaf.ScrollThumbImagePainter;

public class IAppScrollBarArtworkUtils {

    private IAppScrollBarArtworkUtils() {
        // utility class - no constructor needed.
    }

    /* iApp vertical scroll bar image locations. */

    private static final String V_SCROLLER_CAP =
            "/com/explodingpixels/macwidgets/images/v_scroller_top_cap.png";

    private static final String V_TRACK =
            "/com/explodingpixels/macwidgets/images/v_scroller_track.png";

    private static final String V_SCROLLER_TOGETHER_DECREMENT_BUTTONS =
            "/com/explodingpixels/macwidgets/images/v_scroller_together_decrement_button.png";

    private static final String V_SCROLLER_TOGETHER_INCREMENT_BUTTONS =
            "/com/explodingpixels/macwidgets/images/v_scroller_together_increment_button.png";

    private static final String V_SCROLLER_SEPARATE_DECREMENT_BUTTONS =
            "/com/explodingpixels/macwidgets/images/v_scroller_separate_decrement_button.png";

    private static final String V_SCROLLER_SEPARATE_INCREMENT_BUTTONS =
            "/com/explodingpixels/macwidgets/images/v_scroller_separate_increment_button.png";

    private static final String V_SCROLLERS =
            "/com/explodingpixels/macwidgets/images/v_scroller.png";

    private static final String V_TOP_BUTTON_MASK =
            "/com/explodingpixels/macwidgets/images/v_scroller_mask_top.png";

    private static final String V_BOTTOM_BUTTON_MASK =
            "/com/explodingpixels/macwidgets/images/v_scroller_mask_bottom.png";

    /* iApp horizontal scroll bar image locations. */

    private static final String H_SCROLLER_CAP =
            "/com/explodingpixels/macwidgets/images/h_scroller_left_cap.png";

    private static final String H_TRACK =
            "/com/explodingpixels/macwidgets/images/h_scroller_track.png";

    private static final String H_SCROLLER_TOGETHER_DECREMENT_BUTTONS =
            "/com/explodingpixels/macwidgets/images/h_scroller_together_decrement_button.png";

    private static final String H_SCROLLER_TOGETHER_INCREMENT_BUTTONS =
            "/com/explodingpixels/macwidgets/images/h_scroller_together_increment_button.png";

    private static final String H_SCROLLER_SEPARATE_DECREMENT_BUTTONS =
            "/com/explodingpixels/macwidgets/images/h_scroller_separate_decrement_button.png";

    private static final String H_SCROLLER_SEPARATE_INCREMENT_BUTTONS =
            "/com/explodingpixels/macwidgets/images/h_scroller_separate_increment_button.png";

    private static final String H_SCROLLERS =
            "/com/explodingpixels/macwidgets/images/h_scroller.png";

    private static final String H_LEFT_BUTTON_MASK =
            "/com/explodingpixels/macwidgets/images/h_scroller_mask_left.png";

    private static final String H_RIGHT_BUTTON_MASK =
            "/com/explodingpixels/macwidgets/images/h_scroller_mask_right.png";

    // iApp vertical scroll bar support methods. //////////////////////////////////////////////////

    public static AbstractButton createVerticalTogetherDecrementButton() {
        ArtworkUtils.ImageSet images = ArtworkUtils.getImageSet(
                IAppScrollBarArtworkUtils.class.getResource(V_SCROLLER_TOGETHER_DECREMENT_BUTTONS));
        return createButton(images, getBottomButtonMask());
    }

    public static AbstractButton createVerticalTogetherIncrementButton() {
        ArtworkUtils.ImageSet images = ArtworkUtils.getImageSet(
                IAppScrollBarArtworkUtils.class.getResource(V_SCROLLER_TOGETHER_INCREMENT_BUTTONS));
        return createButton(images);
    }

    public static AbstractButton createVerticalSeparateDecrementButton() {
        ArtworkUtils.ImageSet images = ArtworkUtils.getImageSet(
                IAppScrollBarArtworkUtils.class.getResource(V_SCROLLER_SEPARATE_DECREMENT_BUTTONS));
        return createButton(images, getTopButtonMask());
    }

    public static AbstractButton createVerticalSeparateIncrementButton() {
        ArtworkUtils.ImageSet images = ArtworkUtils.getImageSet(
                IAppScrollBarArtworkUtils.class.getResource(V_SCROLLER_SEPARATE_INCREMENT_BUTTONS));
        return createButton(images, getBottomButtonMask());
    }

    public static ScrollThumbImagePainter createVerticalScrollerThumb() {
        ArtworkUtils.ImageSet images = ArtworkUtils.getImageSet(
                IAppScrollBarArtworkUtils.class.getResource(V_SCROLLERS));

        Image disabledLowerBound = ImageUtils.getSubImage(images.getInactiveImage().getImage(), 0, 0, 13, 8);
        Image disabledMiddle = ImageUtils.getSubImage(images.getInactiveImage().getImage(), 0, 8, 13, 8);
        Image disabledUpperBound = ImageUtils.getSubImage(images.getInactiveImage().getImage(), 0, 16, 13, 8);

        Image inactiveLowerBound = ImageUtils.getSubImage(images.getActiveImage().getImage(), 0, 0, 13, 8);
        Image inactiveMiddle = ImageUtils.getSubImage(images.getActiveImage().getImage(), 0, 8, 13, 8);
        Image inactiveUpperBound = ImageUtils.getSubImage(images.getActiveImage().getImage(), 0, 16, 13, 8);

        Image activeLowerBound = ImageUtils.getSubImage(images.getPressedImage().getImage(), 0, 0, 13, 8);
        Image activeMiddle = ImageUtils.getSubImage(images.getPressedImage().getImage(), 0, 8, 13, 8);
        Image activeUpperBound = ImageUtils.getSubImage(images.getPressedImage().getImage(), 0, 16, 13, 8);

        return ScrollThumbImagePainter.createVerticalScrollThumbImagePainter(
                disabledLowerBound, disabledMiddle, disabledUpperBound,
                inactiveLowerBound, inactiveMiddle, inactiveUpperBound,
                activeLowerBound, activeMiddle, activeUpperBound);
    }

    public static int getVerticalScrollBarMiniumumHeight() {
        ImageIcon scrollers = new ImageIcon(IAppScrollBarArtworkUtils.class.getResource(V_SCROLLERS));
        return scrollers.getIconHeight();
    }

    public static Dimension getVerticalScrollBarMinimumSize() {
        return new Dimension(getVerticalTrack().getIconWidth(), getVerticalScrollBarMiniumumHeight());
    }

    private static ImageIcon getTopButtonMask() {
        return new ImageIcon(IAppScrollBarArtworkUtils.class.getResource(V_TOP_BUTTON_MASK));
    }

    private static ImageIcon getBottomButtonMask() {
        return new ImageIcon(IAppScrollBarArtworkUtils.class.getResource(V_BOTTOM_BUTTON_MASK));
    }

    public static ImageIcon getScrollBarTopCap() {
        ArtworkUtils.ImageSet images = ArtworkUtils.getImageSet(
                IAppScrollBarArtworkUtils.class.getResource(V_SCROLLER_CAP));
        return images.getActiveImage();
    }

    public static ImageIcon getVerticalTrack() {
        return new ImageIcon(IAppScrollBarArtworkUtils.class.getResource(V_TRACK));
    }

    // iApp vertical scroll bar support methods. //////////////////////////////////////////////////

    public static AbstractButton createHorizontalTogetherDecrementButton() {
        ArtworkUtils.ImageSet images = ArtworkUtils.getImageSet(
                IAppScrollBarArtworkUtils.class.getResource(H_SCROLLER_TOGETHER_DECREMENT_BUTTONS));
        return createButton(images, getRightButtonMask());
    }

    public static AbstractButton createHorizontalTogetherIncrementButton() {
        ArtworkUtils.ImageSet images = ArtworkUtils.getImageSet(
                IAppScrollBarArtworkUtils.class.getResource(H_SCROLLER_TOGETHER_INCREMENT_BUTTONS));
        return createButton(images);
    }

    public static AbstractButton createHorizontalSeparateDecrementButton() {
        ArtworkUtils.ImageSet images = ArtworkUtils.getImageSet(
                IAppScrollBarArtworkUtils.class.getResource(H_SCROLLER_SEPARATE_DECREMENT_BUTTONS));
        return createButton(images, getLeftButtonMask());
    }

    public static AbstractButton createHorizontalSeparateIncrementButton() {
        ArtworkUtils.ImageSet images = ArtworkUtils.getImageSet(
                IAppScrollBarArtworkUtils.class.getResource(H_SCROLLER_SEPARATE_INCREMENT_BUTTONS));
        return createButton(images, getRightButtonMask());
    }

    public static ScrollThumbImagePainter createHorizontalScrollerThumb() {
        ArtworkUtils.ImageSet images = ArtworkUtils.getImageSet(
                IAppScrollBarArtworkUtils.class.getResource(H_SCROLLERS));

        Image disabledLowerBound = ImageUtils.getSubImage(images.getInactiveImage().getImage(), 0, 0, 8, 13);
        Image disabledMiddle = ImageUtils.getSubImage(images.getInactiveImage().getImage(), 8, 0, 8, 13);
        Image disabledUpperBound = ImageUtils.getSubImage(images.getInactiveImage().getImage(), 16, 0, 8, 13);

        Image inactiveLowerBound = ImageUtils.getSubImage(images.getActiveImage().getImage(), 0, 0, 8, 13);
        Image inactiveMiddle = ImageUtils.getSubImage(images.getActiveImage().getImage(), 8, 0, 8, 13);
        Image inactiveUpperBound = ImageUtils.getSubImage(images.getActiveImage().getImage(), 16, 0, 8, 13);

        Image activeLowerBound = ImageUtils.getSubImage(images.getPressedImage().getImage(), 0, 0, 8, 13);
        Image activeMiddle = ImageUtils.getSubImage(images.getPressedImage().getImage(), 8, 0, 8, 13);
        Image activeUpperBound = ImageUtils.getSubImage(images.getPressedImage().getImage(), 16, 0, 8, 13);

        return ScrollThumbImagePainter.createHorizontalScrollThumbImagePainter(
                disabledLowerBound, disabledMiddle, disabledUpperBound,
                inactiveLowerBound, inactiveMiddle, inactiveUpperBound,
                activeLowerBound, activeMiddle, activeUpperBound);
    }

    public static int getHorizontalScrollBarMiniumumWidth() {
        ImageIcon scrollers = new ImageIcon(IAppScrollBarArtworkUtils.class.getResource(H_SCROLLERS));
        // subtracting an additional 4 pixels because the horizontal scroll bars are padded in the
        // original image with 2 pixels on each side.
        return scrollers.getIconWidth() / 3;
    }

    public static Dimension getHorizontalScrollBarMinimumSize() {
        return new Dimension(getHorizontalScrollBarMiniumumWidth(), getHorizontalTrack().getIconHeight());
    }

    private static ImageIcon getLeftButtonMask() {
        return new ImageIcon(IAppScrollBarArtworkUtils.class.getResource(H_LEFT_BUTTON_MASK));
    }

    private static ImageIcon getRightButtonMask() {
        return new ImageIcon(IAppScrollBarArtworkUtils.class.getResource(H_RIGHT_BUTTON_MASK));
    }

    public static ImageIcon getScrollBarLeftCap() {
        ArtworkUtils.ImageSet images = ArtworkUtils.getImageSet(
                IAppScrollBarArtworkUtils.class.getResource(H_SCROLLER_CAP));
        return images.getActiveImage();
    }

    public static ImageIcon getHorizontalTrack() {
        return new ImageIcon(IAppScrollBarArtworkUtils.class.getResource(H_TRACK));
    }

    // Utility methods. ///////////////////////////////////////////////////////////////////////////

    private static AbstractButton createButton(ArtworkUtils.ImageSet images, ImageIcon buttonMask) {
        ImageButton retVal = new ImageButton(images.getActiveImage(), buttonMask);
        retVal.setPressedIcon(images.getPressedImage());
        retVal.setDisabledIcon(images.getInactiveImage());
        retVal.setInactiveIcon(images.getInactiveImage());
        return retVal;
    }

    private static AbstractButton createButton(ArtworkUtils.ImageSet images) {
        ImageButton retVal = new ImageButton(images.getActiveImage());
        retVal.setPressedIcon(images.getPressedImage());
        retVal.setDisabledIcon(images.getInactiveImage());
        retVal.setInactiveIcon(images.getInactiveImage());
        return retVal;
    }

    public static int getScrollBarTopCapRecess() {
        return 12;
    }

    public static int getDecrementButtonRecess() {
        return 11;
    }

}
