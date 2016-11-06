package com.explodingpixels.macwidgets;

/**
 * An enumeration encapsulating the possible sizes of a Bottom Bar. The sizes called out in the
 * <a href="http://developer.apple.com/documentation/UserExperience/Conceptual/AppleHIGuidelines/XHIGWindows/chapter_18_section_4.html#//apple_ref/doc/uid/20000961-SW6">Apple Human Interface Guideline</a>
 * are 22 pixel and 32 pixel areas. These correspond to the {@link #SMALL} and {@link #LARGE}
 * sizes. The {@link #EXTRA_SMALL} size isn't officially in the HIG, but can be seen in use in
 * Safari's status bar for example.
 */
public enum BottomBarSize {

    EXTRA_SMALL(14), SMALL(22), LARGE(32);

    private int fHeight;

    BottomBarSize(int height) {
        fHeight = height;
    }

    /**
     * Gets the height in pixels that this size represents.
     *
     * @return the height in pixels that this size represents.
     */
    public int getHeight() {
        return fHeight;
    }
}
