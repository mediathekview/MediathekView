package com.explodingpixels.macwidgets;

import javax.swing.JRootPane;

import com.explodingpixels.util.PlatformUtils;

/**
 * A collection of utilities related to the Mac.
 */
public class MacUtils {

    /**
     * Makes this window a Unified window on Mac OS X Leopard or greater systems.
     *
     * @param rootPane
     */
    public static void makeWindowLeopardStyle(JRootPane rootPane) {
        // TODO figure out correct way to determine if the JRootPane has been
        // TODO realized.
        if (rootPane.isValid()) {
            throw new IllegalArgumentException("This method only works if the" +
                    "given JRootPane has not yet been realized.");
        }

        rootPane.putClientProperty("apple.awt.brushMetalLook", Boolean.TRUE);
    }

    /**
     * {@code true} if the Unified Tool Bar, Preference Tool Bar or Bottom Bar backgrounds should
     * be manually painted in code, rather than letting Mac OS X do the painting. This will always
     * return true on platforms other than Mac, and will sometimes return true on Mac's due to
     * painting bugs in the Java distrobution.
     */
    public static boolean shouldManuallyPaintTexturedWindowBackground() {
        boolean shouldManuallyPaintOnMac =
                PlatformUtils.isMac() && PlatformUtils.isLeopard() && PlatformUtils.isJava6OnMac();
        return !PlatformUtils.isMac() || shouldManuallyPaintOnMac;
    }
}
