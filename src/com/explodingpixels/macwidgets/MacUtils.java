package com.explodingpixels.macwidgets;

import javax.swing.JRootPane;

import com.explodingpixels.util.PlatformUtils;
import com.jidesoft.utils.SystemInfo;

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
}
