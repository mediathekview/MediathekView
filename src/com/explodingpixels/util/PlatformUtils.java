package com.explodingpixels.util;

import com.jidesoft.utils.SystemInfo;

/**
 * Utility methods for dealing with the platform (e.g. Mac or Windows).
 */
public class PlatformUtils {

    private PlatformUtils() {
        // utility class - no constructor needed.
    }

    /**
     * True if this JVM is running on Mac OS X 10.5, Leopard.
     *
     * @return true if this JVM is running on Mac OS X 10.5, Leopard.
     */
    public static boolean isLeopard() {
        return SystemInfo.isMacOSX() && SystemInfo.getOSVersion().startsWith("10.5");
    }

}
