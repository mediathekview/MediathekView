package com.explodingpixels.macwidgets;

import java.awt.Color;

import com.explodingpixels.util.PlatformUtils;

/**
 * Utility methods for dealing with Mac colors.
 */
public class MacColorUtils {

    public static Color EMPTY_COLOR = new Color(0, 0, 0, 0);

    // Leopard colors. ////////////////////////////////////////////////////////////////////////////

    public static Color LEOPARD_BORDER_COLOR = new Color(0x555555);

    // OS X unified toolbar colors. ///////////////////////////////////////////////////////////////

    private static final Color LEOPARD_TEXTURED_WINDOW_FOCUSED_BORDER_COLOR = new Color(64, 64, 64);
    private static final Color LEOPARD_TEXTURED_WINDOW_UNFOCUSED_BORDER_COLOR = new Color(135, 135, 135);
    private static final Color TEXTURED_WINDOW_FOCUSED_BORDER_COLOR = new Color(0x515151);
    private static final Color TEXTURED_WINDOW_UNFOCUSED_BORDER_COLOR = new Color(0x969696);

    /**
     * Gets the color used to separate a {@link UnifiedToolBar} from the window content when the
     * window is active.
     *
     * @return the border color when the window is active.
     */
    public static Color getTexturedWindowToolbarBorderFocusedColor() {
        return PlatformUtils.isLeopard()
                ? LEOPARD_TEXTURED_WINDOW_FOCUSED_BORDER_COLOR
                : TEXTURED_WINDOW_FOCUSED_BORDER_COLOR;
    }

    /**
     * Gets the color used to separate a {@link UnifiedToolBar} from the window content when the
     * window is inactive.
     *
     * @return the border color when the window is inactive.
     */
    public static Color getTexturedWindowToolbarBorderUnfocusedColor() {
        return PlatformUtils.isLeopard()
                ? LEOPARD_TEXTURED_WINDOW_UNFOCUSED_BORDER_COLOR
                : TEXTURED_WINDOW_UNFOCUSED_BORDER_COLOR;
    }
}
