package mediathek.mac.touchbar;

import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.concurrent.TimeUnit;

public class TouchBarUtils {
    private static final float TOUCHBAR_BUTTON_SIZE = 64.0f;
    private static final Logger logger = LogManager.getLogger();
    private static boolean TOUCHBAR_SUPPORTED;

    static {
        checkTouchBarSupport();
    }

    public static byte[] getImgBytes(@NotNull BufferedImage image) {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            ImageIO.write(image, "PNG", baos);
        } catch (IOException ex) {
            System.out.println(ex.getMessage());
        }
        return baos.toByteArray();
    }

    public static Icon iconFromFontAwesome(FontAwesome glyph) {
        return IconFontSwing.buildIcon(glyph, TouchBarUtils.TOUCHBAR_BUTTON_SIZE, Color.WHITE);
    }

    public static com.thizzer.jtouchbar.common.Image touchBarImageFromIcon(@NotNull Icon icon) {
        return new com.thizzer.jtouchbar.common.Image(TouchBarUtils.getImgBytes(TouchBarUtils.iconToImage(icon)));
    }

    public static com.thizzer.jtouchbar.common.Image touchBarImageFromFontAwesome(@NotNull FontAwesome glyph) {
        return touchBarImageFromIcon(iconFromFontAwesome(glyph));
    }

    public static BufferedImage iconToImage(@NotNull Icon icon) {
        BufferedImage image = new BufferedImage(
                icon.getIconWidth(),
                icon.getIconHeight(),
                BufferedImage.TYPE_INT_ARGB
        );
        Graphics g = image.createGraphics();
        // paint the Icon to the BufferedImage.
        icon.paintIcon(null, g, 0, 0);
        g.dispose();
        return image;
    }

    /**
     * Check if touch bar support is available in macOS.
     * Needs to use external helper for verification.
     */
    private static void checkTouchBarSupport() {
        logger.trace("checkTouchBarSupport");
        TOUCHBAR_SUPPORTED = false;
        try {
            var process = Runtime.getRuntime().exec("bin/mv_touchbar_support");
            if (process.waitFor(2, TimeUnit.SECONDS)) {
                // no timeout
                TOUCHBAR_SUPPORTED = process.exitValue() == 0;
            }
        } catch (Exception e) {
            logger.error("Failed to check touchbar support", e);
        }
    }

    public static boolean isTouchBarSupported() {
        return TOUCHBAR_SUPPORTED;
    }
}
