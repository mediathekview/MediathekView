package mediathek.mac.touchbar;

import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import org.jetbrains.annotations.NotNull;
import oshi.SystemInfo;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

public class TouchBarUtils {
    private static final float TOUCHBAR_BUTTON_SIZE = 64.0f;

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

    public static boolean isTouchBarSupported() {
        boolean supported = false;

        var osv = new SystemInfo().getOperatingSystem().getVersionInfo();
        switch (osv.getCodeName()) {
            case "Catalina":
            case "Mojave":
            case "High Sierra":
                supported = true;
                break;
        }

        return supported;
    }
}
