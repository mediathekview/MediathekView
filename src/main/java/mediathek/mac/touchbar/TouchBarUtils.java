package mediathek.mac.touchbar;

import oshi.SystemInfo;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

public class TouchBarUtils {
    public static final float TOUCHBAR_BUTTON_SIZE = 64.0f;

    public static byte[] getImgBytes(BufferedImage image) {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            ImageIO.write(image, "PNG", baos);
        } catch (IOException ex) {
            System.out.println(ex.getMessage());
        }
        return baos.toByteArray();
    }

    public static BufferedImage iconToImage(Icon icon) {
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

            default:
                supported = false;
                break;
        }

        return supported;
    }
}
