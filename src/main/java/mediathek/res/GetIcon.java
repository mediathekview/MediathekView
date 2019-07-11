package mediathek.res;

import javax.swing.*;
import java.awt.*;

public class GetIcon {

    private final static String PFAD_PROGRAMM = "/mediathek/res/programm/";

    public static ImageIcon getProgramIcon(String strIcon, int w, int h) {
        return getIcon(strIcon, PFAD_PROGRAMM, w, h);
    }

    public static ImageIcon getIcon(String strIcon, String path, int w, int h) {
        ImageIcon icon = getStandard(strIcon, path);

        if (w > 0 && h > 0) {
            if (icon.getIconWidth() != w || icon.getIconHeight() != h) {
                // nur dann macht es Sinn
                icon.setImage(icon.getImage().getScaledInstance(w, h, Image.SCALE_AREA_AVERAGING));
            }
        }
        return icon;
    }

    private static ImageIcon getStandard(String strIcon, String path) {
        return new ImageIcon(GetIcon.class.getResource(path + strIcon));
    }
}
