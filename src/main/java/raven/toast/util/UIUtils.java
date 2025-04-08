package raven.toast.util;

import com.formdev.flatlaf.extras.FlatSVGIcon;

import javax.swing.*;
import java.awt.*;

/**
 * @author Raven
 */
public class UIUtils {

    public static Icon getIcon(String key, Icon defaultValue) {
        Icon icon = UIManager.getIcon(key);
        if (icon == null) {
            return defaultValue;
        }
        return icon;
    }

    public static Insets getInsets(String key, Insets defaultValue) {
        Insets insets = UIManager.getInsets(key);
        if (insets == null) {
            return defaultValue;
        }
        return insets;
    }

    public static String getString(String key, String defaultValue) {
        String string = UIManager.getString(key);
        if (string == null) {
            return defaultValue;
        }
        return string;
    }

    public static Icon createIcon(String path, Color color, float scale) {
        FlatSVGIcon icon = new FlatSVGIcon(path, scale);
        if (color != null) {
            FlatSVGIcon.ColorFilter colorFilter = new FlatSVGIcon.ColorFilter();
            colorFilter.add(new Color(150, 150, 150), color);
            icon.setColorFilter(colorFilter);
        }
        return icon;
    }
}
