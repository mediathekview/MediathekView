/*
 * Copyright (c) 2025 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.swing;

import com.formdev.flatlaf.FlatLaf;
import org.apache.commons.lang3.SystemUtils;
import org.jetbrains.annotations.NotNull;
import org.kordamp.ikonli.Ikon;
import org.kordamp.ikonli.swing.FontIcon;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeListener;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;


public class IconUtils {
    public static final int DEFAULT_SIZE = 16;
    public static final int DEFAULT_TOOLBAR_SIZE = 18;
    public static final Color DEFAULT_LIGHT_COLOR = new Color(110, 110, 110);
    public static final Color DEFAULT_DARK_COLOR = new Color(176, 177, 179);
    /// used only on macOS when icons are on window button level
    protected static final int DEFAULT_MAC_TOOLBAR_WINDOWBAR_SIZE = 16;
    protected static final int DEFAULT_WINDOWS_TOOLBAR_WINDOWBAR_SIZE = 18;
    private static final List<WeakReference<FontIcon>> themedIcons = new ArrayList<>();

    static {
        PropertyChangeListener lafListener = evt -> {
            if ("lookAndFeel".equals(evt.getPropertyName())) {
                // The L&F has changed, update our icons
                updateIconColors();
            }
        };
        UIManager.addPropertyChangeListener(lafListener);
    }

    public static FontIcon of(Ikon ikon) {
        return of(ikon, DEFAULT_SIZE);
    }

    /// Returns a smaller size for toolbar icons on macOS as we are installed close to the window controls
    protected static int windowBarSpecificSize() {
        int size;
        if (SystemUtils.IS_OS_MAC_OSX) {
            size = DEFAULT_MAC_TOOLBAR_WINDOWBAR_SIZE;
        }
        else if (SystemUtils.IS_OS_WINDOWS) {
            size = DEFAULT_WINDOWS_TOOLBAR_WINDOWBAR_SIZE;
        }
        else {
            size = DEFAULT_TOOLBAR_SIZE;
        }
        return size;
    }

    public static ImageIcon generateDisabledIcon(@NotNull Action action) {
        FontIcon normalIcon = (FontIcon) action.getValue(Action.SMALL_ICON);
        if (normalIcon != null) {
            return generateDisabledIcon(normalIcon);
        }
        else {
            return null;
        }
    }

    public static ImageIcon generateDisabledIcon(@NotNull Icon normalIcon) {
        BufferedImage img = new BufferedImage(normalIcon.getIconWidth(), normalIcon.getIconHeight(),
                BufferedImage.TYPE_INT_ARGB);
        Graphics2D g2 = img.createGraphics();
        try {
            normalIcon.paintIcon(null, g2, 0, 0);
        }
        finally {
            g2.dispose();
        }

        var disabledImg = GrayFilter.createDisabledImage(img);
        return new ImageIcon(disabledImg);
    }

    public static FontIcon windowBarSpecificToolbarIcon(Ikon ikon) {
        return of(ikon, windowBarSpecificSize());
    }

    public static FontIcon toolbarIcon(Ikon ikon) {
        return of(ikon, DEFAULT_TOOLBAR_SIZE);
    }

    public static FontIcon windowBarSpecificToolbarIcon(Ikon ikon, Color color) {
        // will not be stored in weak ref list as we do not want LaF changes
        return FontIcon.of(ikon, windowBarSpecificSize(), color);
    }

    private static Color defaultColor() {
        return FlatLaf.isLafDark() ? DEFAULT_DARK_COLOR : DEFAULT_LIGHT_COLOR;
    }

    public static FontIcon of(Ikon ikon, int size) {
        return of(ikon, size, defaultColor());
    }

    protected static FontIcon of(Ikon ikon, int size, Color color) {
        var icon = FontIcon.of(ikon, size, color);
        themedIcons.add(new WeakReference<>(icon));
        return icon;
    }

    private static void updateIconColors() {
        if (themedIcons.isEmpty()) {
            return;
        }

        var iter = themedIcons.iterator();
        while (iter.hasNext()) {
            var icon = iter.next().get();
            // remove dead icons which got GCed...
            if (icon == null) {
                System.out.println("REMOVED REFERENCE TO WEAK ICON");
                iter.remove();
            }
            else {
                icon.setIconColor(defaultColor());
            }
        }
    }
}
