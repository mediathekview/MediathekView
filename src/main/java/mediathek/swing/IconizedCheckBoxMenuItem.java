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

import org.kordamp.ikonli.swing.FontIcon;

import javax.swing.*;
import javax.swing.plaf.basic.BasicCheckBoxMenuItemUI;
import java.awt.*;

public class IconizedCheckBoxMenuItem extends JCheckBoxMenuItem {
    private final IconizedCheckBoxMenuItemUI myUI;

    public IconizedCheckBoxMenuItem(FontIcon icon, boolean state) {
        this(icon);
        setState(state);
    }

    public IconizedCheckBoxMenuItem(FontIcon icon) {
        super();
        // we need dummy text for icon width and height, otherwise we will no be called...
        setText("dummy");
        myUI = new IconizedCheckBoxMenuItemUI(icon);
        setUI(myUI);
    }

    @Override
    public void updateUI() {
        setUI(myUI);
    }

    static class IconizedCheckBoxMenuItemUI extends BasicCheckBoxMenuItemUI {
        private final FontIcon icon;

        public IconizedCheckBoxMenuItemUI(FontIcon icon) {
            super();
            this.icon = icon;
        }

        @Override
        protected void paintText(Graphics g, JMenuItem menuItem, Rectangle textRect, String text) {
        /*g.setColor(Color.RED);
        g.fill3DRect(textRect.x, textRect.y, textRect.width, textRect.height, false);*/
            //icon.setIconSize(textRect.height);
            icon.paintIcon(null, g, textRect.x, textRect.y);
        }
    }
}
