/*
 * MediathekView
 * Copyright (C) 2014 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.config;

import mediathek.res.GetIcon;

import javax.swing.*;
import java.awt.*;

public class Icons {
    // Icons TABBED_PANE
    public static final ImageIcon ICON_TAB_FILM = GetIcon.getProgramIcon("tab-film.png", 32, 32);
    public static final ImageIcon ICON_TAB_DOWNLOAD = GetIcon.getProgramIcon("tab-download.png", 32, 32);

    public static final Image ICON_TRAY = GetIcon.getProgramIcon("tray.png", 256, 256).getImage();
    public static final Image ICON_TRAY_ERROR = GetIcon.getProgramIcon("tray-fehler.png", 256, 256).getImage();
    public static final Image ICON_TRAY_DOWNLOAD = GetIcon.getProgramIcon("tray-download.png", 256, 256).getImage();

}
