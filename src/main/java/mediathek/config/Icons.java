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
    public static final ImageIcon ICON_DIALOG_EIN_SW = GetIcon.getProgramIcon("dialog-ein-sw.png", 16, 16);

    public static final ImageIcon ICON_MENUE_DOWNLOAD_LOESCHEN = GetIcon.getProgramIcon("menue-download-loeschen.png", 16, 16);
    public static final ImageIcon ICON_MENUE_FILE_OPEN = GetIcon.getProgramIcon("menue-file-open.png", 16, 16);
    public static final ImageIcon ICON_MENUE_VORZIEHEN = GetIcon.getProgramIcon("menue-vorziehen.png", 16, 16);

    public static final ImageIcon ICON_TABELLE_EIN = GetIcon.getProgramIcon("tabelle-ein.png", 16, 16);
    public static final ImageIcon ICON_TABELLE_AUS = GetIcon.getProgramIcon("tabelle-aus.png", 5, 5);

    public static final ImageIcon ICON_BUTTON_REMOVE = GetIcon.getProgramIcon("button-remove.png", 16, 16);
    public static final ImageIcon ICON_BUTTON_ADD = GetIcon.getProgramIcon("button-add.png", 16, 16);
    public static final ImageIcon ICON_BUTTON_MOVE_DOWN = GetIcon.getProgramIcon("button-move-down.png", 16, 16);
    public static final ImageIcon ICON_BUTTON_MOVE_UP = GetIcon.getProgramIcon("button-move-up.png", 16, 16);
    public static final ImageIcon ICON_BUTTON_HELP = GetIcon.getProgramIcon("button-help.png", 16, 16);
    public static final ImageIcon ICON_BUTTON_CLEAR = GetIcon.getProgramIcon("button-clear.png", 16, 16);
    public static final ImageIcon ICON_BUTTON_DEL = GetIcon.getProgramIcon("button-del.png", 16, 16);
    public static final ImageIcon ICON_BUTTON_FILE_OPEN = GetIcon.getProgramIcon("button-file-open.png", 16, 16);

    // Icons TABBED_PANE
    public static final ImageIcon ICON_TAB_FILM = GetIcon.getProgramIcon("tab-film.png", 32, 32);
    public static final ImageIcon ICON_TAB_DOWNLOAD = GetIcon.getProgramIcon("tab-download.png", 32, 32);

    public static final Image ICON_TRAY = GetIcon.getProgramIcon("tray.png", 256, 256).getImage();
    public static final Image ICON_TRAY_ERROR = GetIcon.getProgramIcon("tray-fehler.png", 256, 256).getImage();
    public static final Image ICON_TRAY_DOWNLOAD = GetIcon.getProgramIcon("tray-download.png", 256, 256).getImage();

}
