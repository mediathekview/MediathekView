/*
 * MediathekView
 * Copyright (C) 2013 W. Xaver
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
package mediathek.res;

import java.awt.Image;
import java.io.File;
import javax.swing.ImageIcon;
import mSearch.tool.DebugMsg;
import mSearch.tool.Log;
import mSearch.tool.MVConfig;
import mediathek.tool.GuiFunktionen;

public class GetIcon {

    private final static String PFAD_PROGRAMM = "/mediathek/res/programm/";
    private final static String PFAD_SENDER = "/mediathek/res/sender/";
    public final static String PFAD_RES = "/mediathek/res/";

    public static ImageIcon getIcon(String strIcon) {
        return getIcon(strIcon, PFAD_RES, 0, 0);
    }

    public static ImageIcon getSenderIcon(String strIcon) {
        return getIcon(strIcon, PFAD_SENDER, 0, 0);
    }

    public static ImageIcon getProgramIcon(String strIcon, int w, int h) {
        DebugMsg.print("getIcon");
        return getIcon(strIcon, PFAD_PROGRAMM, w, h);
    }

    public static ImageIcon getProgramIcon(String strIcon) {
        return getIcon(strIcon, PFAD_PROGRAMM, 0, 0);
    }

    public static ImageIcon getIcon(String strIcon, String path, int w, int h) {
        ImageIcon icon;
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_ICON_STANDARD))) {
            icon = getStandard(strIcon, path);
        } else {
            try {
                String pfad = GuiFunktionen.addsPfad(MVConfig.get(MVConfig.SYSTEM_ICON_PFAD), strIcon);
                if (new File(pfad).exists()) {
                    icon = new ImageIcon(pfad);
                } else {
                    icon = getStandard(strIcon, path);
                }
            } catch (Exception ex) {
                Log.errorLog(932107891, strIcon);
                icon = getStandard(strIcon, path);
            }
        }
        if (w > 0 && h > 0) {
            if (icon.getIconWidth() != w || icon.getIconHeight() != h) {
                // nur dann macht es Sinn
                icon.setImage(icon.getImage().getScaledInstance(w, h, Image.SCALE_AREA_AVERAGING));
            }
        }
        return icon;
    }

    public static ImageIcon getIcon(String strIcon, String path) {
        return getIcon(strIcon, path, 0, 0);
    }

    private static ImageIcon getStandard(String strIcon, String path) {
        return new ImageIcon(GetIcon.class.getResource(path + strIcon));
    }
}
