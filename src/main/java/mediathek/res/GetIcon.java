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
import mSearch.tool.Log;
import mediathek.config.MVConfig;
import mediathek.tool.GuiFunktionen;

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
