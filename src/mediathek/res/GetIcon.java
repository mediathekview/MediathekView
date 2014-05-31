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
import mediathek.controller.Log;
import mediathek.daten.Daten;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MVConfig;

public class GetIcon {

    private final static String PFAD_INTERN = "/mediathek/res/";

    public static Image getImage(String strIcon) {
        return getIcon(strIcon).getImage();
    }

    public static ImageIcon getIcon(String strIcon) {
        if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_ICON_STANDARD))) {
            return getStandard(strIcon);
        } else {
            ImageIcon icon;
            try {
                String pfad = GuiFunktionen.addsPfad(Daten.mVConfig.get(MVConfig.SYSTEM_ICON_PFAD), strIcon);
//                System.out.println(new File(pfad).getAbsolutePath());
                if (new File(pfad).exists()) {
                    icon = new ImageIcon(pfad);
                } else {
                    icon = getStandard(strIcon);
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(932107891, Log.FEHLER_ART_PROG, "GetIcon.getIcon", strIcon);
                icon = getStandard(strIcon);
            }
            return icon;
        }
    }

    private static ImageIcon getStandard(String strIcon) {
        return new ImageIcon(GetIcon.class.getResource(PFAD_INTERN + strIcon));
    }
}
