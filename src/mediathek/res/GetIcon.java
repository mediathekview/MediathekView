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
import mSearch.tool.MVConfig;
import mediathek.tool.GuiFunktionen;

public class GetIcon {

    // Icons TABBED_PANE
    private static final int W = 30, H = 30;
    public static final ImageIcon ICON_TAB_FILM = GetIcon.getProgramIcon("tab-film.png", W, H);
    public static final ImageIcon ICON_TAB_DOWNLOAD = GetIcon.getProgramIcon("tab-download.png", W, H);
    public static final ImageIcon ICON_TAB_ABO = GetIcon.getProgramIcon("tab-abo.png", W, H);
    public static final ImageIcon ICON_TAB_MELDUNG = GetIcon.getProgramIcon("tab-meldung.png", W, H);
    public static final ImageIcon ICON_TAB_FILM_SW = GetIcon.getProgramIcon("tab-film-sw.png", W, H);
    public static final ImageIcon ICON_TAB_DOWNLOAD_SW = GetIcon.getProgramIcon("tab-download-sw.png", W, H);
    public static final ImageIcon ICON_TAB_ABO_SW = GetIcon.getProgramIcon("tab-abo-sw.png", W, H);
    public static final ImageIcon ICON_TAB_MELDUNG_SW = GetIcon.getProgramIcon("tab-meldung-sw.png", W, H);

    private static final int WT = 20, HT = 20;
    public static final ImageIcon ICON_TAB_TOP_FILM = GetIcon.getProgramIcon("tab-film.png", WT, HT);
    public static final ImageIcon ICON_TAB_TOP_DOWNLOAD = GetIcon.getProgramIcon("tab-download.png", WT, HT);
    public static final ImageIcon ICON_TAB_TOP_ABO = GetIcon.getProgramIcon("tab-abo.png", WT, HT);
    public static final ImageIcon ICON_TAB_TOP_MELDUNG = GetIcon.getProgramIcon("tab-meldung.png", WT, HT);
    public static final ImageIcon ICON_TAB_TOP_FILM_SW = GetIcon.getProgramIcon("tab-film-sw.png", WT, HT);
    public static final ImageIcon ICON_TAB_TOP_DOWNLOAD_SW = GetIcon.getProgramIcon("tab-download-sw.png", WT, HT);
    public static final ImageIcon ICON_TAB_TOP_ABO_SW = GetIcon.getProgramIcon("tab-abo-sw.png", WT, HT);
    public static final ImageIcon ICON_TAB_TOP_MELDUNG_SW = GetIcon.getProgramIcon("tab-meldung-sw.png", WT, HT);

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
            icon.setImage(icon.getImage().getScaledInstance(w, h, Image.SCALE_SMOOTH));
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
