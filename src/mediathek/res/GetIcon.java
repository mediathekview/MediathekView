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
    private static final int w = 30, h = 30;
    public static final ImageIcon ICON_TAB_FILM = GetIcon.getGuiIcon("tab-film.png", w, h);
    public static final ImageIcon ICON_TAB_DOWNLOAD = GetIcon.getGuiIcon("tab-download.png", w, h);
    public static final ImageIcon ICON_TAB_ABO = GetIcon.getGuiIcon("tab-abo.png", w, h);
    public static final ImageIcon ICON_TAB_MELDUNG = GetIcon.getGuiIcon("tab-meldung.png", w, h);
    public static final ImageIcon ICON_TAB_FILM_SW = GetIcon.getGuiIcon("tab-film-sw.png", w, h);
    public static final ImageIcon ICON_TAB_DOWNLOAD_SW = GetIcon.getGuiIcon("tab-download-sw.png", w, h);
    public static final ImageIcon ICON_TAB_ABO_SW = GetIcon.getGuiIcon("tab-abo-sw.png", w, h);
    public static final ImageIcon ICON_TAB_MELDUNG_SW = GetIcon.getGuiIcon("tab-meldung-sw.png", w, h);

    private static final int wt = 20, ht = 20;
    public static final ImageIcon ICON_TAB_TOP_FILM = GetIcon.getGuiIcon("tab-film.png", wt, ht);
    public static final ImageIcon ICON_TAB_TOP_DOWNLOAD = GetIcon.getGuiIcon("tab-download.png", wt, ht);
    public static final ImageIcon ICON_TAB_TOP_ABO = GetIcon.getGuiIcon("tab-abo.png", wt, ht);
    public static final ImageIcon ICON_TAB_TOP_MELDUNG = GetIcon.getGuiIcon("tab-meldung.png", wt, ht);
    public static final ImageIcon ICON_TAB_TOP_FILM_SW = GetIcon.getGuiIcon("tab-film-sw.png", wt, ht);
    public static final ImageIcon ICON_TAB_TOP_DOWNLOAD_SW = GetIcon.getGuiIcon("tab-download-sw.png", wt, ht);
    public static final ImageIcon ICON_TAB_TOP_ABO_SW = GetIcon.getGuiIcon("tab-abo-sw.png", wt, ht);
    public static final ImageIcon ICON_TAB_TOP_MELDUNG_SW = GetIcon.getGuiIcon("tab-meldung-sw.png", wt, ht);

    private final static String PFAD_PROGRAMM = "/mediathek/res/programm/";
    private final static String PFAD_SENDER = "/mediathek/res/sender/";
    public final static String PFAD_RES = "/mediathek/res/";
    public final static String PFAD_GUI = "/mediathek/res/gui/";

    public static ImageIcon getGuiIcon(String strIcon, int w, int h) {
        return getIcon(strIcon, PFAD_GUI, w, h);
    }

    public static ImageIcon getGuiIcon(String strIcon) {
        return getIcon(strIcon, PFAD_GUI, 0, 0);
    }

    public static ImageIcon getIcon(String strIcon) {
        return getIcon(strIcon, PFAD_RES, 0, 0);
    }

    public static ImageIcon getSenderIcon(String strIcon) {
        return getIcon(strIcon, PFAD_SENDER, 0, 0);
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
