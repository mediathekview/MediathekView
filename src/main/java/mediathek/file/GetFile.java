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
package mediathek.file;

import mSearch.Const;
import mSearch.tool.Log;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 *
 * @author emil
 */
public class GetFile {

    public static final String PFAD_PSET_LINUX = "/mediathek/file/pset_linux.xml";
    public static final String PFAD_PSET_WINDOWS = "/mediathek/file/pset_windows.xml";
    public static final String PFAD_PSET_MAC = "/mediathek/file/pset_mac.xml";
    public static final String PFAD_HILFETEXT_GEO = "/mediathek/file/hilfetext_geo.txt";
    public static final String PFAD_HILFETEXT_FILTER = "/mediathek/file/hilfetext_filter.txt";
    public static final String PFAD_HILFETEXT_BLACKLIST = "/mediathek/file/hilfetext_blacklist.txt";
    public static final String PFAD_HILFETEXT_BEENDEN = "/mediathek/file/hilfetext_beenden.txt";
    public static final String PFAD_HILFETEXT_PRGRAMME = "/mediathek/file/hilfetext_pset.txt";
    public static final String PFAD_HILFETEXT_STANDARD_PSET = "hilfetext_standardPset.txt";
    public static final String PFAD_HILFETEXT_EDIT_DOWNLOAD_PROG = "hilfetext_editDownloadProg.txt";
    public static final String PFAD_HILFETEXT_UNICODE = "hilfetext_unicode.txt";
    public static final String PFAD_HILFETEXT_RESET = "hilfetext_reset.txt";
    public static final String PFAD_HILFETEXT_RESET_SET = "hilfetext_reset_set.txt";
    public static final String PFAD_HILFETEXT_DIALOG_MEDIA_DB = "hilfetext_dialog_mediaDb.txt";
    public static final String PFAD_HILFETEXT_PANEL_MEDIA_DB = "hilfetext_panel_mediaDb.txt";
    public static final String PFAD_HILFETEXT_DIALOG_ADD_ABO = "hilfetext_dialog_add_abo.txt";

    public String getHilfeSuchen(String pfad) {
        String ret = "";
        try (InputStreamReader in = new InputStreamReader(getClass().getResource(pfad).openStream(), Const.KODIERUNG_UTF);
             BufferedReader br = new BufferedReader(in)) {
            String strLine;
            while ((strLine = br.readLine()) != null) {
                ret = ret + "\n" + strLine;
            }
        } catch (IOException ex) {
            Log.errorLog(885692213, ex);
        }
        return ret;
    }

    public InputStreamReader getPsetVorlageLinux() {
        try {
            return new InputStreamReader(getClass().getResource(PFAD_PSET_LINUX).openStream(), Const.KODIERUNG_UTF);
        } catch (IOException ex) {
            Log.errorLog(469691002, ex);
        }
        return null;
    }

    public InputStreamReader getPsetVorlageWindows() {
        try {
            return new InputStreamReader(getClass().getResource(PFAD_PSET_WINDOWS).openStream(), Const.KODIERUNG_UTF);
        } catch (IOException ex) {
            Log.errorLog(842306087, ex);
        }
        return null;
    }

    public InputStreamReader getPsetVorlageMac() {
        try {
            return new InputStreamReader(getClass().getResource(PFAD_PSET_MAC).openStream(), Const.KODIERUNG_UTF);
        } catch (IOException ex) {
            Log.errorLog(496532180, ex);
        }
        return null;
    }
}
