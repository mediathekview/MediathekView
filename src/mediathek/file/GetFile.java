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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;

/**
 *
 * @author emil
 */
public class GetFile {

    String PFAD_PSET_LINUX = "/mediathek/file/pset_linux.xml";
    String PFAD_PSET_WINDOWS = "/mediathek/file/pset_windows.xml";
    String PFAD_PSET_MAC = "/mediathek/file/pset_mac.xml";
    public static String PFAD_HILFETEXT_FILTER = "/mediathek/file/hilfetext_filter.txt";
    public static String PFAD_HILFETEXT_BLACKLIST = "/mediathek/file/hilfetext_blacklist.txt";
    public static String PFAD_HILFETEXT_PRGRAMME = "/mediathek/file/hilfetext_programme.txt";
    public static String PFAD_HILFETEXT_STANDARD_PSET = "hilfetext_standardPset.txt";
    public static String PFAD_HILFETEXT_UNICODE = "hilfetext_unicode.txt";
    public static String PFAD_HILFETEXT_RESET = "hilfetext_reset.txt";

    public String getHilfeSuchen(String pfad) {
        String ret = "";
        try {
            InputStreamReader in = new InputStreamReader(getClass().getResource(pfad).openStream(), Konstanten.KODIERUNG_UTF);
            BufferedReader br = new BufferedReader(in);
            String strLine;
            while ((strLine = br.readLine()) != null) {
                ret = ret + "\n" + strLine;
            }
            //Close the input stream
            in.close();
        } catch (IOException ex) {
            Log.fehlerMeldung(885692213, Log.FEHLER_ART_PROG, this.getClass().getSimpleName() + ".getHilfeSuchen", ex);
        }
        return ret;
    }

    public InputStream getPsetVorlageLinux() {
        try {
            return getClass().getResource(PFAD_PSET_LINUX).openStream();
        } catch (IOException ex) {
            Log.fehlerMeldung(469691002, Log.FEHLER_ART_PROG, this.getClass().getSimpleName() + ".getLinux", ex);
        }
        return null;
    }

    public InputStream getPsetVorlageWindows() {
        try {
            return getClass().getResource(PFAD_PSET_WINDOWS).openStream();
        } catch (IOException ex) {
            Log.fehlerMeldung(842306087, Log.FEHLER_ART_PROG, this.getClass().getSimpleName() + ".getWindows", ex);
        }
        return null;
    }

    public InputStream getPsetVorlageMac() {
        try {
            return getClass().getResource(PFAD_PSET_MAC).openStream();
        } catch (IOException ex) {
            Log.fehlerMeldung(496532180, Log.FEHLER_ART_PROG, this.getClass().getSimpleName() + ".getMac", ex);
        }
        return null;
    }
}
