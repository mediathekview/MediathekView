/*    
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *    
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.daten;

import mediathek.tool.GuiFunktionen;

public class DatenAbo implements Comparable<DatenAbo> {
    //Tags Abo

    public static final String ABO = "Abonnement";
    public static final int ABO_MAX_ELEM = 10;
    public static final String ABO_NR = "Nr";
    public static final int ABO_NR_NR = 0;
    public static final String ABO_EINGESCHALTET = "aktiv";
    public static final int ABO_EINGESCHALTET_NR = 1;
    public static final String ABO_NAME = "Name";
    public static final int ABO_NAME_NR = 2;
    public static final String ABO_SENDER = DatenFilm.FILM_SENDER;
    public static final int ABO_SENDER_NR = 3;
    public static final String ABO_THEMA = DatenFilm.FILM_THEMA;
    public static final int ABO_THEMA_NR = 4;
    public static final String ABO_TITEL = DatenFilm.FILM_TITEL;
    public static final int ABO_TITEL_NR = 5;
    public static final String ABO_THEMA_TITEL = DatenFilm.FILM_THEMA + "-" + DatenFilm.FILM_TITEL;
    public static final int ABO_THEMA_TITEL_NR = 6;
    public static final String ABO_ZIELPFAD = "Zielpfad";
    public static final int ABO_ZIELPFAD_NR = 7;
    public static final String ABO_DOWN_DATUM = "letztes_Abo";
    public static final int ABO_DOWN_DATUM_NR = 8;
    public static final String ABO_PGRUPPE = "Programmgruppe";
    public static final int ABO_PGRUPPE_NR = 9;
    public static final String[] ABO_COLUMN_NAMES = {ABO_NR, ABO_EINGESCHALTET, ABO_NAME, ABO_SENDER, ABO_THEMA, ABO_TITEL, ABO_THEMA_TITEL,
        ABO_ZIELPFAD, ABO_DOWN_DATUM, ABO_PGRUPPE};
    public String[] arr;

    public DatenAbo() {
        makeArr();
    }

    public DatenAbo(String name, String sender, String thema,  String titel, String ziel, String programmname) {
        makeArr();
        arr[ABO_NAME_NR] = name;
        arr[ABO_SENDER_NR] = sender;
        arr[ABO_THEMA_NR] = thema;
        arr[ABO_TITEL_NR] = titel;
        arr[ABO_ZIELPFAD_NR] = ziel;
        arr[ABO_PGRUPPE_NR] = programmname;
    }

    public DatenAbo getCopy() {
        DatenAbo ret = new DatenAbo();
        for (int i = 0; i < arr.length; ++i) {
            ret.arr[i] = new String(this.arr[i]);
        }
        return ret;
    }

//    public boolean binEinmal() {
//        // true wenn EinmalAbo
//        if (arr[ABO_EINMAL_URL_NR].equals("")) {
//            return false;
//        } else {
//            return true;
//        }
//    }
    public boolean aboIstEingeschaltet() {
        if (arr[DatenAbo.ABO_EINGESCHALTET_NR].equals("")) {
            aboEin();
            return true;
        }
        if (Boolean.parseBoolean(arr[DatenAbo.ABO_EINGESCHALTET_NR])) {
            return true;
        }
        return false;
    }

//    public boolean aboIstExakt() {
//        if (arr[DatenAbo.ABO_THEMA_EXAKT_NR].equals("")) {
//            arr[DatenAbo.ABO_THEMA_EXAKT_NR] = String.valueOf(true);
//            return true;
//        }
//        return Boolean.parseBoolean(arr[DatenAbo.ABO_THEMA_EXAKT_NR]);
//    }
    public boolean toggleAboEinAus() {
        // Abo EinAus wird geändert und der Zustand NACH der Änderung
        // wird zurückgegeben
        if (arr[DatenAbo.ABO_EINGESCHALTET_NR].equals("")) {
            aboEin();
        }
        arr[DatenAbo.ABO_EINGESCHALTET_NR] = String.valueOf(!Boolean.parseBoolean(arr[DatenAbo.ABO_EINGESCHALTET_NR]));
        return Boolean.parseBoolean(arr[DatenAbo.ABO_EINGESCHALTET_NR]);
    }

    public void aboAus() {
        arr[DatenAbo.ABO_EINGESCHALTET_NR] = String.valueOf(false);
    }

    public void aboEin() {
        arr[DatenAbo.ABO_EINGESCHALTET_NR] = String.valueOf(true);
    }

    public void aufMichKopieren(DatenAbo datenAbo) {
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = new String(datenAbo.arr[i]);
        }
    }

    public String aboPfadAnhaengen(String pfad) {
        String ret = pfad;
        if (!arr[ABO_ZIELPFAD_NR].equals("")) {
            ret = GuiFunktionen.addsPfad(pfad, arr[ABO_ZIELPFAD_NR]);
        }
        return ret;
    }

    private void makeArr() {
        arr = new String[ABO_MAX_ELEM];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = new String("");
        }
        // neue Abos sind immer ein
        aboEin();
    }

    @Override
    public int compareTo(DatenAbo arg0) {
        return (arr[ABO_THEMA_NR].compareToIgnoreCase(((DatenAbo) arg0).arr[ABO_THEMA_NR]));
    }
}
