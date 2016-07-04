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

import mSearch.daten.DatenFilm;
import mSearch.tool.GermanStringSorter;
import mSearch.tool.Log;

public class DatenAbo implements Comparable<DatenAbo> {
    //Tags Abo

    private static GermanStringSorter sorter = GermanStringSorter.getInstance();
    public static final int ABO_NR_NR = 0;
    public static final int ABO_EINGESCHALTET_NR = 1;
    public static final int ABO_NAME_NR = 2;
    public static final int ABO_SENDER_NR = 3;
    public static final int ABO_THEMA_NR = 4;
    public static final int ABO_TITEL_NR = 5;
    public static final int ABO_THEMA_TITEL_NR = 6;
    public static final int ABO_IRGENDWO_NR = 7;
    public static final int ABO_MINDESTDAUER_NR = 8;
    public static final int ABO_ZIELPFAD_NR = 9;
    public static final int ABO_DOWN_DATUM_NR = 10;
    public static final int ABO_PSET_NR = 11;
    public static final String[] COLUMN_NAMES = {"Nr", "aktiv", "Name",
        DatenFilm.FILM_SENDER, DatenFilm.FILM_THEMA, DatenFilm.FILM_TITEL, DatenFilm.FILM_THEMA + "-" + DatenFilm.FILM_TITEL,
        "Irgendwo", "Mindestdauer", "Zielpfad", "letztes_Abo", "Programmset"};
    public static final String[] XML_NAMES = COLUMN_NAMES;

    public static final int MAX_ELEM = 12;
    public static final String TAG = "Abonnement";
    public static boolean[] spaltenAnzeigen = new boolean[MAX_ELEM];
    public int mindestdauerMinuten = 0;
    public String[] arr;
    public int nr = 0;

    public DatenAbo() {
        makeArr();
    }

    public DatenAbo(String name, String sender, String thema, String titel, String themaTitel, String irgendwo, int mmindestdauerMinuten, String ziel, String pset) {
        makeArr();
        arr[ABO_NAME_NR] = name;
        arr[ABO_SENDER_NR] = sender;
        arr[ABO_THEMA_NR] = thema;
        arr[ABO_TITEL_NR] = titel;
        arr[ABO_THEMA_TITEL_NR] = themaTitel;
        arr[ABO_IRGENDWO_NR] = irgendwo;
        setMindestDauerMinuten(mmindestdauerMinuten);
        arr[ABO_ZIELPFAD_NR] = ziel;
        arr[ABO_PSET_NR] = pset;
    }

    public DatenAbo getCopy() {
        DatenAbo ret = new DatenAbo();
        System.arraycopy(this.arr, 0, ret.arr, 0, arr.length);
        ret.mindestdauerMinuten = this.mindestdauerMinuten;
        return ret;
    }

    public boolean isEmpty() {
        //liefert TRUE wenn das Abo leer ist, also bei jedem Film ansprechen würde
        //ist dann offensichtlich falsch!!
        if (arr[ABO_SENDER_NR].isEmpty()
                && arr[ABO_THEMA_NR].isEmpty()
                && arr[ABO_TITEL_NR].isEmpty()
                && arr[ABO_THEMA_TITEL_NR].isEmpty()
                && arr[ABO_IRGENDWO_NR].isEmpty()) {
            return true;
        } else {
            return false;
        }
    }

    public final void setMindestDauerMinuten(int d) {
        mindestdauerMinuten = d;
        arr[ABO_MINDESTDAUER_NR] = String.valueOf(d);
    }

    public void setMindestDauerMinuten() {
        if (this.arr[DatenAbo.ABO_MINDESTDAUER_NR].equals("")) {
            // für den ProgUpdate
            mindestdauerMinuten = 0;
            arr[ABO_MINDESTDAUER_NR] = "0";
        }
        try {
            mindestdauerMinuten = Integer.parseInt(this.arr[DatenAbo.ABO_MINDESTDAUER_NR]);
        } catch (Exception ex) {
            Log.errorLog(462558700, ex);
            mindestdauerMinuten = 0;
            arr[ABO_MINDESTDAUER_NR] = "0";
        }
    }

    public boolean aboIstEingeschaltet() {
        if (arr[DatenAbo.ABO_EINGESCHALTET_NR].equals("")) {
            aboEin();
            return true;
        }
        return Boolean.parseBoolean(arr[DatenAbo.ABO_EINGESCHALTET_NR]);
    }

    public static boolean anzeigen(int i) {
        return spaltenAnzeigen == null || spaltenAnzeigen[i];
    }

    /*
     public boolean toggleAboEinAus() {
     // Abo EinAus wird geändert und der Zustand NACH der Änderung
     // wird zurückgegeben
     if (arr[DatenAbo.ABO_EINGESCHALTET_NR].equals("")) {
     aboEin();
     }
     arr[DatenAbo.ABO_EINGESCHALTET_NR] = String.valueOf(!Boolean.parseBoolean(arr[DatenAbo.ABO_EINGESCHALTET_NR]));
     return Boolean.parseBoolean(arr[DatenAbo.ABO_EINGESCHALTET_NR]);
     }
     */

 /*
     public void aboAus() {
     arr[DatenAbo.ABO_EINGESCHALTET_NR] = String.valueOf(false);
     }
     */
    public void aboEin() {
        arr[DatenAbo.ABO_EINGESCHALTET_NR] = String.valueOf(true);
    }

    public void aufMichKopieren(DatenAbo datenAbo) {
        System.arraycopy(datenAbo.arr, 0, arr, 0, arr.length);
        this.mindestdauerMinuten = datenAbo.mindestdauerMinuten;
    }

    /*
     public String aboPfadAnhaengen(String pfad) {
     String ret = pfad;
     if (!arr[ABO_ZIELPFAD_NR].equals("")) {
     ret = GuiFunktionen.addsPfad(pfad, arr[ABO_ZIELPFAD_NR]);
     }
     return ret;
     }
     */
    private void makeArr() {
        arr = new String[MAX_ELEM];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = "";
        }
        // neue Abos sind immer ein
        aboEin();
    }

    @Override
    public int compareTo(DatenAbo arg0) {
        return sorter.compare(arr[ABO_NAME_NR], arg0.arr[ABO_NAME_NR]);
    }
}
