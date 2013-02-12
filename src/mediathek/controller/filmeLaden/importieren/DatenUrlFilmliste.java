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
package mediathek.controller.filmeLaden.importieren;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import mediathek.tool.Log;

public class DatenUrlFilmliste implements Comparable<DatenUrlFilmliste> {

    public String[] arr;
    public static SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy, HH:mm:ss");

    public DatenUrlFilmliste() {
        makeArr();
    }

    public DatenUrlFilmliste(String url, String prio) {
        makeArr();
        arr[FilmlistenSuchen.FILM_UPDATE_SERVER_URL_NR] = url;
        arr[FilmlistenSuchen.FILM_UPDATE_SERVER_PRIO_NR] = prio;
    }

    public DatenUrlFilmliste(String url, String prio, String zeit, String datum) {
        makeArr();
        arr[FilmlistenSuchen.FILM_UPDATE_SERVER_URL_NR] = url;
        arr[FilmlistenSuchen.FILM_UPDATE_SERVER_PRIO_NR] = prio;
        arr[FilmlistenSuchen.FILM_UPDATE_SERVER_DATUM_NR] = datum;
        arr[FilmlistenSuchen.FILM_UPDATE_SERVER_ZEIT_NR] = zeit;
    }

    @Override
    public int compareTo(DatenUrlFilmliste arg0) {
        int ret = 0;
        try {
            //31.10.2010	16:54:17
            String ich = arr[FilmlistenSuchen.FILM_UPDATE_SERVER_DATUM_NR] + ", " + arr[FilmlistenSuchen.FILM_UPDATE_SERVER_ZEIT_NR];
            String du = arg0.arr[FilmlistenSuchen.FILM_UPDATE_SERVER_DATUM_NR] + ", " + arg0.arr[FilmlistenSuchen.FILM_UPDATE_SERVER_ZEIT_NR];
            if (ich.equals(du)) {
                return 0;
            }
            Date d_ich = sdf.parse(ich);
            Date d_du = sdf.parse(du);
            ret = d_du.compareTo(d_ich);
        } catch (ParseException ex) {
            Log.fehlerMeldung(936542876,Log.FEHLER_ART_PROG, this.getClass().getName(), ex);
        }
        return ret;
    }

    public boolean aelterAls(int tage) {
        boolean ret = false;
        try {
            //31.10.2010	16:54:17
            String ich = arr[FilmlistenSuchen.FILM_UPDATE_SERVER_DATUM_NR] + ", " + arr[FilmlistenSuchen.FILM_UPDATE_SERVER_ZEIT_NR];
            Date d_ich = sdf.parse(ich);
            Calendar cal = Calendar.getInstance();
            // tage vom calendar abziehen
            cal.add(Calendar.DATE, -tage);
            ret = d_ich.before(cal.getTime());
        } catch (ParseException ex) {
            Log.fehlerMeldung(915468973, Log.FEHLER_ART_PROG,this.getClass().getName(), ex);
        }
        return ret;
    }

    private void makeArr() {
        arr = new String[FilmlistenSuchen.FILM_UPDATE_SERVER_MAX_ELEM];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = "";
        }
    }
}
