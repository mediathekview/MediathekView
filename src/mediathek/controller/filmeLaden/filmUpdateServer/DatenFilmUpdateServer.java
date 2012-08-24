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
package mediathek.controller.filmeLaden.filmUpdateServer;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import mediathek.tool.Log;

public class DatenFilmUpdateServer implements Comparable<DatenFilmUpdateServer> {

    public String[] arr;
    public static SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy, HH:mm:ss");

    public DatenFilmUpdateServer() {
        makeArr();
    }

    public DatenFilmUpdateServer(String url, String prio) {
        makeArr();
        arr[FilmUpdateServer.FILM_UPDATE_SERVER_URL_NR] = url;
        arr[FilmUpdateServer.FILM_UPDATE_SERVER_PRIO_NR] = prio;
    }

    public DatenFilmUpdateServer(String url, String prio, String zeit, String datum, String anzahl) {
        makeArr();
        arr[FilmUpdateServer.FILM_UPDATE_SERVER_URL_NR] = url;
        arr[FilmUpdateServer.FILM_UPDATE_SERVER_PRIO_NR] = prio;
        arr[FilmUpdateServer.FILM_UPDATE_SERVER_DATUM_NR] = datum;
        arr[FilmUpdateServer.FILM_UPDATE_SERVER_ZEIT_NR] = zeit;
        arr[FilmUpdateServer.FILM_UPDATE_SERVER_ANZAHL_NR] = anzahl;
    }

    public DatenFilmUpdateServer getCopy() {
        DatenFilmUpdateServer ret = new DatenFilmUpdateServer(new String(arr[FilmUpdateServer.FILM_UPDATE_SERVER_URL_NR]), new String(arr[FilmUpdateServer.FILM_UPDATE_SERVER_PRIO_NR]));
        return ret;
    }

    @Override
    public int compareTo(DatenFilmUpdateServer arg0) {
        int ret = 0;
        try {
            //31.10.2010	16:54:17
            String ich = arr[FilmUpdateServer.FILM_UPDATE_SERVER_DATUM_NR] + ", " + arr[FilmUpdateServer.FILM_UPDATE_SERVER_ZEIT_NR];
            String du = arg0.arr[FilmUpdateServer.FILM_UPDATE_SERVER_DATUM_NR] + ", " + arg0.arr[FilmUpdateServer.FILM_UPDATE_SERVER_ZEIT_NR];
            if (ich.equals(du)) {
                return 0;
            }
            Date d_ich = sdf.parse(ich);
            Date d_du = sdf.parse(du);
            ret = d_du.compareTo(d_ich);
        } catch (ParseException ex) {
            Log.fehlerMeldung(936542876,this.getClass().getName(), ex);
        }
        return ret;
    }

    public boolean aelterAls(int tage) {
        boolean ret = false;
        try {
            //31.10.2010	16:54:17
            String ich = arr[FilmUpdateServer.FILM_UPDATE_SERVER_DATUM_NR] + ", " + arr[FilmUpdateServer.FILM_UPDATE_SERVER_ZEIT_NR];
            Date d_ich = sdf.parse(ich);
            Calendar cal = Calendar.getInstance();
            // tage vom calendar abziehen
            cal.add(Calendar.DATE, -tage);
            ret = d_ich.before(cal.getTime());
        } catch (ParseException ex) {
            Log.fehlerMeldung(915468973,this.getClass().getName(), ex);
        }
        return ret;
    }

    private void makeArr() {
        arr = new String[FilmUpdateServer.FILM_UPDATE_SERVER_MAX_ELEM];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = "";
        }
    }
}
