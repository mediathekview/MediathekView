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

import mediathek.tool.Log;

public class DatenFilmlistenServer implements Comparable<DatenFilmlistenServer> {

    public static final String FILM_LISTEN_SERVER = "film-listen-server";
    public static final int FILM_LISTEN_SERVER_MAX_ELEM = 3;
    public static final String FILM_LISTEN_SERVER_NR = "film-listen-server-nr";
    public static final int FILM_LISTEN_SERVER_NR_NR = 0;
    public static final String FILM_LISTEN_SERVER_URL = "film-listen-server-url";
    public static final int FILM_LISTEN_SERVER_URL_NR = 1;
    public static final String FILM_LISTEN_SERVER_ALTER = "film-listen-server-alter";
    public static final int FILM_LISTEN_SERVER_ALTER_NR = 2;
    public static final String[] FILM_LISTEN_SERVER_COLUMN_NAMES = {FILM_LISTEN_SERVER_NR, FILM_LISTEN_SERVER_URL, FILM_LISTEN_SERVER_ALTER};
    public static final String[] FILM_LISTEN_SERVER_COLUMN_NAMES_ANZEIGE = {"Nr", "Url", "Alter"};
    public String[] arr;

    public DatenFilmlistenServer() {
        makeArr();
    }

    public DatenFilmlistenServer(String url) {
        makeArr();
        arr[FILM_LISTEN_SERVER_URL_NR] = url;
    }

    @Override
    public int compareTo(DatenFilmlistenServer arg0) {
        return arr[FILM_LISTEN_SERVER_URL_NR].compareTo(arg0.arr[FILM_LISTEN_SERVER_URL_NR]);
    }

    public void incAlter() {
        try {
            int alter = Integer.parseInt(arr[FILM_LISTEN_SERVER_ALTER_NR]);
            arr[FILM_LISTEN_SERVER_ALTER_NR] = String.valueOf(++alter);
        } catch (Exception ex) {
            Log.fehlerMeldung(689412357, this.getClass().getName(), ex, arr[FILM_LISTEN_SERVER_ALTER_NR]);
            arr[FILM_LISTEN_SERVER_ALTER_NR] = "0";
        }
    }

    public int getAlter() {
        int alter = 0;
        try {
            alter = Integer.parseInt(arr[FILM_LISTEN_SERVER_ALTER_NR]);
        } catch (Exception ex) {
            Log.fehlerMeldung(689412357, this.getClass().getName(), ex, arr[FILM_LISTEN_SERVER_ALTER_NR]);
            alter = 0;
            arr[FILM_LISTEN_SERVER_ALTER_NR] = "0";
        }
        return alter;
    }

    private void makeArr() {
        arr = new String[FILM_LISTEN_SERVER_MAX_ELEM];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = "";
        }
        arr[FILM_LISTEN_SERVER_ALTER_NR] = "0";
    }
}
