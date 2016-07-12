/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
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
package mediathek.daten;

public class DatenBlacklist extends MVData<DatenBlacklist> {

    public static final int BLACKLIST_NR = 0;
    public static final int BLACKLIST_SENDER = 1;
    public static final int BLACKLIST_THEMA = 2;
    public static final int BLACKLIST_TITEL = 3;
    public static final int BLACKLIST_THEMA_TITEL = 4;

    public static final int MAX_ELEM = 5;
    public static final String TAG = "Blacklist";
    public static final String[] COLUMN_NAMES = {"Nr", "Sender", "Thema", "Titel", "Thema-Titel"};
    public static final String[] XML_NAMES = {"black-nr", "black-sender", "black-thema", "black-titel", "black-thema-titel"};

    public String[] arr;

    public DatenBlacklist() {
        makeArr();
    }

    public DatenBlacklist(String sender, String thema, String titel, String themaTitel) {
        makeArr();
        arr[BLACKLIST_NR] = "";
        arr[BLACKLIST_SENDER] = sender;
        arr[BLACKLIST_THEMA] = thema;
        arr[BLACKLIST_TITEL] = titel;
        arr[BLACKLIST_THEMA_TITEL] = themaTitel;
    }

    private void makeArr() {
        arr = new String[MAX_ELEM];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = "";
        }
    }
}
