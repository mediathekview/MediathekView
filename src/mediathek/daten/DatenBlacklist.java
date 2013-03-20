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

public class DatenBlacklist {

    public static final String BLACKLIST = "Blacklist";
    public static final String BLACKLIST_NR = "black-nr";
    public static final int BLACKLIST_NR_NR = 0;
    public static final String BLACKLIST_SENDER = "black-sender";
    public static final int BLACKLIST_SENDER_NR = 1;
    public static final String BLACKLIST_THEMA = "black-thema";
    public static final int BLACKLIST_THEMA_NR = 2;
    public static final String BLACKLIST_TITEL = "black-titel";
    public static final int BLACKLIST_TITEL_NR = 3;
    public static final String BLACKLIST_THEMA_TITEL = "black-thema-titel";
    public static final int BLACKLIST_THEMA_TITEL_NR = 4;
    public static final int BLACKLIST_MAX_ELEM = 5;
    public static final String[] BLACKLIST_COLUMN_NAMES = {BLACKLIST_NR, BLACKLIST_SENDER, BLACKLIST_THEMA, BLACKLIST_TITEL, BLACKLIST_THEMA_TITEL};
    public static final String[] BLACKLIST_COLUMN_NAMES_ANZEIGE = {"Nr", "Sender", "Thema", "Titel", "Thema-Titel"};
    public String[] arr;

    public DatenBlacklist() {
        makeArr();
    }

    public DatenBlacklist(String sender, String thema, String titel, String themaTitel) {
        makeArr();
        arr[BLACKLIST_NR_NR] = "";
        arr[BLACKLIST_SENDER_NR] = sender;
        arr[BLACKLIST_THEMA_NR] = thema;
        arr[BLACKLIST_TITEL_NR] = titel;
        arr[BLACKLIST_THEMA_TITEL_NR] = themaTitel;
    }

    private void makeArr() {
        arr = new String[BLACKLIST_MAX_ELEM];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = "";
        }
    }
}
