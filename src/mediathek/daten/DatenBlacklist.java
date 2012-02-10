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
    public static final int BLACKLIST_MAX_ELEM = 2;
    public static final String BLACKLIST_SENDER = "black-sender";
    public static final int BLACKLIST_SENDER_NR = 0;
    public static final String BLACKLIST_THEMA = "black-thema";
    public static final int BLACKLIST_THEMA_NR = 1;
    public static final String[] BLACKLIST_COLUMN_NAMES = {BLACKLIST_SENDER, BLACKLIST_THEMA};
    public String[] arr;

    public DatenBlacklist() {
        makeArr();
    }

    public DatenBlacklist(String sender, String thema) {
        makeArr();
        arr[BLACKLIST_SENDER_NR] = sender;
        arr[BLACKLIST_THEMA_NR] = thema;
    }

    private void makeArr() {
        arr = new String[BLACKLIST_MAX_ELEM];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = "";
        }
    }
}
