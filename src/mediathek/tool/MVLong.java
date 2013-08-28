/*
 *   MediathekView
 *   Copyright (C) 2013 W. Xaver
 *   W.Xaver[at]googlemail.com
 *   http://zdfmediathk.sourceforge.net/
 *
 *   This program is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.tool;

public class MVLong implements Comparable<MVLong> {

    Long l = 0L;
    String s = "";

    public MVLong(long ll) {
        l = new Long(ll);
        s = l.toString();
    }

    public MVLong(String ss) {
        try {
            if (ss.equals("<1")) {
                l = 1L;
                s = "<1";
            } else if (!ss.isEmpty()) {
                l = new Long(Long.valueOf(ss));
                s = ss;
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(649891025, Log.FEHLER_ART_MREADER, MVLong.class.getName(), ex, "String: " + ss);
        }
    }

    @Override
    public String toString() {
        return s;
    }

    @Override
    public int compareTo(MVLong ll) {
        return (l.compareTo(ll.l));
    }
}
