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
package mediathek.tool;

public class MVStringBuilder {

    private StringBuilder cont;
    private int pos1 = 0, pos2 = 0, pos3 = 0;

    public MVStringBuilder() {
        cont = new StringBuilder();
    }

    public MVStringBuilder(int capacity) {
        cont = new StringBuilder(capacity);
    }

    public String substring(int start) {
        return cont.substring(start);
    }

    public int lastIndexOf(String of) {
        return cont.lastIndexOf(of);
    }

    public int length() {
        return cont.length();
    }

    public String substring(int start, int end) {
        return cont.substring(start, end);
    }

    public synchronized void append(char[] str) {
        cont.append(str);
    }

    public synchronized void setLength(int newLength) {
        cont.setLength(newLength);
    }

    public synchronized int indexOf(String str, int fromIndex) {
        return cont.indexOf(str, fromIndex);
    }

    public synchronized int indexOf(String str) {
        return cont.indexOf(str);
    }

    public String extract(String musterStart, String musterEnde) {
        return extract(musterStart, musterEnde, 0);
    }

    public String extract(String musterStart, String musterEnde, int abPos) {
        if ((pos1 = cont.indexOf(musterStart, abPos)) != -1) {
            pos1 += musterStart.length();
            if ((pos2 = cont.indexOf(musterEnde, pos1)) != -1) {
                return cont.substring(pos1, pos2);
            }
        }
        return "";
    }

    public String extract(String musterStart1, String musterStart2, String musterEnde) {
        return extract(musterStart1, musterStart2, musterEnde, 0);
    }

    public String extract(String musterStart1, String musterStart2, String musterEnde, int abPos) {
        if ((pos1 = cont.indexOf(musterStart1, abPos)) != -1) {
            pos1 += musterStart1.length();
            if ((pos1 = cont.indexOf(musterStart2, pos1)) != -1) {
                pos1 += musterStart2.length();
                if ((pos2 = cont.indexOf(musterEnde, pos1)) != -1) {
                    return cont.substring(pos1, pos2);
                }
            }
        }
        return "";
    }
}
