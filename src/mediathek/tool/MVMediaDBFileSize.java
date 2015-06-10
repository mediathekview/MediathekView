/*
 * MediathekView
 * Copyright (C) 2014 W. Xaver
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

public class MVMediaDBFileSize implements Comparable<MVMediaDBFileSize> {

    public Long sizeL = 0L;
    private String sizeStr = "";

    public MVMediaDBFileSize(long size) {
        sizeL = size;
        sizeStr = setGroesse(size);
    }

    @Override
    public int compareTo(MVMediaDBFileSize ll) {
        return (sizeL.compareTo(ll.sizeL));
    }

    @Override
    public String toString() {
        return sizeStr;
    }

    private String setGroesse(long l) {
        // l: Anzahl Bytes
        String ret = "";
        if (l > 1000 * 1000) {
            // größer als 1MB sonst kann ich mirs sparen
            ret = String.valueOf(l / (1000 * 1000));
        } else if (l > 0) {
            //0<....<1M
            ret = "< 1";
        } else if (l == 0) {
            ret = "0";
        }
        return ret;
    }
}
