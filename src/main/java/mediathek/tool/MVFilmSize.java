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

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

public class MVFilmSize implements Comparable<MVFilmSize> {

    private long aktSizeL = -1L;
    private Long sizeL = 0L;
    private static final Logger logger = LogManager.getLogger(MVFilmSize.class);

    public MVFilmSize() {
    }

    public static String getGroesse(long l) {
        String ret = "";
        if (l > 1_000_000) {
            // größer als 1MB sonst kann ich mirs sparen
            ret = String.valueOf(l / 1_000_000);
        } else if (l > 0) {
            ret = "1";
        }
        return ret;
    }

    @Override
    public int compareTo(@NotNull MVFilmSize ll) {
        return (sizeL.compareTo(ll.sizeL));
    }

    @Override
    public String toString() {
        return prepareString();
    }

    public void reset() {
        aktSizeL = -1L;
    }

    public long getSize() {
        return sizeL;
    }

    public void setSize(String size) {
        // im Film ist die Größe in "MB" !!
        if (size.isEmpty()) {
            aktSizeL = -1L;
            sizeL = 0L;
        } else {
            try {
                sizeL = Long.valueOf(size) * 1_000_000;
            } catch (Exception ex) {
                logger.error("string: {}, ex: {}", size, ex);
                sizeL = 0L;
            }
        }
    }

    public void setSize(long l) {
        sizeL = l;
    }

    public void addAktSize(long l) {
        aktSizeL += l;
    }

    public long getAktSize() {
        return aktSizeL;
    }

    public void setAktSize(long l) {
        aktSizeL = l;
    }

    private String prepareString() {
        String sizeStr;

        if (aktSizeL <= 0) {
            if (sizeL > 0) {
                sizeStr = getGroesse(sizeL);
            } else {
                sizeStr = "";
            }
        } else if (sizeL > 0) {
            sizeStr = getGroesse(aktSizeL) + " von " + getGroesse(sizeL);
        } else {
            sizeStr = getGroesse(aktSizeL);
        }

        return sizeStr;
    }
}
