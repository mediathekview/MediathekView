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
package mSearch.tool;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

@SuppressWarnings("serial")
public class DatumFilm extends Datum {
    // die Filme werden immer in der Zeitzone "Europe/Berlin" gesucht

    private final static SDF dateFormatter1 = new SDF("dd.MM.yyyy");
    private final static SDF dateFormatter2 = new SDF("yyyy.MM.dd");

    public DatumFilm(long l) {
        super(l);
    }

    @Override
    public String toString() {
        if (this.getTime() == 0) {
            return "";
        } else {
            return dateFormatter1.format(this);
        }
    }

    @Override
    public String toStringR() {
        if (this.getTime() == 0) {
            return dateFormatter2.format(new Date());
        } else {
            return dateFormatter2.format(this);
        }
    }

    private static class SDF extends SimpleDateFormat {
        private final static TimeZone tz = TimeZone.getTimeZone("Europe/Berlin");

        SDF(String str) {
            super(str);
            this.setTimeZone(tz);
        }
    }
}
