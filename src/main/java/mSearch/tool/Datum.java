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

import org.apache.commons.lang3.time.FastDateFormat;

import java.util.Date;

@SuppressWarnings("serial")
public class Datum extends Date {
    protected final static FastDateFormat dateFormatter1 = FastDateFormat.getInstance("dd.MM.yyyy");

    public Datum() {
        super();
    }

    public Datum(long l) {
        super(l);
    }

    @Override
    public String toString() {
        if (getTime() == 0) {
            return "";
        } else {
            return dateFormatter1.format(this);
        }
    }

    /**
     * Liefert den Betrag der Zeitdifferenz zu jetzt.
     *
     * @return Differenz in Sekunden.
     */
    public int diffInSekunden() {
        final int ret = Long.valueOf((getTime() - new Date().getTime()) / 1000).intValue();
        return Math.abs(ret);
    }

    /**
     * Liefert den BETRAG! der Zeitdifferenz zu jetzt.
     *
     * @return Differenz in Minuten.
     */
    public int diffInMinuten() {
        return (diffInSekunden() / 60);
    }
}
