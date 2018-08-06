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

@SuppressWarnings("serial")
public class Datum extends Date {
    private final static SimpleDateFormat dateFormatter1 = new SimpleDateFormat("dd.MM.yyyy");
    private final static SimpleDateFormat dateFormatter2 = new SimpleDateFormat("yyyy.MM.dd");

    public Datum() {
        super();
    }

    public Datum(long l) {
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

    public String toStringR() {
        if (this.getTime() == 0) {
            return dateFormatter2.format(new Date());
        } else {
            return dateFormatter2.format(this);
        }
    }

    /**
     * Liefert den Betrag der Zeitdifferenz zu jetzt.
     *
     * @return Differenz in Sekunden.
     */
    public int diffInSekunden() {
        final int ret = new Long((this.getTime() - new Datum().getTime()) / (1000)).intValue();
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
