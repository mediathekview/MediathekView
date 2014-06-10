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
package mediathek.tool;

import java.text.SimpleDateFormat;
import java.util.Date;

public class Datum extends Date {

    final static SimpleDateFormat sdf1 = new SimpleDateFormat("dd.MM.yyyy");
    final static SimpleDateFormat sdf2 = new SimpleDateFormat("yyyy.MM.dd");

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
            return sdf1.format(this);
        }
    }

    public String toStringR() {
        if (this.getTime() == 0) {
            return sdf2.format(new Date());
        } else {
            return sdf2.format(this);
        }
    }

    public int diffInSekunden() {
        // liefert den BETRAG! der Zeitdifferenz zu jetzt
        int ret;
        ret = new Long((this.getTime() - new Datum().getTime()) / (1000)).intValue(); // Zeitdifferenz in Sekunden
        if (ret < 0) {
            ret = -1 * ret;
        }
        return ret;
    }

    public int diffInMinuten() {
        // liefert den BETRAG! der Zeitdifferenz zu jetzt
        int ret = this.diffInSekunden();
        ret = ret / 60; // Minuten
        return ret;
    }
}
