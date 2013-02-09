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
            return new SimpleDateFormat("dd.MM.yyyy").format(this);
        }
    }

    public String toStringR() {
        if (this.getTime() == 0) {
            return new SimpleDateFormat("yyyy.MM.dd").format(new Date());
        } else {
            return new SimpleDateFormat("yyyy.MM.dd").format(this);
        }
    }

    public long diffInSekunden() {
        // liefert den BETRAG! der Zeitdifferenz zu jetzt
        long ret = 0;
        ret = this.getTime() - new Datum().getTime();
        ret = ret / (1000); // Sekunden
        if (ret < 0) {
            ret = -1 * ret;
        }
        return ret;
    }
}
