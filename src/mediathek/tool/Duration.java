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

public class Duration {

    private Date startZeit = new Date(System.currentTimeMillis());
    private Date stopZeit = new Date(System.currentTimeMillis());
    private final static SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
    private int sekunden;
    private int count = 0;
    private String TEXT = "";

    public Duration(String t) {
        TEXT = t;
        start("");
    }

    public void ping(String text) {
        stop(TEXT + " #  " + text + " " + count++);
        startZeit = new Date(System.currentTimeMillis());
    }

    public final void start(String text) {
        startZeit = new Date(System.currentTimeMillis());
        if (!text.isEmpty()) {
            System.out.println(" Start: " + text);
        }
    }

    public void stop(String text) {
        stopZeit = new Date(System.currentTimeMillis());
        try {
            sekunden = Math.round(stopZeit.getTime() - startZeit.getTime());
        } catch (Exception ex) {
            sekunden = -1;
        }
        System.out.println(" " + text + " [ms]: " + sekunden);
    }
}
