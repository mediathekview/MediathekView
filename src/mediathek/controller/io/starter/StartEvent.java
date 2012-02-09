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

package mediathek.controller.io.starter;

import java.util.EventObject;

public class StartEvent extends EventObject {
    // meldet eine Änderung an den "Starts" in der Liste
    // der zu Startenden Downloads
    private int downloads;
    private int progress;
    private int max;
    private boolean allesStop;

    public StartEvent(Object source, int down, int pprogress, int m, boolean aallesStop) {
        super(source);
        downloads = down;
        progress = pprogress;
        max = m;
        allesStop = aallesStop;
    }

    public int getDown() {
        return downloads;
    }

    public int getProgress() {
        return progress;
    }

    public int getMax() {
        return max;
    }

    public boolean nixTun() {
        return max == 0;
    }

    public boolean allesStop() {
        return allesStop;
    }

}
