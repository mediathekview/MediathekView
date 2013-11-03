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
package mediathek.controller.starter;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.ListIterator;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.TModel;

public class ListeStarts extends LinkedList<Start> {

    Daten ddaten;

    public ListeStarts(Daten d) {
        super();
        ddaten = d;
    }




    synchronized Start urlTauschen(String url) {
        // Starts mit der URL wird vorgezogen und startet als nächster
        Start s = null;
        Iterator<Start> it = iterator();
        while (it.hasNext()) {
            s = it.next();
            if (s.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR].equals(url)) {
                if (s.status < Start.STATUS_RUN) {
                    // sonst bringts nichts mehr
                    it.remove();
                    addFirst(s);
                }
                break;
            }
        }
        return s;
    }






    public static TModel getEmptyModel() {
        int max = DatenDownload.MAX_ELEM + 1;
        String[] titel = new String[max];
        for (int i = 0; i < max; ++i) {
            if (i < DatenDownload.MAX_ELEM) {
                titel[i] = DatenDownload.COLUMN_NAMES[i];
            } else {
                titel[i] = "Art";
            }
        }
        TModel model = new TModel(new Object[][]{}, titel);
        return model;
    }

    // ***********************************
    // private
    // ***********************************

    private boolean contain(Start start) {
        boolean ret = false;
        ListIterator<Start> it = this.listIterator(0);
        while (it.hasNext()) {
            if (it.next().datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR].equals(start.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR])) {
                ret = true;
                break;
            }
        }
        return ret;
    }



    private void notifyStartEvent() {
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_START_EVENT, this.getClass().getSimpleName());
    }
}
