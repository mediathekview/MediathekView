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
package mediathek.controller.io.starter;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.ListIterator;
import mediathek.daten.DDaten;
import mediathek.daten.DatenDownload;
import mediathek.tool.TModel;

public class ListeStarts extends LinkedList<Start> {

    DDaten daten;

    public ListeStarts(DDaten d) {
        super();
        daten = d;
    }

    public synchronized Iterator<Start> getIt() {
        return super.iterator();
    }

    boolean contain(Start start) {
        boolean ret = false;
        ListIterator<Start> it = this.listIterator(0);
        while (it.hasNext()) {
            Start s = it.next();
            if (s.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR].equals(start.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR])) {
                ret = true;
                break;
            }
        }
        return ret;
    }

    void delStart(String url) {
        ListIterator<Start> it = this.listIterator(0);
        while (it.hasNext()) {
            Start s = it.next();
            if (s.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR].equals(url)) {
                s.stoppen = true;
                it.remove();
                break;
            }
        }
    }

    void delStart() {
        ListIterator<Start> it = this.listIterator(0);
        while (it.hasNext()) {
            Start s = it.next();
            s.stoppen = true;
            it.remove();
            break;
        }
    }

    int getDown() {
        int ret = 0;
        ListIterator<Start> it = this.listIterator(0);
        while (it.hasNext()) {
            Start s = it.next();
            if (s.status == Start.STATUS_RUN) {
                ++ret;
            }
        }
        return ret;
    }

    void aufraeumen() {
        Iterator<Start> it = this.iterator();
        while (it.hasNext()) {
            Start start = it.next();
            if (start.status >= Start.STATUS_FERTIG) {
                it.remove();
            }
        }
    }

    int delRest() {
        //löscht alle Starts die noch nicht laufen
        int ret = 0;
        ListIterator<Start> it = this.listIterator(0);
        while (it.hasNext()) {
            if (it.next().status < Start.STATUS_RUN) {
                it.remove();
            }
        }
        return ret;
    }

    int getmax() {
        // liefert die Listengröße wenn noch nicht alle fertig
        // sonst wenn alle fertig: 0
        ListIterator<Start> it = this.listIterator(0);
        while (it.hasNext()) {
            if (it.next().status < Start.STATUS_FERTIG) {
                return this.size();
            }
        }
        return 0;
    }

    TModel getModel(TModel model) {
        model.setRowCount(0);
        Object[] object;
        Start start;
        if (this.size() > 0) {
            Iterator<Start> iterator = this.getIt();
            int objLen = DatenDownload.DOWNLOAD_MAX_ELEM + 1;
            object = new Object[objLen];
            while (iterator.hasNext()) {
                start = iterator.next();
                for (int k = 0; k < objLen; ++k) {
                    if (k < DatenDownload.DOWNLOAD_MAX_ELEM) {
                        object[k] = start.datenDownload.arr[k];
                    } else {
                        if (start.datenDownload.istAbo()) {
                            object[k] = "Abo";
                        } else {
                            object[k] = "";
                        }
                    }
                }
                model.addRow(object);
            }
        }
        return model;
    }

    public static TModel getNewModel() {
        int max = DatenDownload.DOWNLOAD_MAX_ELEM + 1;
        String[] titel = new String[max];
        for (int i = 0; i < max; ++i) {
            if (i < DatenDownload.DOWNLOAD_MAX_ELEM) {
                titel[i] = DatenDownload.DOWNLOAD_COLUMN_NAMES[i];
            } else {
                titel[i] = "Art";
            }
        }
        TModel model = new TModel(new Object[][]{}, titel);
        return model;
    }
}
