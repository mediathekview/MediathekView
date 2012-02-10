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
package mediathek.daten;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.ListIterator;
import javax.swing.event.EventListenerList;
import mediathek.controller.filme.filmeImportieren.MediathekListener;
import mediathek.tool.TModel;

public class ListePgruppe extends LinkedList<DatenPgruppe> {

    static EventListenerList listeners = new EventListenerList();

    public static void addAdListener(MediathekListener listener) {
        listeners.add(MediathekListener.class, listener);
    }

    public static void notifyMediathekListener() {
        for (MediathekListener l : listeners.getListeners(MediathekListener.class)) {
            l.ping(ListePgruppe.class.getSimpleName());
        }
    }

//    public ListePgruppe getCopyOfMe() {
//        //Kopie dieser Liste zurückgeben
//        ListePgruppe ret = new ListePgruppe();
//        Iterator<DatenPgruppe> it = this.iterator();
//        while (it.hasNext()) {
//            ret.add(it.next().copy());
//        }
//        return ret;
//    }
    public boolean nameExists(String name) {
        boolean ret = false;
        ListIterator<DatenPgruppe> it = this.listIterator(0);
        while (it.hasNext()) {
            if (it.next().arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR].equals(name)) {
                ret = true;
            }
        }
        return ret;
    }

    public DatenPgruppe getPgruppeAbspielen() {
        //Programmgruppe zum Abspielen
        Iterator<DatenPgruppe> it = this.iterator();
        while (it.hasNext()) {
            DatenPgruppe datenPgruppe = it.next();
            if (Boolean.parseBoolean(datenPgruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_ABSPIELEN_NR])) {
                return datenPgruppe;
            }
        }
        return null;
    }

    public DatenPgruppe getPgruppeSpeichern() {
        //Pgruppe zum Aufzeichnen
        ListePgruppe liste = getListeSpeichern();
        if (liste.size() == 0) {
            return null;
        } else if (liste.size() == 1) {
            return liste.getFirst();
        } else {
            /////////////////////////////
            // Dialog zum Auswählen
            return liste.getFirst();
        }
    }

    public DatenPgruppe getPgruppeAbo(String name) {
        //liefert mit dem Namen eines Abos die passende Programmgruppe zurück
        //wird nichts gefunden, wird die erste Programmgruppe (der Abos) genommen
        DatenPgruppe ret = null;
        if (this.size() == 0) {
            ret = null;
        } else if (this.size() == 1) {
            ret = this.getFirst();
        } else {
            ListIterator<DatenPgruppe> it = this.listIterator(0);
            while (it.hasNext()) {
                DatenPgruppe gruppe;
                gruppe = it.next();
                if (gruppe.istAbo()) {
                    if (gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR].equals(name)) {
                        ret = gruppe;
                    }
                }
            }
            if (ret == null) {
                // die erste Pgruppe der Abos
                ret = getListeAbo().getFirst();
                if (ret == null) {
                    // dann die erste Prgruppe
                    ret = this.getFirst();
                }
            }
        }
        return ret;
    }

    public ListePgruppe getListeSpeichern() {
        ListePgruppe liste = new ListePgruppe();
        Iterator<DatenPgruppe> it = this.iterator();
        while (it.hasNext()) {
            DatenPgruppe datenPgruppe = it.next();
            if (Boolean.parseBoolean(datenPgruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_SPEICHERN_NR])) {
                liste.add(datenPgruppe);
            }
        }
        return liste;
    }

    public ListePgruppe getListeButton() {
        ListePgruppe liste = new ListePgruppe();
        Iterator<DatenPgruppe> it = this.iterator();
        while (it.hasNext()) {
            DatenPgruppe datenPgruppe = it.next();
            if (Boolean.parseBoolean(datenPgruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_BUTTON_NR])) {
                liste.add(datenPgruppe);
            }
        }
        return liste;
    }

    public ListePgruppe getListeAbo() {
        ListePgruppe liste = new ListePgruppe();
        Iterator<DatenPgruppe> it = this.iterator();
        while (it.hasNext()) {
            DatenPgruppe datenPgruppe = it.next();
            if (Boolean.parseBoolean(datenPgruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_ABO_NR])) {
                liste.add(datenPgruppe);
            }
        }
        return liste;
    }

    public void duplicate(DatenPgruppe gruppe) {
        this.addPgruppe(gruppe.copy());
    }

    public String[] getObjectDataCombo() {
        //liefert eine Liste aller Pgruppen
        String[] object;
        int i = 0;
        ListIterator<DatenPgruppe> it = this.listIterator(0);
        object = new String[this.size()];
        while (it.hasNext()) {
            object[i] = it.next().arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR];
            ++i;
        }
        return object;
    }

    public int auf(int idx, boolean auf) {
        DatenPgruppe prog = this.remove(idx);
        int neu = idx;
        if (auf) {
            if (neu > 0) {
                --neu;
            }
        } else {
            if (neu < this.size()) {
                ++neu;
            }
        }
        this.add(neu, prog);
        notifyMediathekListener();
        return neu;
    }

    public boolean addPgruppe(DatenPgruppe gruppe) {
        boolean ret = add(gruppe);
        notifyMediathekListener();
        return ret;
    }

    public TModel getModel() {
        TModel model;
        Object[][] object;
        DatenPgruppe daten;
        int i = 0;
        if (this.size() > 0) {
            ListIterator<DatenPgruppe> iterator = this.listIterator(0);
            object = new Object[this.size()][DatenPgruppe.PROGRAMMGRUPPE_MAX_ELEM];
            while (iterator.hasNext()) {
                daten = iterator.next();
                object[i][DatenPgruppe.PROGRAMMGRUPPE_NAME_NR] = daten.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR];
                ++i;
            }
            model = new TModel(object, DatenPgruppe.PROGRAMMGRUPPE_COLUMN_NAMES);
        } else {
            model = new TModel(new Object[0][DatenPgruppe.PROGRAMMGRUPPE_MAX_ELEM], DatenPgruppe.PROGRAMMGRUPPE_COLUMN_NAMES);
        }
        return model;
    }
}
