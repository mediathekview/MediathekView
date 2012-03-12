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
import mediathek.Daten;
import mediathek.controller.filme.filmeImportieren.MediathekListener;
import mediathek.tool.TModel;

public class ListePset extends LinkedList<DatenPset> {

    public boolean nameExists(String name) {
        boolean ret = false;
        ListIterator<DatenPset> it = this.listIterator(0);
        while (it.hasNext()) {
            if (it.next().arr[DatenPset.PROGRAMMSET_NAME_NR].equals(name)) {
                ret = true;
            }
        }
        return ret;
    }

    public DatenPset getPsetAbspielen() {
        //Programmgruppe zum Abspielen
        Iterator<DatenPset> it = this.iterator();
        while (it.hasNext()) {
            DatenPset datenPset = it.next();
            if (Boolean.parseBoolean(datenPset.arr[DatenPset.PROGRAMMSET_IST_ABSPIELEN_NR])) {
                return datenPset;
            }
        }
        return null;
    }

    public DatenPset getPsetAbo(String name) {
        //liefert mit dem Namen eines Abos die passende Programmgruppe zurück
        //wird nichts gefunden, wird die erste Programmgruppe (der Abos) genommen
        DatenPset ret = null;
        if (this.size() == 0) {
            ret = null;
        } else if (this.size() == 1) {
            ret = this.getFirst();
        } else {
            ListIterator<DatenPset> it = this.listIterator(0);
            while (it.hasNext()) {
                DatenPset gruppe;
                gruppe = it.next();
                if (gruppe.istAbo()) {
                    if (gruppe.arr[DatenPset.PROGRAMMSET_NAME_NR].equals(name)) {
                        ret = gruppe;
                    }
                }
            }
            if (ret == null) {
                // die erste Pset der Abos
                ret = getListeAbo().getFirst();
                if (ret == null) {
                    // dann die erste Prgruppe
                    ret = this.getFirst();
                }
            }
        }
        return ret;
    }

    public ListePset getListeSpeichern() {
        ListePset liste = new ListePset();
        Iterator<DatenPset> it = this.iterator();
        while (it.hasNext()) {
            DatenPset datenPset = it.next();
            if (Boolean.parseBoolean(datenPset.arr[DatenPset.PROGRAMMSET_IST_SPEICHERN_NR])) {
                liste.add(datenPset);
            }
        }
        return liste;
    }

    public ListePset getListeButton() {
        ListePset liste = new ListePset();
        Iterator<DatenPset> it = this.iterator();
        while (it.hasNext()) {
            DatenPset datenPset = it.next();
            if (Boolean.parseBoolean(datenPset.arr[DatenPset.PROGRAMMSET_IST_BUTTON_NR])) {
                liste.add(datenPset);
            }
        }
        return liste;
    }

    public ListePset getListeAbo() {
        ListePset liste = new ListePset();
        Iterator<DatenPset> it = this.iterator();
        while (it.hasNext()) {
            DatenPset datenPset = it.next();
            if (Boolean.parseBoolean(datenPset.arr[DatenPset.PROGRAMMSET_IST_ABO_NR])) {
                liste.add(datenPset);
            }
        }
        return liste;
    }

    public String[] getObjectDataCombo() {
        //liefert eine Liste aller Pset
        String[] object;
        int i = 0;
        ListIterator<DatenPset> it = this.listIterator(0);
        object = new String[this.size()];
        while (it.hasNext()) {
            object[i] = it.next().arr[DatenPset.PROGRAMMSET_NAME_NR];
            ++i;
        }
        return object;
    }

    public int auf(int idx, boolean auf) {
        DatenPset prog = this.remove(idx);
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
        Daten.notifyMediathekListener(MediathekListener.EREIGNIS_LISTE_PSET, ListePset.class.getSimpleName());
        return neu;
    }

    public boolean addPset(DatenPset gruppe) {
        boolean abspielen = false;
        Iterator<DatenPset> it = this.iterator();
        while (it.hasNext()) {
            if (Boolean.parseBoolean(it.next().arr[DatenPset.PROGRAMMSET_IST_ABSPIELEN_NR])) {
                abspielen = true;
                break;
            }
        }
        if (abspielen) {
            gruppe.arr[DatenPset.PROGRAMMSET_IST_ABSPIELEN_NR] = Boolean.FALSE.toString();
        }
        boolean ret = add(gruppe);
        return ret;
    }

    public boolean addPset(DatenPset[] gruppe) {
        boolean ret = true;
        for (int i = 0; i < gruppe.length; ++i) {
            if (!addPset(gruppe[i])) {
                ret = false;
            }
        }
        return ret;
    }

    public TModel getModel() {
        TModel model;
        Object[][] object;
        DatenPset daten;
        int i = 0;
        if (this.size() > 0) {
            ListIterator<DatenPset> iterator = this.listIterator(0);
            object = new Object[this.size()][DatenPset.PROGRAMMSET_MAX_ELEM];
            while (iterator.hasNext()) {
                daten = iterator.next();
                //////////////////??????
                object[i][DatenPset.PROGRAMMSET_NAME_NR] = daten.arr[DatenPset.PROGRAMMSET_NAME_NR];
                ++i;
            }
            model = new TModel(object, DatenPset.PROGRAMMSET_COLUMN_NAMES);
        } else {
            model = new TModel(new Object[0][DatenPset.PROGRAMMSET_MAX_ELEM], DatenPset.PROGRAMMSET_COLUMN_NAMES);
        }
        return model;
    }

}
