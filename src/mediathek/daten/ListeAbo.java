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

import java.util.Collections;
import java.util.LinkedList;
import java.util.ListIterator;
import javax.swing.JOptionPane;
import mediathek.tool.ListenerMediathekView;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.tool.DatumZeit;
import mediathek.tool.Filter;
import mediathek.tool.TModelAbo;

public class ListeAbo extends LinkedList<DatenAbo> {

    DDaten daten;

    public ListeAbo(DDaten ddaten) {
        daten = ddaten;
    }
    private int nr = 0;

    public boolean addAbo(String filmSender, String filmThema, String filmTitel) {
        return addAbo(filmSender, filmThema, filmTitel, "", filmThema);

    }

    public boolean addAbo(String filmSender, String filmThema, String filmTitel, String filmThemaTitel, String namePfad) {
        //abo anlegen, oder false wenns schon existiert
        boolean ret = false;
        DatenAbo datenAbo = new DatenAbo(namePfad /* name */, filmSender, filmThema, filmTitel, filmThemaTitel, namePfad, "");
        DialogEditAbo dialogEditAbo = new DialogEditAbo(null, true, daten, datenAbo);
        dialogEditAbo.setVisible(true);
        if (dialogEditAbo.ok) {
            if (getAbo(filmSender, filmThema, filmTitel) == null) {
                addAbo(datenAbo);
                sort();
                ret = true;
            } else {
                JOptionPane.showMessageDialog(null, "Abo existiert bereits", "Abo anlegen", JOptionPane.INFORMATION_MESSAGE);
            }
        }
        return ret;
    }

    public void aboLoeschen(DatenAbo abo) {
        if (abo != null) {
            this.remove(abo);
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_ABOS, ListeAbo.class.getSimpleName());
        }
    }

    public void addAbo(DatenAbo datenAbo) {
        String str = String.valueOf(nr++);
        while (str.length() < 3) {
            str = "0" + str;
        }
        datenAbo.arr[DatenAbo.ABO_NR_NR] = str;
        super.add(datenAbo);
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_ABOS, ListeAbo.class.getSimpleName());
    }

    public DatenAbo getAboNr(int i) {
        return this.get(i);
    }

    public void sort() {
        Collections.<DatenAbo>sort(this);
    }

    public void addObjectData(TModelAbo model) {
        Object[] object;
        DatenAbo datenAbo;
        model.setRowCount(0);
        ListIterator<DatenAbo> iterator = this.listIterator();
        object = new Object[DatenAbo.ABO_MAX_ELEM];
        while (iterator.hasNext()) {
            datenAbo = iterator.next();
            //object[i] = datenAbo.arr;
            for (int m = 0; m < DatenAbo.ABO_MAX_ELEM; ++m) {
                if (m == DatenAbo.ABO_DOWN_DATUM_NR) {
                    object[m] = DatumZeit.getDatumForObject(datenAbo.arr[DatenAbo.ABO_DOWN_DATUM_NR]);
                } else if (m == DatenAbo.ABO_EINGESCHALTET_NR) {
                    object[m] = ""; //Boolean.valueOf(datenAbo.aboIstEingeschaltet());
                } else {
                    object[m] = datenAbo.arr[m];
                }
            }
            model.addRow(object);
        }
    }

    public DatenAbo getAbo(String filmSender, String filmThema, String FilmTitel) {
        DatenAbo datenAbo;
        ListIterator<DatenAbo> it = this.listIterator();
        while (it.hasNext()) {
            datenAbo = it.next();
            if (Filter.filterAufAboPruefen(datenAbo.arr[DatenAbo.ABO_SENDER_NR], datenAbo.arr[DatenAbo.ABO_THEMA_NR], datenAbo.arr[DatenAbo.ABO_TITEL_NR],
                    datenAbo.arr[DatenAbo.ABO_THEMA_TITEL_NR], filmSender, filmThema, FilmTitel)) {
                return datenAbo;
            }
        }
        return null;
    }

}
