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
import java.util.Iterator;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.regex.Pattern;
import javax.swing.JOptionPane;
import mediathek.Log;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.tool.DatumZeit;
import mediathek.tool.TModelAbo;
import org.apache.commons.lang.StringEscapeUtils;

public class ListeAbo extends LinkedList<DatenAbo> {

    DDaten daten;

    public ListeAbo(DDaten ddaten) {
        daten = ddaten;
    }
    private int nr = 0;

    public boolean addAbo(String sender, String thema, boolean exakt, String text) {
        //abo anlegen, oder false wenns schon existiert
        boolean ret = false;
        DatenAbo datenAbo = new DatenAbo(thema, sender, thema, exakt, text, thema, "");
        DialogEditAbo dialogEditAbo = new DialogEditAbo(null, true, daten, datenAbo);
        dialogEditAbo.setVisible(true);
        if (dialogEditAbo.ok) {
            if (!aboSuchen(datenAbo.arr[DatenAbo.ABO_SENDER_NR],
                    datenAbo.arr[DatenAbo.ABO_THEMA_NR],
                    //                    Boolean.parseBoolean(datenAbo.arr[DatenAbo.ABO_THEMA_EXAKT_NR]),
                    datenAbo.arr[DatenAbo.ABO_TITEL_NR])) {
                addAbo(datenAbo);
                sort();
                ret = true;
            } else {
                JOptionPane.showMessageDialog(null, "Abo existiert bereits",
                        "Abo anlegen", JOptionPane.INFORMATION_MESSAGE);
            }
        }
        return ret;
    }

    public void aboLoeschen(DatenAbo abo) {
        if (abo != null) {
            this.remove(abo);
        }
    }

    public void addAbo(DatenAbo datenAbo) {
        String str = String.valueOf(nr++);
        while (str.length() < 3) {
            str = "0" + str;
        }
        datenAbo.arr[DatenAbo.ABO_NR_NR] = str;
        //für die neue Funktion
//        if (datenAbo.arr[DatenAbo.ABO_THEMA_EXAKT_NR].equals("")) {
//            datenAbo.arr[DatenAbo.ABO_THEMA_EXAKT_NR] = Boolean.toString(true);
//        }
        super.add(datenAbo);
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
//                } else if (m == DatenAbo.ABO_THEMA_EXAKT_NR) {
//                    object[m] = "";
                } else {
                    object[m] = datenAbo.arr[m];
                }
            }
            model.addRow(object);
        }
    }

    public boolean aboSuchen(String sender, String thema, String text) {
        //Abo suchen
        boolean ret = false;
        DatenAbo abo = null;
        Iterator<DatenAbo> it = this.iterator();
        while (it.hasNext()) {
            abo = it.next();
            if (abo.arr[DatenAbo.ABO_SENDER_NR].equalsIgnoreCase(sender)
                    && abo.arr[DatenAbo.ABO_THEMA_NR].equalsIgnoreCase(thema)
                    && abo.arr[DatenAbo.ABO_TITEL_NR].equalsIgnoreCase(text)) {
                ret = true;
            }
        }
        return ret;
    }

    public boolean aboExists(String sender, String thema) {
        thema = StringEscapeUtils.unescapeHtml(thema.trim());
        boolean ret = false;
        if (getAbo(sender, thema, "", "") != null) {
            ret = true;
        }
        return ret;
    }

    public DatenAbo getAbo(String sender, String thema, String text, String url) {
        DatenAbo datenAbo = null;
        ListIterator<DatenAbo> it = this.listIterator();
        if (sender.equals("") && thema.equals("") && text.equals("") && url.equals("")) {
            Log.fehlerMeldung("ListeAbo.getAbo", "Leeres Abo!");
        } else {
            while (it.hasNext()) {
                datenAbo = it.next();
                if (ListeFilme.filterPruefen(datenAbo.arr[DatenAbo.ABO_SENDER_NR], datenAbo.arr[DatenAbo.ABO_THEMA_NR],
                        new String[]{datenAbo.arr[DatenAbo.ABO_TITEL_NR]},
                        sender, thema, text)) {
                    return datenAbo;
                }
            }

        }
        return null;
    }
}
