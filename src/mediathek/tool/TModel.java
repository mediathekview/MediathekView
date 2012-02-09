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
package mediathek.tool;

import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableModel;
import mediathek.daten.DDaten;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeFilme;

public class TModel extends DefaultTableModel {

    TableModelListener[] tmls = null;

    /** Creates a new instance of TModel */
    public TModel() {
    }
    public Object[][] object;
    Object[] columns;

    public TModel(Object[][] data, Object[] columnNames) {
        super(data, columnNames);
        object = data;
        columns = columnNames;
    }

    @Override
    public boolean isCellEditable(int i, int j) {
        return false;
    }

    public void filterModel(String str, int feld, boolean exact) {
        if (str != null) {
            if (!str.equals("")) {
                for (int i = 0; i < this.getRowCount(); ++i) {
                    String tmp;
                    tmp = this.getValueAt(i, feld).toString();
                    if (exact && tmp.length() != str.length()) {
                        this.removeRow(i);
                        --i;
                        continue;
                    }
                    if (!tmp.toLowerCase().contains(str.toLowerCase())) {
                        this.removeRow(i);
                        --i;
                    }
                }
            }
        }
    }

//    public void verkuerzen(int feld1, int feld2, int anz) {
//        //nur eine Zeile mit gleichem Inhalt aus fleld
//        int start = 0;
//        int count = 1;
//        while (start < this.getRowCount()) {
//            String str1 = this.getValueAt(start, feld1).toString();
//            String str2 = this.getValueAt(start, feld2).toString();
//            for (int i = start + 1; i < this.getRowCount(); ++i) {
//                String tmp1 = this.getValueAt(i, feld1).toString();
//                String tmp2 = this.getValueAt(i, feld2).toString();
//                if (tmp1.equals(str1) && tmp2.equals(str2)) {
//                    if (count < anz) {
//                        ++count;
//                    } else {
//                        this.removeRow(i);
//                        --i;
//                    }
//                }
//            }
//            count = 1;
//            ++start;
//        }
//    }
    public void filter(DDaten daten, boolean keineAbos, boolean kGesehen, boolean live) {
        List zeile;
        ListIterator<List> it = this.getDataVector().listIterator();
        while (it.hasNext()) {
            zeile = it.next();
            if (live) {
                if (!zeile.get(DatenFilm.FILM_THEMA_NR).equals(ListeFilme.THEMA_LIVE)) {
                    it.remove();
                    continue;
                }
            } else {
                if (!zeile.get(DatenFilm.FILM_ABO_NAME_NR).equals("")) {
                    if (keineAbos) {
                        it.remove();
                        continue;
                    }
                }
                if (kGesehen) {
                    if (daten.history.contains(zeile.get(DatenFilm.FILM_URL_NR).toString())) {
                        it.remove();
                        continue;
                    }
                }
            }


        }
    }

    public String[] getModelOfField(DDaten daten, int feld, boolean leer) {
        /* erstellt ein StringArray mit den Daten des Feldes
         * lee: immer ein leeres Feld am Anfang */
        LinkedList<String> list = new LinkedList<String>();
        String[] ret;
        String str = new String();
        for (int i = 0; i < this.getRowCount(); ++i) {
            str = this.getValueAt(i, feld).toString();
            if (str.equals("")) {
                leer = true;
            } else if (!list.contains(str)) {
                list.add(str);
            }
        }
        if (leer) {
            ret = new String[list.size() + 1];
            ret[0] = "";
            for (int i = 0; i < list.size(); ++i) {
                ret[i + 1] = list.get(i);
            }
        } else {
            ret = new String[list.size()];
            for (int i = 0; i < list.size(); ++i) {
                ret[i] = list.get(i);
            }
        }
        return ret;
    }
//    public void removeListener() {
//        tmls = (TableModelListener[]) (this.getListeners(TableModelListener.class));
//        for (int i = 0; i < tmls.length; ++i) {
//            this.removeTableModelListener(tmls[i]);
//        }
//
//    }
//
//    public void addListener() {
//        if (tmls != null) {
//            for (int i = 0; i < tmls.length; ++i) {
//                this.addTableModelListener(tmls[i]);
//            }
//            this.fireTableDataChanged();
//            tmls = null;
//        }
//    }
}
