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

import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableModel;
import mediathek.daten.Daten;
import msearch.daten.DatenFilm;
import msearch.daten.ListeFilme;

public class TModel extends DefaultTableModel {

    TableModelListener[] tmls = null;
    public Object[][] object;
    Object[] columns;

    public TModel() {
    }

    public TModel(Object[][] data, Object[] columnNames) {
        super(data, columnNames);
        object = data;
        columns = columnNames;
    }

    public void reorder(int fromIndex, int toIndex) {
        this.moveRow(fromIndex, fromIndex, toIndex);
    }

    @Override
    public boolean isCellEditable(int i, int j) {
        return false;
    }

    /// tunen!
    public int getIdxRow(int idxSpalte, String idxWert) {
        //liefert die Zeile in der die erste Spalte idx enthält
        int ret = 0;
        ListIterator<List> it = this.getDataVector().listIterator();
        while (it.hasNext()) {
            if (it.next().get(idxSpalte).equals(idxWert)) {
                return ret;
            }
            ++ret;
        }
        return -1;
    }

    public int getIdxRow(int idxSpalte, int idxWert) {
        //liefert die Zeile in der die erste Spalte idx enthält
        int ret = 0;
        ListIterator<List> it = this.getDataVector().listIterator();
        while (it.hasNext()) {
            if (((Integer) it.next().get(idxSpalte)).intValue() == idxWert) {
                return ret;
            }
            ++ret;
        }
        return -1;
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

    public void filter(Daten daten, boolean keineAbos, boolean kGesehen, boolean nurHd, boolean live) {
        List zeile;
        ListIterator<List> it = this.getDataVector().listIterator();
        while (it.hasNext()) {
            zeile = it.next();
            if (live) {
                if (!zeile.get(DatenFilm.FILM_THEMA_NR).equals(ListeFilme.THEMA_LIVE)) {
                    it.remove();
                    continue;
                }
            } else if (nurHd) {
                if (zeile.get(DatenFilm.FILM_URL_HD_NR).toString().isEmpty()) {
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

    public String[] getModelOfField(Daten daten, int feld, boolean leer) {
        /* erstellt ein StringArray mit den Daten des Feldes
         * leer: immer ein leeres Feld am Anfang */
        LinkedList<String> list = new LinkedList<String>();
        String[] ret;
        String str;
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

    public boolean delRow(int spalte, String wert) {
        /* löscht die Zeile(n) in der die "Spalte" den "Wert" hat */
        boolean gefunden = false;
        for (int i = this.getRowCount() - 1; i >= 0; --i) {
            if (this.getValueAt(i, spalte).toString().equals(wert)) {
                this.removeRow(i);
                gefunden = true;
            }
        }
        return gefunden;
    }
}
