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
package mediathek.gui;

import java.util.List;
import javax.swing.JTable;
import mediathek.Log;
import mediathek.daten.DDaten;

public class PanelVorlage extends javax.swing.JPanel {

    public DDaten ddaten;
    public boolean geaendert = false;
    public boolean stopBeob = false;
    private int[] breite = null;
    private int[] reihe = null;
    private List<? extends javax.swing.RowSorter.SortKey> listeSort = null;
    private int sel;
    private int rows;
    boolean erstesMal = false;

    /**
     *
     *  @param d
     */
    public PanelVorlage(DDaten d) {
        ddaten = d;
        ddaten.panelListe.addPanel(this);
        addComponentListener(new java.awt.event.ComponentAdapter() {

            @Override
            public void componentShown(java.awt.event.ComponentEvent evt) {
                isShown();
            }
        });
    }

    public void isShown() {
        // immer wenn isShown
        if (!erstesMal) {
            erstesIsShown();
        }
        if (geaendert) {
            geaendert = false;
            neuLaden();
        }
    }

    void erstesIsShown() {
    }

    public void neuLaden() {
        //nur wenn geändert
    }

    public void neuLadenSofort() {
        //nur wenn geändert
    }

    public void getSpalten(JTable tabelle) {
        try {
            breite = new int[tabelle.getColumnCount()];
            reihe = new int[tabelle.getColumnCount()];
            sel = tabelle.getSelectedRow();
            rows = tabelle.getRowCount();
            for (int i = 0; i < reihe.length && i < tabelle.getModel().getColumnCount(); ++i) {
                reihe[i] = tabelle.convertColumnIndexToModel(i);
            }
            for (int i = 0; i < breite.length && i < tabelle.getModel().getColumnCount(); ++i) {
                breite[i] = tabelle.getColumnModel().getColumn(
                        tabelle.convertColumnIndexToView(i)).getWidth();
            }
            if (tabelle.getRowSorter() != null) {
                listeSort = tabelle.getRowSorter().getSortKeys();
            }
        } catch (Exception ex) {
            Log.fehlerMeldung("PanelVorlage.getSpalten", ex);
        }
    }

    public void setSpalten(JTable tabelle) {
        try {
            for (int i = 0; i < breite.length && i < tabelle.getColumnCount(); ++i) {
                tabelle.getColumnModel().getColumn(
                        tabelle.convertColumnIndexToView(i)).setPreferredWidth(breite[i]);
            }
            for (int i = 0; i < reihe.length && i < tabelle.getColumnCount(); ++i) {
                tabelle.getColumnModel().moveColumn(
                        tabelle.convertColumnIndexToView(reihe[i]), i);

            }
            if (listeSort != null) {
                if (!listeSort.isEmpty()) {
                    tabelle.getRowSorter().setSortKeys(listeSort);
                }
            }
            if (rows == tabelle.getRowCount()) {
                if (sel >= 0 && sel < tabelle.getRowCount()) {
                    tabelle.setRowSelectionInterval(sel, sel);
                    tabelle.scrollRectToVisible(tabelle.getCellRect(sel, 0, false));
                }
            }
            tabelle.validate();
        } catch (Exception ex) {
            Log.fehlerMeldung("PanelVorlage.setSpalten", ex);
        }
    }
}
