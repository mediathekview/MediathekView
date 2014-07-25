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
package mediathek.tool;

import java.awt.Component;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableCellRenderer;
import mediathek.controller.Log;
import mediathek.daten.Daten;
import mediathek.daten.DatenPset;
import mediathek.res.GetIcon;

public class CellRendererPset extends DefaultTableCellRenderer {

    Daten daten;

    public CellRendererPset(Daten d) {
        daten = d;
    }

    @Override
    public Component getTableCellRendererComponent(
            JTable table,
            Object value,
            boolean isSelected,
            boolean hasFocus,
            int row,
            int column) {
        setBackground(null);
        setForeground(null);
        setFont(null);
        setIcon(null);
        setHorizontalAlignment(SwingConstants.LEADING);
        super.getTableCellRendererComponent(
                table, value, isSelected, hasFocus, row, column);
        try {
            int r = table.convertRowIndexToModel(row);
            int c = table.convertColumnIndexToModel(column);
            DatenPset datenPset = new DatenPset();
            for (int i = 0; i < DatenPset.MAX_ELEM; ++i) {
                datenPset.arr[i] = table.getModel().getValueAt(r, i).toString();
            }
            if (c == DatenPset.PROGRAMMSET_NAME_NR) {
                setForeground(datenPset.getFarbe());
            }
            if (c == DatenPset.PROGRAMMSET_IST_ABSPIELEN_NR) {
                setHorizontalAlignment(SwingConstants.CENTER);
                setText(""); // nur das Icon anzeigen
                if (datenPset.istAbspielen()) {
                    setIcon(GetIcon.getProgramIcon("ja_16.png"));
                } else {
                    setIcon(GetIcon.getProgramIcon("nein_12.png"));
                }
            }
            if (c == DatenPset.PROGRAMMSET_IST_SPEICHERN_NR) {
                setHorizontalAlignment(SwingConstants.CENTER);
                setText(""); // nur das Icon anzeigen
                if (datenPset.istSpeichern()) {
                    setIcon(GetIcon.getProgramIcon("ja_16.png"));
                } else {
                    setIcon(GetIcon.getProgramIcon("nein_12.png"));
                }
            }
//            if (c == DatenPset.PROGRAMMSET_IST_BUTTON_NR) {
//                setText(""); // nur das Icon anzeigen
//                if (datenPset.istButton()) {
//                    if (isSelected) {
//                        setBackground(GuiKonstanten.ABO_SEL);
//                    } else {
//                        setBackground(GuiKonstanten.ABO);
//                    }
//                    setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/ja_16.png")));
//                } else {
//                    if (isSelected) {
//                        setBackground(GuiKonstanten.FARBE_GRAU_SEL);
//                    } else {
//                        setBackground(GuiKonstanten.FARBE_GRAU);
//                    }
//                    setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/nein_12.png")));
//                }
//            }
//            if (c == DatenPset.PROGRAMMSET_IST_ABO_NR) {
//                setText(""); // nur das Icon anzeigen
//                if (datenPset.istAbo()) {
//                    if (isSelected) {
//                        setBackground(GuiKonstanten.ABO_SEL);
//                    } else {
//                        setBackground(GuiKonstanten.ABO);
//                    }
//                    setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/ja_16.png")));
//                } else {
//                    if (isSelected) {
//                        setBackground(GuiKonstanten.FARBE_GRAU_SEL);
//                    } else {
//                        setBackground(GuiKonstanten.FARBE_GRAU);
//                    }
//                    setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/nein_12.png")));
//                }
//            }
        } catch (Exception ex) {
            Log.fehlerMeldung(962380071, Log.FEHLER_ART_PROG, this.getClass().getName(), ex);
        }
        return this;
    }
}
