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
import java.awt.Font;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;
import mediathek.controller.io.History;
import mediathek.controller.io.starter.Start;
import mediathek.daten.DDaten;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeFilme;

public class CellRendererFilme extends DefaultTableCellRenderer {

    private DDaten ddaten;
    private History history = null;

    public CellRendererFilme(DDaten d) {
        ddaten = d;
        history = ddaten.history;
    }

    @Override
    public Component getTableCellRendererComponent(
            JTable table,
            Object value,
            boolean isSelected,
            boolean hasFocus,
            int row,
            int column) {
        try {
            setBackground(null);
            setForeground(null);
            setFont(null);
            super.getTableCellRendererComponent(
                    table, value, isSelected, hasFocus, row, column);
            int r = table.convertRowIndexToModel(row);
            String url = table.getModel().getValueAt(r, DatenFilm.FILM_URL_NR).toString();
            boolean live = table.getModel().getValueAt(r, DatenFilm.FILM_THEMA_NR).equals(ListeFilme.THEMA_LIVE);
            boolean start = false;
            Start s = ddaten.starterClass.getStart(url);
            if (s != null) {
                if (s.datenDownload.getQuelle() == Start.QUELLE_BUTTON) {
                    start = true;
                    switch (s.status) {
                        case Start.STATUS_INIT:
                            if (isSelected) {
                                setBackground(GuiKonstanten.DOWNLOAD_FARBE_WAIT_SEL);
                            } else {
                                setBackground(GuiKonstanten.DOWNLOAD_FARBE_WAIT);
                            }
                            break;
                        case Start.STATUS_RUN:
                            if (isSelected) {
                                setBackground(GuiKonstanten.DOWNLOAD_FARBE_RUN_SEL);
                            } else {
                                setBackground(GuiKonstanten.DOWNLOAD_FARBE_RUN);
                            }
                            break;
                        case Start.STATUS_FERTIG:
                            if (isSelected) {
                                setBackground(GuiKonstanten.DOWNLOAD_FARBE_FERTIG_SEL);
                            } else {
                                setBackground(GuiKonstanten.DOWNLOAD_FARBE_FERTIG);
                            }
                            break;
                        case Start.STATUS_ERR:
                            if (isSelected) {
                                setBackground(GuiKonstanten.DOWNLOAD_FARBE_ERR_SEL);
                            } else {
                                setBackground(GuiKonstanten.DOWNLOAD_FARBE_ERR);
                            }
                            break;
                    }
                }
            }
            if (!start) {
                if (!live) {
                    // bei livestreams keine History anzeigen
                    if (history.contains(table.getModel().getValueAt(r, DatenFilm.FILM_URL_NR).toString())) {
                        if (isSelected) {
                            setBackground(GuiKonstanten.FARBE_GRAU_SEL);
                        } else {
                            setBackground(GuiKonstanten.FARBE_GRAU);
                        }
                    }
                } else {
                    setFont(new java.awt.Font("Dialog", Font.BOLD, 12));
                    setForeground(GuiKonstanten.DOWNLOAD_FARBE_LIVE);
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(630098552, this.getClass().getName(), ex);
        }
        return this;
    }
}
