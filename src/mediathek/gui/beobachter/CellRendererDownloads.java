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
package mediathek.gui.beobachter;

import java.awt.Component;
import java.awt.Font;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableCellRenderer;
import mediathek.Log;
import mediathek.controller.io.starter.Starts;
import mediathek.daten.DDaten;
import mediathek.daten.DatenDownload;
import mediathek.tool.GuiKonstanten;

public class CellRendererDownloads extends DefaultTableCellRenderer {

    private DDaten ddaten;

    public CellRendererDownloads(DDaten d) {
        ddaten = d;
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
            setIcon(null);
//            setHorizontalAlignment(SwingConstants.LEADING);
            super.getTableCellRendererComponent(
                    table, value, isSelected, hasFocus, row, column);
            int r = table.convertRowIndexToModel(row);
            int c = table.convertColumnIndexToModel(column);
            String url = table.getModel().getValueAt(r, DatenDownload.DOWNLOAD_URL_NR).toString();
            // Abos
            boolean abo = !table.getModel().getValueAt(r, DatenDownload.DOWNLOAD_ABO_NR).equals("");
            // Starts
            Starts s = ddaten.starterClass.getStart(url);
            if (s != null) {
                switch (s.status) {
                    case Starts.STATUS_INIT:
                        if (isSelected) {
                            setBackground(GuiKonstanten.DOWNLOAD_FARBE_WAIT_SEL);
                        } else {
                            setBackground(GuiKonstanten.DOWNLOAD_FARBE_WAIT);
                        }
                        break;
                    case Starts.STATUS_RUN:
                        if (isSelected) {
                            setBackground(GuiKonstanten.DOWNLOAD_FARBE_RUN_SEL);
                        } else {
                            setBackground(GuiKonstanten.DOWNLOAD_FARBE_RUN);
                        }
                        break;
                    case Starts.STATUS_FERTIG:
                        if (isSelected) {
                            setBackground(GuiKonstanten.DOWNLOAD_FARBE_FERTIG_SEL);
                        } else {
                            setBackground(GuiKonstanten.DOWNLOAD_FARBE_FERTIG);
                        }
                        break;
                    case Starts.STATUS_ERR:
                        if (isSelected) {
                            setBackground(GuiKonstanten.DOWNLOAD_FARBE_ERR_SEL);
                        } else {
                            setBackground(GuiKonstanten.DOWNLOAD_FARBE_ERR);
                        }
                        break;
                }
            }
            if (c == DatenDownload.DOWNLOAD_ABO_NR) {
                setFont(new java.awt.Font("Dialog", Font.BOLD, 12));
                if (abo) {
                    setForeground(GuiKonstanten.ABO_FOREGROUND);
//                    if (isSelected) {
//                        setBackground(GuiKonstanten.ABO_SEL);
//                    } else {
//                        setBackground(GuiKonstanten.ABO);
//                    }
                } else {
                    setForeground(GuiKonstanten.DOWNLOAD_FOREGROUND);
//                    if (isSelected) {
//                        setBackground(GuiKonstanten.DOWNLOAD_SEL);
//                    } else {
//                        setBackground(GuiKonstanten.DOWNLOAD);
//                    }
                    setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/nein_12.png")));
//                    setHorizontalAlignment(SwingConstants.CENTER);
//                    setText("-");
                }
            }
            if (c == DatenDownload.DOWNLOAD_PROGRAMM_RESTART_NR) {
                boolean restart = ddaten.listeDownloads.getDownloadByUrl(url).isRestart();
                setHorizontalAlignment(SwingConstants.CENTER);
                if (restart) {
                    setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/ja_16.png")));
                } else {
                    setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/nein_12.png")));
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(this.getClass().getName(), ex);
        }
        return this;
    }
}
