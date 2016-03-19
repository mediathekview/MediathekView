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

import com.jidesoft.utils.SystemInfo;
import mediathek.controller.Log;
import mediathek.controller.MVUsedUrls;
import mediathek.controller.starter.Start;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;
import mediathek.res.GetIcon;
import msearch.daten.DatenFilm;
import msearch.daten.ListeFilme;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.*;

public class CellRendererFilme extends DefaultTableCellRenderer {

    private static ImageIcon film_start_tab = null;
    private static ImageIcon film_start_sw_tab = null;
    private static ImageIcon film_rec_tab = null;
    private static ImageIcon film_rec_sw_tab = null;
    private static ImageIcon film_stop_tab = null;
    private static ImageIcon film_stop_sw_tab = null;
    private boolean geoMelden = false;
    private MVUsedUrls history = null;
    private final MVSenderIconCache senderIconCache;
    private static ImageIcon ja_16 = null;
    private static ImageIcon nein_12 = null;

    public CellRendererFilme(Daten d) {
        ja_16 = GetIcon.getProgramIcon("ja_16.png");
        nein_12 = GetIcon.getProgramIcon("nein_12.png");
        history = d.history;
        film_start_tab = GetIcon.getProgramIcon("film_start_tab.png");
        film_start_sw_tab = GetIcon.getProgramIcon("film_start_sw_tab.png");
        film_rec_tab = GetIcon.getProgramIcon("film_rec_tab.png");
        film_rec_sw_tab = GetIcon.getProgramIcon("film_rec_sw_tab.png");
        film_stop_tab = GetIcon.getProgramIcon("film_stop_tab.png");
        film_stop_sw_tab = GetIcon.getProgramIcon("film_stop_sw_tab.png");
        geoMelden = Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_GEO_MELDEN));
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_GEO, CellRendererFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                geoMelden = Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_GEO_MELDEN));
            }
        });
        senderIconCache = new MVSenderIconCache();
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
            setIcon(null);
            setToolTipText(null);
            setHorizontalAlignment(SwingConstants.LEADING);
            super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);

            final int rowModelIndex = table.convertRowIndexToModel(row);
            final int columnModelIndex = table.convertColumnIndexToModel(column);

            DatenFilm datenFilm = (DatenFilm) table.getModel().getValueAt(rowModelIndex, DatenFilm.FILM_REF_NR);
            DatenDownload datenDownload = Daten.listeDownloadsButton.getDownloadUrlFilm(datenFilm.arr[DatenFilm.FILM_URL_NR]);

            boolean live = datenFilm.arr[DatenFilm.FILM_THEMA_NR].equals(ListeFilme.THEMA_LIVE);
            boolean start = false;

            /*
             * On OS X do not change fonts as it violates HIG...
             */
            if (!SystemInfo.isMacOSX()) {
                if (isSelected)
                    setFont(new java.awt.Font("Dialog", Font.BOLD, MVFont.fontSize));
                else
                    setFont(new java.awt.Font("Dialog", Font.PLAIN, MVFont.fontSize));
            }

            switch (columnModelIndex) {
                case DatenFilm.FILM_NR_NR:
                case DatenFilm.FILM_DATUM_NR:
                case DatenFilm.FILM_ZEIT_NR:
                case DatenFilm.FILM_DAUER_NR:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    break;
                case DatenFilm.FILM_GROESSE_NR:
                    setHorizontalAlignment(SwingConstants.RIGHT);
                    break;
                case DatenFilm.FILM_ABSPIELEN_NR:
                    handleButtonStartColumn(datenDownload, isSelected);
                    break;

                case DatenFilm.FILM_AUFZEICHNEN_NR:
                    handleButtonDownloadColumn(isSelected);
                    break;
                case DatenFilm.FILM_SENDER_NR:
                    if (((MVTable) table).iconAnzeigen) {
                        handleSenderColumn((String) value, ((MVTable) table).iconKlein);
                    }
                    break;
                case DatenFilm.FILM_NEU_NR:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    if (datenFilm.neuerFilm) {
                        setIcon(ja_16);
                    } else {
                        setIcon(nein_12);
                    }
                    break;
            }

            // Farben setzen
            if (datenDownload != null) {
                // gestarteter Film
                if (datenDownload.start != null) {
                    start = true;
                    setColor(this, datenDownload.start, isSelected);
                }
            }
            if (!start) {
                if (live) {
                    // bei livestreams keine History anzeigen
                    setForeground(MVColor.FILM_LIVESTREAM.color);
                } else {
                    if (history.urlPruefen(datenFilm.getUrlHistory())) {
                        if (!isSelected) {
                            setBackground(MVColor.FILM_HISTORY.color);
                        }
                    } else if (datenFilm.neuerFilm) {
                        setForeground(MVColor.FILM_NEU.color);
                    }
                }
            }
            if (!start && geoMelden) {
                if (!datenFilm.arr[DatenFilm.FILM_GEO_NR].isEmpty()) {
                    if (!datenFilm.arr[DatenFilm.FILM_GEO_NR].contains(Daten.mVConfig.get(MVConfig.SYSTEM_GEO_STANDORT))) {
                        //setForeground(GuiKonstanten.FARBE_FILM_GEOBLOCK_FORGROUND);
                        if (isSelected) {
                            setBackground(MVColor.FILM_GEOBLOCK_BACKGROUND_SEL.color);
                        } else {
                            setBackground(MVColor.FILM_GEOBLOCK_BACKGROUND.color);
                        }
                    }
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(630098552, ex);
        }
        return this;
    }

    private void handleButtonStartColumn(final DatenDownload datenDownload, final boolean isSelected) {
        // Button Abspielen
        setHorizontalAlignment(SwingConstants.CENTER);
        if (datenDownload != null) {
            if (datenDownload.start != null) {
                if (datenDownload.start.status == Start.STATUS_RUN) {
                    setToolTipText("Film stoppen");
                    if (isSelected) {
                        setIcon(film_stop_tab);
                    } else {
                        setIcon(film_stop_sw_tab);
                    }
                }
            }
        }
        if (getIcon() == null) {
            setToolTipText("Film abspielen");
            if (isSelected) {
                setIcon(film_start_tab);
            } else {
                setIcon(film_start_sw_tab);
            }
        }
    }

    private void handleButtonDownloadColumn(final boolean isSelected) {
        // Button Aufzeichnen
        setHorizontalAlignment(SwingConstants.CENTER);
        setToolTipText("Film aufzeichnen");
        if (isSelected) {
            setIcon(film_rec_tab);
        } else {
            setIcon(film_rec_sw_tab);
        }
    }

    /**
     * Draws the sender icon in the sender model column.
     *
     * @param sender Name of the sender.
     */
    private void handleSenderColumn(String sender, boolean small) {
        setHorizontalAlignment(SwingConstants.CENTER);
        ImageIcon icon = senderIconCache.get(sender, small);
        if (icon != null) {
            setText("");
            setIcon(icon);
        }
    }

    private void setColor(Component c, Start s, boolean isSelected) {
        switch (s.status) {
            case Start.STATUS_INIT:
                if (isSelected) {
                    c.setBackground(MVColor.DOWNLOAD_WAIT_SEL.color);
                } else {
                    c.setBackground(MVColor.DOWNLOAD_WAIT.color);
                }
                break;
            case Start.STATUS_RUN:
                if (isSelected) {
                    c.setBackground(MVColor.DOWNLOAD_RUN_SEL.color);
                } else {
                    c.setBackground(MVColor.DOWNLOAD_RUN.color);
                }
                break;
            case Start.STATUS_FERTIG:
                if (isSelected) {
                    c.setBackground(MVColor.DOWNLOAD_FERTIG_SEL.color);
                } else {
                    c.setBackground(MVColor.DOWNLOAD_FERTIG.color);
                }
                break;
            case Start.STATUS_ERR:
                if (isSelected) {
                    c.setBackground(MVColor.DOWNLOAD_FEHLER_SEL.color);
                } else {
                    c.setBackground(MVColor.DOWNLOAD_FEHLER.color);
                }
                break;
        }
    }
}
