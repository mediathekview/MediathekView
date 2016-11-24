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
import mSearch.daten.DatenFilm;
import mSearch.daten.ListeFilme;
import mSearch.tool.Listener;
import mSearch.tool.Log;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVColor;
import mediathek.config.MVConfig;
import mediathek.controller.MVUsedUrls;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.*;

@SuppressWarnings("serial")
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
        ja_16 = Icons.ICON_TABELLE_EIN;
        nein_12 = Icons.ICON_TABELLE_AUS;
        history = d.history;
        film_start_tab = Icons.ICON_TABELLE_FILM_START;
        film_start_sw_tab = Icons.ICON_TABELLE_FILM_START_SW;
        film_rec_tab = Icons.ICON_TABELLE_FILM_REC;
        film_rec_sw_tab = Icons.ICON_TABELLE_FILM_REC_SW;
        film_stop_tab = Icons.ICON_TABELLE_FILM_STOP;
        film_stop_sw_tab = Icons.ICON_TABELLE_FILM_STOP_SW;
        geoMelden = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_GEO_MELDEN));
        Listener.addListener(new Listener(Listener.EREIGNIS_GEO, CellRendererFilme.class.getSimpleName()) {
            @Override
            public void ping() {
                geoMelden = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_GEO_MELDEN));
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
            DatenFilm datenFilm = (DatenFilm) table.getModel().getValueAt(rowModelIndex, DatenFilm.FILM_REF);
            DatenDownload datenDownload = Daten.getInstance().getListeDownloadsButton().getDownloadUrlFilm(datenFilm.arr[DatenFilm.FILM_URL]);

            if (((MVTable) table).lineBreak) {
                JTextArea textArea;
                switch (columnModelIndex) {
                    case DatenFilm.FILM_BESCHREIBUNG:
                    case DatenFilm.FILM_THEMA:
                    case DatenFilm.FILM_TITEL:
                    case DatenFilm.FILM_URL:
                        textArea = new JTextArea();
                        textArea.setLineWrap(true);
                        textArea.setWrapStyleWord(true);
                        textArea.setText(value.toString());
                        textArea.setForeground(getForeground());
                        textArea.setBackground(getBackground());
                        if (!SystemInfo.isMacOSX()) {
                            // On OS X do not change fonts as it violates HIG...
                            if (isSelected) {
                                textArea.setFont(new java.awt.Font("Dialog", Font.BOLD, MVFont.fontSize));
                            } else {
                                textArea.setFont(new java.awt.Font("Dialog", Font.PLAIN, MVFont.fontSize));
                            }
                        }
                        setColor(textArea, datenFilm, datenDownload, isSelected);
                        return textArea;
                }
            }

            if (!SystemInfo.isMacOSX()) {
                // On OS X do not change fonts as it violates HIG...
                if (isSelected) {
                    setFont(new java.awt.Font("Dialog", Font.BOLD, MVFont.fontSize));
                } else {
                    setFont(new java.awt.Font("Dialog", Font.PLAIN, MVFont.fontSize));
                }
            }
            switch (columnModelIndex) {
                case DatenFilm.FILM_NR:
                case DatenFilm.FILM_DATUM:
                case DatenFilm.FILM_ZEIT:
                case DatenFilm.FILM_DAUER:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    break;
                case DatenFilm.FILM_GROESSE:
                    setHorizontalAlignment(SwingConstants.RIGHT);
                    break;
                case DatenFilm.FILM_ABSPIELEN:
                    handleButtonStartColumn(datenDownload, isSelected);
                    break;

                case DatenFilm.FILM_AUFZEICHNEN:
                    handleButtonDownloadColumn(isSelected);
                    break;
                case DatenFilm.FILM_SENDER:
                    if (((MVTable) table).iconAnzeigen) {
                        handleSenderColumn((String) value, ((MVTable) table).iconKlein);
                    }
                    break;
                case DatenFilm.FILM_NEU:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    if (datenFilm.isNew()) {
                        setIcon(ja_16);
                    } else {
                        setIcon(nein_12);
                    }
                    setText("");
                    break;
                case DatenFilm.FILM_HD:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    if (datenFilm.isHD()) {
                        setIcon(ja_16);
                    } else {
                        setIcon(nein_12);
                    }
                    setText("");//im Modle brauchen wir den Text zum Sortieren
                    break;
                case DatenFilm.FILM_UT:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    if (datenFilm.hasUT()) {
                        setIcon(ja_16);
                    } else {
                        setIcon(nein_12);
                    }
                    setText("");
                    break;
            }
            setColor(this, datenFilm, datenDownload, isSelected);
        } catch (Exception ex) {
            Log.errorLog(630098552, ex);
        }
        return this;
    }

    private void setColor(Component c, DatenFilm datenFilm, DatenDownload datenDownload, boolean isSelected) {
        boolean live = datenFilm.arr[DatenFilm.FILM_THEMA].equals(ListeFilme.THEMA_LIVE);
        boolean start = false;

        if (datenDownload != null) {
            // gestarteter Film
            if (datenDownload.start != null) {
                start = true;

                switch (datenDownload.start.status) {
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
        if (!start) {
            if (live) {
                // bei livestreams keine History anzeigen
                c.setForeground(MVColor.FILM_LIVESTREAM.color);
            } else if (history.urlPruefen(datenFilm.getUrlHistory())) {
                if (!isSelected) {
                    c.setBackground(MVColor.FILM_HISTORY.color);
                }
            } else if (datenFilm.isNew()) {
                c.setForeground(MVColor.FILM_NEU.color);
            }
        }
        if (!start && geoMelden) {
            if (!datenFilm.arr[DatenFilm.FILM_GEO].isEmpty()) {
                if (!datenFilm.arr[DatenFilm.FILM_GEO].contains(MVConfig.get(MVConfig.Configs.SYSTEM_GEO_STANDORT))) {
                    //setForeground(GuiKonstanten.FARBE_FILM_GEOBLOCK_FORGROUND);
                    if (isSelected) {
                        c.setBackground(MVColor.FILM_GEOBLOCK_BACKGROUND_SEL.color);
                    } else {
                        c.setBackground(MVColor.FILM_GEOBLOCK_BACKGROUND.color);
                    }
                }
            }
        }

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
}
