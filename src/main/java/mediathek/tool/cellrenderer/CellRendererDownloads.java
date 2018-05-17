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
package mediathek.tool.cellrenderer;

import com.jidesoft.utils.SystemInfo;
import mSearch.tool.Log;
import mediathek.config.Icons;
import mediathek.config.MVColor;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.tool.MVSenderIconCache;
import mediathek.tool.table.MVTable;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.basic.BasicProgressBarUI;
import java.awt.*;

@SuppressWarnings("serial")
public class CellRendererDownloads extends CellRendererBaseWithStart {
    private final static String DOWNLOAD_STARTEN = "Download starten";
    private final static String DOWNLOAD_LOESCHEN = "Download aus Liste entfernen";
    private final static String DOWNLOAD_STOPPEN = "Download stoppen";
    private final static String DOWNLOAD_ENTFERNEN = "Download entfernen";
    private final static String PLAY_DOWNLOADED_FILM = "gespeicherten Film abspielen";
    private static ImageIcon download_stop_tab = null;
    private static ImageIcon download_start_tab = null;
    private static ImageIcon download_stop_sw_tab = null;
    private static ImageIcon download_start_sw_tab = null;
    private static ImageIcon download_clear_tab = null;
    private static ImageIcon download_clear_sw_tab = null;
    private static ImageIcon download_del_tab = null;
    private static ImageIcon download_del_sw_tab = null;
    private JProgressBar progressBar;
    private final Border emptyBorder = BorderFactory.createEmptyBorder();
    private final Border largeBorder = BorderFactory.createEmptyBorder(9, 2, 9, 2);
    private final JPanel panel;

    public CellRendererDownloads(MVSenderIconCache cache) {
        super(cache);

        download_stop_tab = Icons.ICON_TABELLE_DOWNOAD_STOP;
        download_stop_sw_tab = Icons.ICON_TABELLE_DOWNOAD_STOP_SW;
        download_start_tab = Icons.ICON_TABELLE_DOWNOAD_START;
        download_start_sw_tab = Icons.ICON_TABELLE_DOWNOAD_START_SW;
        download_clear_tab = Icons.ICON_TABELLE_DOWNOAD_CLEAR;
        download_clear_sw_tab = Icons.ICON_TABELLE_DOWNOAD_CLEAR_SW;
        download_del_tab = Icons.ICON_TABELLE_DOWNOAD_DEL;
        download_del_sw_tab = Icons.ICON_TABELLE_DOWNOAD_DEL_SW;

        setupProgressBar();

        panel = new JPanel(new BorderLayout());
        panel.add(progressBar);
    }

    private void setupProgressBar() {
        progressBar = new JProgressBar(0, 1000);
        progressBar.setStringPainted(true);
        //on OSX the OS provided progress bar looks much better...
        if (!SystemInfo.isMacOSX()) {
            progressBar.setUI(new BasicProgressBarUI() {
                @Override
                protected Color getSelectionBackground() {
                    return UIManager.getDefaults().getColor("Table.foreground");
                }

                @Override
                protected Color getSelectionForeground() {
                    return Color.white;
                }
            });
        }
    }

    @Override
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
                                                   int row, int column) {
        try {
            setBackground(null);
            setForeground(null);
            setIcon(null);
            setToolTipText(null);
            setHorizontalAlignment(SwingConstants.LEADING);

            super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);

            final int rowModelIndex = table.convertRowIndexToModel(row);
            final int columnModelIndex = table.convertColumnIndexToModel(column);
            DatenDownload datenDownload = (DatenDownload) table.getModel().getValueAt(rowModelIndex, DatenDownload.DOWNLOAD_REF);

            if (((MVTable) table).lineBreak) {
                JTextArea textArea;
                switch (columnModelIndex) {
                    case DatenDownload.DOWNLOAD_TITEL:
                    case DatenDownload.DOWNLOAD_THEMA:
                    case DatenDownload.DOWNLOAD_URL:
                    case DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF:
                    case DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY:
                    case DatenDownload.DOWNLOAD_FILM_URL:
                    case DatenDownload.DOWNLOAD_URL_SUBTITLE:
                    case DatenDownload.DOWNLOAD_ZIEL_DATEINAME:
                    case DatenDownload.DOWNLOAD_ZIEL_PFAD:
                    case DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME:
                    case DatenDownload.DOWNLOAD_ABO:
                        textArea = new JTextArea();
                        textArea.setLineWrap(true);
                        textArea.setWrapStyleWord(true);
                        textArea.setText(value.toString());
                        textArea.setForeground(getForeground());
                        textArea.setBackground(getBackground());
                        if (columnModelIndex == DatenDownload.DOWNLOAD_ABO) {
                            handleAboColumn(textArea, datenDownload);
                        }
                        setBackgroundColor(textArea, datenDownload.start, isSelected);
                        handleGeoBlocking(textArea, datenDownload, isSelected);
                        setSelectionFont(textArea, isSelected);
                        return textArea;
                }
            }

            setSelectionFont(this, isSelected);

            switch (columnModelIndex) {
                case DatenDownload.DOWNLOAD_PROGRESS:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    if (((MVTable) table).getShowIcons() && !((MVTable) table).iconKlein) {
                        progressBar.setBorder(largeBorder);
                    } else {
                        progressBar.setBorder(emptyBorder);
                    }
                    if (datenDownload.start != null) {
                        if (1 < datenDownload.start.percent && datenDownload.start.percent < Start.PROGRESS_FERTIG) {

                            setBackgroundColor(panel, datenDownload.start, isSelected);
                            setBackgroundColor(progressBar, datenDownload.start, isSelected);

                            progressBar.setValue(datenDownload.start.percent);

                            final double progressValue = datenDownload.start.percent / 10.0;
                            progressBar.setString(Double.toString(progressValue) + '%');

                            return panel;
                        } else {
                            setText(Start.getTextProgress(datenDownload.isDownloadManager(), datenDownload.start));
                        }
                    } else {
                        setText("");
                    }
                    break;

                case DatenDownload.DOWNLOAD_RESTZEIT:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    if (datenDownload.start != null && datenDownload.start.beginnAnschauen) {
                        setForeground(MVColor.DOWNLOAD_ANSEHEN.color);
                    }
                    break;

                case DatenDownload.DOWNLOAD_FILM_NR:
                    if ((int) table.getModel().getValueAt(rowModelIndex, DatenDownload.DOWNLOAD_FILM_NR) == 0) {
                        setText("");
                    }
                    setHorizontalAlignment(SwingConstants.CENTER);
                    break;

                case DatenDownload.DOWNLOAD_PROGRAMM_RESTART:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    setYesNoIcon(datenDownload.isRestart());
                    break;

                case DatenDownload.DOWNLOAD_PROGRAMM_DOWNLOADMANAGER:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    setYesNoIcon(datenDownload.isDownloadManager());
                    break;

                case DatenDownload.DOWNLOAD_ART:
                    switch (datenDownload.art) {
                        case DatenDownload.ART_DOWNLOAD:
                            setText(DatenDownload.ART_DOWNLOAD_TXT);
                            break;
                        case DatenDownload.ART_PROGRAMM:
                            setText(DatenDownload.ART_PROGRAMM_TXT);
                            break;
                    }
                    break;
                case DatenDownload.DOWNLOAD_QUELLE:
                    switch (datenDownload.quelle) {
                        case DatenDownload.QUELLE_ALLE:
                            setText(DatenDownload.QUELLE_ALLE_TXT);
                            break;
                        case DatenDownload.QUELLE_ABO:
                            setText(DatenDownload.QUELLE_ABO_TXT);
                            break;
                        case DatenDownload.QUELLE_BUTTON:
                            setText(DatenDownload.QUELLE_BUTTON_TXT);
                            break;
                        case DatenDownload.QUELLE_DOWNLOAD:
                            setText(DatenDownload.QUELLE_DOWNLOAD_TXT);
                            break;
                    }
                    break;
                case DatenDownload.DOWNLOAD_UNTERBROCHEN:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    setYesNoIcon(datenDownload.isInterrupted());
                    break;

                case DatenDownload.DOWNLOAD_ZURUECKGESTELLT:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    setYesNoIcon(datenDownload.istZurueckgestellt());
                    break;

                case DatenDownload.DOWNLOAD_INFODATEI:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    setYesNoIcon(datenDownload.isInfoFile());
                    break;

                case DatenDownload.DOWNLOAD_SUBTITLE:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    setYesNoIcon(datenDownload.isSubtitle());
                    break;

                case DatenDownload.DOWNLOAD_SPOTLIGHT:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    setYesNoIcon(datenDownload.isSpotlight());
                    break;

                case DatenDownload.DOWNLOAD_BUTTON_START:
                    handleButtonStartColumn(datenDownload, isSelected);
                    break;

                case DatenDownload.DOWNLOAD_BUTTON_DEL:
                    handleButtonDeleteColumn(datenDownload, isSelected);
                    break;

                case DatenDownload.DOWNLOAD_GROESSE:
                    setHorizontalAlignment(SwingConstants.RIGHT);
                    break;

                case DatenDownload.DOWNLOAD_ABO:
                    handleAboColumn(datenDownload);
                    break;

                case DatenDownload.DOWNLOAD_NR:
                case DatenDownload.DOWNLOAD_DATUM:
                case DatenDownload.DOWNLOAD_ZEIT:
                case DatenDownload.DOWNLOAD_DAUER:
                case DatenDownload.DOWNLOAD_BANDBREITE:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    break;

                case DatenDownload.DOWNLOAD_SENDER:
                    if (((MVTable) table).getShowIcons()) {
                        handleSenderColumn((String) value, ((MVTable) table).iconKlein);
                    }
                    break;
                case DatenDownload.DOWNLOAD_HD:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    setYesNoIcon(datenDownload.film != null && datenDownload.film.isHD());
                    setText("");//im Modle brauchen wir den Text zum Sortieren
                    break;

                case DatenDownload.DOWNLOAD_UT:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    setYesNoIcon(datenDownload.film != null && datenDownload.film.hasSubtitle());
                    setText("");//im Modle brauchen wir den Text zum Sortieren
                    break;
            }

            setBackgroundColor(this, datenDownload.start, isSelected);
            handleGeoBlocking(this, datenDownload, isSelected);
        } catch (Exception ex) {
            Log.errorLog(758200166, ex);
        }
        return this;
    }

    private void handleButtonStartColumn(final DatenDownload datenDownload, final boolean isSelected) {
        setHorizontalAlignment(SwingConstants.CENTER);
        if (isSelected) {
            if (datenDownload.start != null && !datenDownload.isDownloadManager()) {
                switch (datenDownload.start.status) {
                    case Start.STATUS_FERTIG:
                        setIcon(film_start_tab);
                        setToolTipText(PLAY_DOWNLOADED_FILM);
                        break;

                    case Start.STATUS_ERR:
                        setIcon(download_start_tab);
                        setToolTipText(DOWNLOAD_STARTEN);
                        break;

                    default:
                        setIcon(download_stop_tab);
                        setToolTipText(DOWNLOAD_STOPPEN);
                        break;
                }
            } else {
                setIcon(download_start_tab);
                setToolTipText(DOWNLOAD_STARTEN);
            }
        } else if (datenDownload.start != null && !datenDownload.isDownloadManager()) {
            switch (datenDownload.start.status) {
                case Start.STATUS_FERTIG:
                    setIcon(film_start_sw_tab);
                    setToolTipText(PLAY_DOWNLOADED_FILM);
                    break;

                case Start.STATUS_ERR:
                    setIcon(download_start_sw_tab);
                    setToolTipText(DOWNLOAD_STARTEN);
                    break;

                default:
                    setIcon(download_stop_sw_tab);
                    setToolTipText(DOWNLOAD_STOPPEN);
                    break;
            }
        } else {
            setIcon(download_start_sw_tab);
            setToolTipText(DOWNLOAD_STARTEN);
        }
    }

    private void handleAboColumn(JTextArea a, final DatenDownload datenDownload) {
        if (!datenDownload.arr[DatenDownload.DOWNLOAD_ABO].isEmpty()) {
            a.setForeground(MVColor.DOWNLOAD_IST_ABO.color);
        } else {
            a.setForeground(MVColor.DOWNLOAD_IST_DIREKTER_DOWNLOAD.color);
            a.setText("Download");
        }
    }

    private void handleAboColumn(final DatenDownload datenDownload) {
        setHorizontalAlignment(SwingConstants.CENTER);
        if (!datenDownload.arr[DatenDownload.DOWNLOAD_ABO].isEmpty()) {
            setForeground(MVColor.DOWNLOAD_IST_ABO.color);
        } else {
            setForeground(MVColor.DOWNLOAD_IST_DIREKTER_DOWNLOAD.color);
            setText("Download");
        }
    }

    private void handleGeoBlocking(Component c, final DatenDownload datenDownload, final boolean isSelected) {
        if (geoMelden) {
            if (datenDownload.start == null) {
                setupGeoblockingBackground(c, datenDownload.arr[DatenDownload.DOWNLOAD_GEO], isSelected);
            }
        }
    }

    private void handleButtonDeleteColumn(final DatenDownload datenDownload, final boolean isSelected) {
        setHorizontalAlignment(SwingConstants.CENTER);
        if (datenDownload.start != null) {
            if (datenDownload.start.status >= Start.STATUS_FERTIG) {
                setIcons(download_clear_tab, download_clear_sw_tab, DOWNLOAD_ENTFERNEN, isSelected);
            } else {
                setupDownloadLoeschen(isSelected);
            }
        } else {
            setupDownloadLoeschen(isSelected);
        }
    }

    private void setIcons(Icon tab, Icon tab_sw, String text, final boolean isSelected) {
        final Icon icon;
        if (isSelected) {
            icon = tab;
        } else {
            icon = tab_sw;
        }
        setIcon(icon);
        setToolTipText(text);
    }

    private void setupDownloadLoeschen(final boolean isSelected) {
        setIcons(download_del_tab, download_del_sw_tab, DOWNLOAD_LOESCHEN, isSelected);
    }
}
