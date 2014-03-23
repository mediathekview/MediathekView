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

import mediathek.controller.Log;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.plaf.basic.BasicProgressBarUI;
import javax.swing.table.DefaultTableCellRenderer;
import mediathek.controller.starter.Start;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;
import mediathek.res.GetIcon;

public class CellRendererDownloads extends DefaultTableCellRenderer {

    private static ImageIcon ja_16 = null;
    private static ImageIcon nein_12 = null;
    private static ImageIcon film_start_tab = null;
    private static ImageIcon download_stop_tab = null;
    private static ImageIcon download_start_tab = null;
    private static ImageIcon film_start_sw_tab = null;
    private static ImageIcon download_stop_sw_tab = null;
    private static ImageIcon download_start_sw_tab = null;
    private static ImageIcon download_clear_tab = null;
    private static ImageIcon download_clear_sw_tab = null;
    private static ImageIcon download_del_tab = null;
    private static ImageIcon download_del_sw_tab = null;
    private boolean geoMelden = false;

    public CellRendererDownloads() {
        ja_16 = GetIcon.getIcon("ja_16.png");
        nein_12 = GetIcon.getIcon("nein_12.png");
        film_start_tab = GetIcon.getIcon("film_start_tab.png");
        film_start_sw_tab = GetIcon.getIcon("film_start_sw_tab.png");
        download_stop_tab = GetIcon.getIcon("download_stop_tab.png");
        download_stop_sw_tab = GetIcon.getIcon("download_stop_sw_tab.png");
        download_start_tab = GetIcon.getIcon("download_start_tab.png");
        download_start_sw_tab = GetIcon.getIcon("download_start_sw_tab.png");
        download_clear_tab = GetIcon.getIcon("download_clear_tab.png");
        download_clear_sw_tab = GetIcon.getIcon("download_clear_sw_tab.png");
        download_del_tab = GetIcon.getIcon("download_del_tab.png");
        download_del_sw_tab = GetIcon.getIcon("download_del_sw_tab.png");
        geoMelden = Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_GEO_MELDEN));
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_GEO, CellRendererDownloads.class.getSimpleName()) {
            @Override
            public void ping() {
                geoMelden = Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_GEO_MELDEN));
            }
        });
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
            setToolTipText(null);
            setHorizontalAlignment(SwingConstants.LEADING);
            super.getTableCellRendererComponent(
                    table, value, isSelected, hasFocus, row, column);
            int r = table.convertRowIndexToModel(row);
            int c = table.convertColumnIndexToModel(column);
            DatenDownload datenDownload = (DatenDownload) table.getModel().getValueAt(r, DatenDownload.DOWNLOAD_REF_NR);
            if (c == DatenDownload.DOWNLOAD_PROGRESS_NR) {
                setHorizontalAlignment(SwingConstants.CENTER);
                if (datenDownload.start != null) {
                    if (1 < datenDownload.start.percent && datenDownload.start.percent < Start.PROGRESS_FERTIG) {
                        JProgressBar progressBar = new JProgressBar(0, 1000);
                        progressBar.setBorder(BorderFactory.createEmptyBorder());
                        progressBar.setStringPainted(true);
                        JPanel panel = new JPanel(new BorderLayout());
                        panel.setBorder(BorderFactory.createEmptyBorder());
                        panel.add(progressBar);
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
                        setColor(panel, datenDownload.start, isSelected);
                        setColor(progressBar, datenDownload.start, isSelected);
                        progressBar.setValue(datenDownload.start.percent);
                        double d = datenDownload.start.percent / 10.0;
                        progressBar.setString(Double.toString(d) + "%");
                        return panel;
                    } else {
                        this.setText(Start.getTextProgress(datenDownload.start));
                    }
                } else {
                    this.setText("");
                }
            } else if (c == DatenDownload.DOWNLOAD_RESTZEIT_NR) {
                setHorizontalAlignment(SwingConstants.CENTER);
                if (datenDownload.start != null) {
                    if (datenDownload.start.beginnAnschauen) {
                        setForeground(MVColor.DOWNLOAD_ANSEHEN.color);
                    }
                }
            } else if (c == DatenDownload.DOWNLOAD_FILM_NR_NR) {
                if ((Integer) table.getModel().getValueAt(r, DatenDownload.DOWNLOAD_FILM_NR_NR) == 0) {
                    this.setText("");
                }
                setHorizontalAlignment(SwingConstants.CENTER);
            } else if (c == DatenDownload.DOWNLOAD_NR_NR || c == DatenDownload.DOWNLOAD_DATUM_NR
                    || c == DatenDownload.DOWNLOAD_ZEIT_NR || c == DatenDownload.DOWNLOAD_DAUER_NR
                    || c == DatenDownload.DOWNLOAD_BANDBREITE_NR) {
                setHorizontalAlignment(SwingConstants.CENTER);
            } else if (c == DatenDownload.DOWNLOAD_GROESSE_NR) {
                setHorizontalAlignment(SwingConstants.RIGHT);
            } else if (c == DatenDownload.DOWNLOAD_ABO_NR) {
                setHorizontalAlignment(SwingConstants.CENTER);
                if (!datenDownload.arr[DatenDownload.DOWNLOAD_ABO_NR].equals("")) {
                    setForeground(MVColor.DOWNLOAD_IST_ABO.color);
                } else {
                    setForeground(MVColor.DOWNLOAD_IST_DIREKTER_DOWNLOAD.color);
                    setText("Download");
                    //setIcon(GetIcon.getIcon("nein_12.png"));
                }
            } else if (c == DatenDownload.DOWNLOAD_PROGRAMM_RESTART_NR) {
                setHorizontalAlignment(SwingConstants.CENTER);
                if (datenDownload.isRestart()) {
                    setIcon(ja_16);
                } else {
                    setIcon(nein_12);
                }
            } else if (c == DatenDownload.DOWNLOAD_BUTTON_START_NR) {
                setHorizontalAlignment(SwingConstants.CENTER);
                if (isSelected) {
                    if (datenDownload.start != null) {
                        if (datenDownload.start.status == Start.STATUS_FERTIG) {
                            setIcon(film_start_tab);
                            setToolTipText("gespeicherten Film abspielen");
                        } else if (datenDownload.start.status == Start.STATUS_ERR) {
                            setIcon(download_start_tab);
                            setToolTipText("Download starten");
                        } else {
                            setIcon(download_stop_tab);
                            setToolTipText("Download stoppen");
                        }
                    } else {
                        setIcon(download_start_tab);
                        setToolTipText("Download starten");
                    }
                } else {
                    if (datenDownload.start != null) {
                        if (datenDownload.start.status == Start.STATUS_FERTIG) {
                            setIcon(film_start_sw_tab);
                            setToolTipText("gespeicherten Film abspielen");
                        } else if (datenDownload.start.status == Start.STATUS_ERR) {
                            setIcon(download_start_sw_tab);
                            setToolTipText("Download starten");
                        } else {
                            setIcon(download_stop_sw_tab);
                            setToolTipText("Download stoppen");
                        }
                    } else {
                        setIcon(download_start_sw_tab);
                        setToolTipText("Download starten");
                    }
                }
            } else if (c == DatenDownload.DOWNLOAD_BUTTON_DEL_NR) {
                setHorizontalAlignment(SwingConstants.CENTER);
                if (datenDownload.start != null) {
                    if (datenDownload.start.status >= Start.STATUS_FERTIG) {
                        if (isSelected) {
                            setIcon(download_clear_tab);
                            setToolTipText("Download entfernen");
                        } else {
                            setIcon(download_clear_sw_tab);
                            setToolTipText("Download entfernen");
                        }
                    } else if (isSelected) {
                        setIcon(download_del_tab);
                        setToolTipText("Download löschen");
                    } else {
                        setIcon(download_del_sw_tab);
                        setToolTipText("Download löschen");
                    }
                } else if (isSelected) {
                    setIcon(download_del_tab);
                    setToolTipText("Download löschen");
                } else {
                    setIcon(download_del_sw_tab);
                    setToolTipText("Download löschen");
                }
            }
            setColor(this, datenDownload.start, isSelected);
            if (datenDownload.start == null) {
                if (geoMelden) {
                    if (!datenDownload.arr[DatenDownload.DOWNLOAD_GEO_NR].isEmpty()) {
                        if (!datenDownload.arr[DatenDownload.DOWNLOAD_GEO_NR].contains(Daten.mVConfig.get(MVConfig.SYSTEM_GEO_STANDORT))) {
                            //setForeground(GuiKonstanten.FARBE_FILM_GEOBLOCK_FORGROUND);
                            if (isSelected) {
                                setBackground(MVColor.FILM_GEOBLOCK_BACKGROUND_SEL.color);
                            } else {
                                setBackground(MVColor.FILM_GEOBLOCK_BACKGROUND.color);
                            }
                        }
                    }
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(758200166, Log.FEHLER_ART_PROG, this.getClass().getName(), ex);
        }
        if (isSelected) {
            setFont(new java.awt.Font("Dialog", Font.BOLD, 12));
        } else {
            setFont(new java.awt.Font(null));
        }
        return this;
    }

    private void setColor(Component c, Start s, boolean isSelected) {
        if (s != null) {
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
}
