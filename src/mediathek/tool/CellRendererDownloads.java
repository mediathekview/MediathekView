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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.plaf.basic.BasicProgressBarUI;
import javax.swing.table.DefaultTableCellRenderer;
import mediathek.controller.io.starter.Start;
import mediathek.daten.DDaten;
import mediathek.daten.DatenDownload;

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
            setHorizontalAlignment(SwingConstants.LEADING);
            super.getTableCellRendererComponent(
                    table, value, isSelected, hasFocus, row, column);
            int r = table.convertRowIndexToModel(row);
            int c = table.convertColumnIndexToModel(column);
            String url = table.getModel().getValueAt(r, DatenDownload.DOWNLOAD_URL_NR).toString();
            // Abos
            boolean abo = !table.getModel().getValueAt(r, DatenDownload.DOWNLOAD_ABO_NR).equals("");
            // Starts
            Start s = ddaten.starterClass.getStart(url);
            if (s != null) {
                setColor(this, s, isSelected);
                if (c == DatenDownload.DOWNLOAD_RESTZEIT_NR) {
                    if (s.restSekunden > 0) {
                        if (s.restSekunden < 60) {
                            this.setText("< 1 Min.");
                        } else {
                            this.setText(Long.toString(s.restSekunden / 60) + " Min.");
                        }
                    } else {
                        this.setText("");
                    }
                } else if (c == DatenDownload.DOWNLOAD_PROGRESS_NR) {
                    int i = Integer.parseInt(s.datenDownload.arr[DatenDownload.DOWNLOAD_PROGRESS_NR]);
                    setHorizontalAlignment(SwingConstants.CENTER);
                    if (i == -1) {
                        // noch nicht gestartet
                        this.setText("");
                    } else if (i == DatenDownload.PROGRESS_WARTEN) {
                        this.setText("warten");
                    } else if (i == DatenDownload.PROGRESS_GESTARTET) {
                        this.setText("gestartet");
                    } else if (1 < i && i < DatenDownload.PROGRESS_FERTIG) {
//////                        return new ProgressPanel(s).progressPanel();
                        JProgressBar progressBar = new JProgressBar(0, 1000);
                        JPanel panel = new JPanel(new BorderLayout());
                        setColor(panel, s, isSelected);
                        setColor(progressBar, s, isSelected);
                        progressBar.setBorder(BorderFactory.createEmptyBorder());
                        progressBar.setStringPainted(true);
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
                        panel.add(progressBar);
                        panel.setBorder(BorderFactory.createEmptyBorder());
                        progressBar.setValue(i);
                        double d = i / 10.0;
                        progressBar.setString(Double.toString(d) + "%");
                        return panel;
                    } else if (i == DatenDownload.PROGRESS_FERTIG) {
                        if (s != null) {
                            if (s.status == Start.STATUS_ERR) {
                                this.setText("fehlerhaft");
                            } else {
                                this.setText("fertig");
                            }
                        } else {
                            this.setText("fertig");
                        }
                    }
                }
            } else {
                if (c == DatenDownload.DOWNLOAD_PROGRESS_NR) {
                    this.setText("");
                }
            }
            if (c == DatenDownload.DOWNLOAD_ABO_NR) {
                setFont(new java.awt.Font("Dialog", Font.BOLD, 12));
                if (abo) {
                    setForeground(GuiKonstanten.ABO_FOREGROUND);
                } else {
                    setForeground(GuiKonstanten.DOWNLOAD_FOREGROUND);
                    setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/nein_12.png")));
                    setHorizontalAlignment(SwingConstants.CENTER);
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
            Log.fehlerMeldung(758200166, Log.FEHLER_ART_PROG, this.getClass().getName(), ex);
        }
        return this;
    }

    private void setColor(Component c, Start s, boolean isSelected) {
        switch (s.status) {
            case Start.STATUS_INIT:
                if (isSelected) {
                    c.setBackground(GuiKonstanten.DOWNLOAD_FARBE_WAIT_SEL);
                } else {
                    c.setBackground(GuiKonstanten.DOWNLOAD_FARBE_WAIT);
                }
                break;
            case Start.STATUS_RUN:
                if (isSelected) {
                    c.setBackground(GuiKonstanten.DOWNLOAD_FARBE_RUN_SEL);
                } else {
                    c.setBackground(GuiKonstanten.DOWNLOAD_FARBE_RUN);
                }
                break;
            case Start.STATUS_FERTIG:
                if (isSelected) {
                    c.setBackground(GuiKonstanten.DOWNLOAD_FARBE_FERTIG_SEL);
                } else {
                    c.setBackground(GuiKonstanten.DOWNLOAD_FARBE_FERTIG);
                }
                break;
            case Start.STATUS_ERR:
                if (isSelected) {
                    c.setBackground(GuiKonstanten.DOWNLOAD_FARBE_ERR_SEL);
                } else {
                    c.setBackground(GuiKonstanten.DOWNLOAD_FARBE_ERR);
                }
                break;
        }
    }
//    private class ProgressPanel extends JPanel {
//
//        private JPanel panel = new JPanel(new BorderLayout());
//        private Start s = null;
//        private int i = 0;
//        private JProgressBar progressBar = new JProgressBar(0, 1000);
//
//        public ProgressPanel(Start s_) {
//            s = s_;
//            ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_ART_DOWNLOAD_PROZENT, CellRendererDownloads.class.getSimpleName()) {
//                @Override
//                public void ping() {
//                    panelUpdate();
//                }
//            });
//            setHorizontalAlignment(SwingConstants.CENTER);
//            // JProgressBar progressBar = new JProgressBar(0, 1000);
//            // JPanel panel = new JPanel(new BorderLayout());
//            // setColor(panel, s, isSelected);
//            // setColor(progressBar, s, isSelected);
//            progressBar.setBorder(BorderFactory.createEmptyBorder());
//            progressBar.setStringPainted(true);
//            progressBar.setUI(new BasicProgressBarUI() {
//                @Override
//                protected Color getSelectionBackground() {
//                    return UIManager.getDefaults().getColor("Table.foreground");
//                }
//
//                @Override
//                protected Color getSelectionForeground() {
//                    return Color.white;
//                }
//            });
//            panel.add(progressBar);
//            panel.setBorder(BorderFactory.createEmptyBorder());
//        }
//
//        public JPanel progressPanel() {
//            return panel;
//        }
//
//        private void panelUpdate() {
//            if (s != null) {
//                i = Integer.parseInt(s.datenDownload.arr[DatenDownload.DOWNLOAD_PROGRESS_NR]);
//                progressBar.setValue(i);
//                double d = i / 10.0;
//                progressBar.setString(Double.toString(d) + "%");
//                this.updateUI();
//            }
//        }
//    }
}
