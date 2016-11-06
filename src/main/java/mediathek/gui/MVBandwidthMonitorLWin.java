/*
 * MediathekView
 * Copyright (C) 2014 W. Xaver
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
package mediathek.gui;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.IAxis;
import info.monitorenter.gui.chart.labelformatters.LabelFormatterAutoUnits;
import info.monitorenter.gui.chart.rangepolicies.RangePolicyForcedPoint;
import info.monitorenter.gui.chart.traces.Trace2DLtd;
import java.awt.*;
import java.awt.event.*;
import java.util.TimerTask;
import javax.swing.*;
import mSearch.tool.DbgMsg;
import mSearch.tool.Functions.OperatingSystemType;
import static mSearch.tool.Functions.getOs;
import mSearch.tool.Listener;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.controller.MVBandwidthTokenBucket;
import mediathek.tool.GuiFunktionen;

public class MVBandwidthMonitorLWin extends javax.swing.JPanel {

    private static final long serialVersionUID = 1L;

    private double counter = 0; // double sonst "läuft" die Chart nicht
    private Trace2DLtd m_trace = new Trace2DLtd(300);
    private IAxis<?> x_achse = null;
    private boolean stopBeob = false;
    private JDialog jDialog = null;
    private JFrame parent = null;
    private static Point mouseDownCompCoords;
    private JPanel panel;

    /**
     * Timer for collecting sample data.
     */
    private final java.util.Timer timer = new java.util.Timer(false);
    private TimerTask timerTask = null;

    /** Creates new form MVBandwidthInfo_
     *
     * @param parent */
    public MVBandwidthMonitorLWin(JFrame parent) {
        initComponents();
        this.parent = parent;
        this.panel = this;
        jDialog = new JDialog(MVConfig.getBool(MVConfig.Configs.SYSTEM_DOWNLOAD_INFO_TOP) ? parent : (Frame) null, "Bandbreite");
        jDialog.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        jDialog.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                beenden();
            }
        });

        Chart2D chart = new Chart2D();
        chart.setPaintLabels(true);
        chart.setUseAntialiasing(true);
        chart.setToolTipType(Chart2D.ToolTipType.VALUE_SNAP_TO_TRACEPOINTS);
        if (getOs() == OperatingSystemType.LINUX) {
            jDialog.setBackground(null);
            chart.setOpaque(true);
            this.setOpaque(true);
        } else {
            //a transparent chart is a HUGE GPU performance killer and will BURN GPU resources :(
            //panel.setOpaque(false);
            this.setBackground(Color.WHITE);
        }

        x_achse = chart.getAxisX();
        x_achse.getAxisTitle().setTitle("Minuten");
        x_achse.setPaintScale(true);
        x_achse.setVisible(true);
        x_achse.setPaintGrid(false);
        x_achse.setMajorTickSpacing(10);
        x_achse.setMinorTickSpacing(1);

        IAxis<?> y_achse = chart.getAxisY();
        y_achse.getAxisTitle().setTitle("");
        y_achse.setPaintScale(true);
        y_achse.setVisible(true);
        y_achse.setPaintGrid(true);
        y_achse.setMajorTickSpacing(5);
        y_achse.setMinorTickSpacing(1);
        y_achse.setFormatter(new LabelFormatterAutoUnits());
        y_achse.setRangePolicy(new RangePolicyForcedPoint());

        m_trace.setName("");
        m_trace.setColor(Color.RED);
        chart.addTrace(m_trace);
        jPanelChart.setBackground(Color.WHITE);
        jPanelChart.setLayout(new BorderLayout(0, 0));
        jPanelChart.add(chart, BorderLayout.CENTER);

        Listener.addListener(new Listener(Listener.EREIGNIS_BANDWIDTH_MONITOR, MVBandwidthMonitorLWin.class.getSimpleName()) {
            @Override
            public void ping() {
                setVisibility();
            }
        });

        jDialog.setContentPane(this);

        // size
        jPanelChart.setMinimumSize(new Dimension());
        if (!GuiFunktionen.setSize(MVConfig.Configs.SYSTEM_GROESSE_INFODIALOG, jDialog, parent)) {
            // erster Programmstart
            final Dimension dim = jDialog.getSize();
            dim.height = 250;
            dim.width = 400;
            jDialog.setSize(dim);
        }
        BeobMaus bom = new BeobMaus();
        chart.addMouseListener(bom);

        mouseDownCompCoords = null;
        chart.addMouseListener(new MouseListener() {
            @Override
            public void mouseReleased(MouseEvent e) {
                mouseDownCompCoords = null;
            }

            @Override
            public void mousePressed(MouseEvent e) {
                mouseDownCompCoords = e.getPoint();
            }

            @Override
            public void mouseExited(MouseEvent e) {
            }

            @Override
            public void mouseEntered(MouseEvent e) {
            }

            @Override
            public void mouseClicked(MouseEvent e) {
            }
        });
        chart.addMouseMotionListener(new MouseMotionListener() {
            @Override
            public void mouseMoved(MouseEvent e) {
            }

            @Override
            public void mouseDragged(MouseEvent e) {
                Point currCoords = e.getLocationOnScreen();
                jDialog.setLocation(currCoords.x - mouseDownCompCoords.x, currCoords.y - mouseDownCompCoords.y);
            }
        });

        GuiFunktionen.setDialogDecorated(jDialog, this, MVConfig.getBool(MVConfig.Configs.SYSTEM_DOWNLOAD_INFO_DECORATED));
        setVisibility();
    }

    private void beenden() {
        MVConfig.add(MVConfig.Configs.SYSTEM_BANDWIDTH_MONITOR_VISIBLE, Boolean.toString(false));
        Listener.notify(Listener.EREIGNIS_BANDWIDTH_MONITOR, MVBandwidthMonitorLWin.class.getSimpleName());
        setVisibility();
    }

    public JDialog getDialog() {
        return jDialog;
    }

    public static void setSliderBandwith(JSlider slider) {
        int bandbreiteKByte;
        String ret;
        try {
            bandbreiteKByte = Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_BANDBREITE_KBYTE));
        } catch (Exception ex) {
            bandbreiteKByte = MVBandwidthTokenBucket.BANDWIDTH_MAX_KBYTE;
            MVConfig.add(MVConfig.Configs.SYSTEM_BANDBREITE_KBYTE, MVBandwidthTokenBucket.BANDWIDTH_MAX_KBYTE + "");
        }
        slider.setValue(bandbreiteKByte / 10);
    }

    public static String setTextBandwith(String txtBefore, JLabel label, JTextField txt) {
        int bandbreiteKByte;
        String ret;
        try {
            bandbreiteKByte = Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_BANDBREITE_KBYTE));
        } catch (Exception ex) {
            bandbreiteKByte = MVBandwidthTokenBucket.BANDWIDTH_MAX_KBYTE;
            MVConfig.add(MVConfig.Configs.SYSTEM_BANDBREITE_KBYTE, MVBandwidthTokenBucket.BANDWIDTH_MAX_KBYTE + "");
        }
        if (bandbreiteKByte == MVBandwidthTokenBucket.BANDWIDTH_MAX_KBYTE) {
            ret = "aus";
        } else {
            ret = bandbreiteKByte + " kByte/s";
        }
        if (label != null) {
            label.setText(txtBefore + ret);
            if (bandbreiteKByte > MVBandwidthTokenBucket.BANDWIDTH_MAX_RED_KBYTE) {
                label.setForeground(Color.red);
            } else {
                label.setForeground(Color.black);
            }
        }
        if (txt != null) {
            txt.setText(txtBefore + ret);
            if (bandbreiteKByte > MVBandwidthTokenBucket.BANDWIDTH_MAX_RED_KBYTE) {
                txt.setForeground(Color.red);
            } else {
                txt.setForeground(Color.black);
            }
        }
        return ret;
    }

    /**
     * Show/hide bandwidth display. Take also care about the used timer.
     */
    private void setVisibility() {
        final boolean isVis = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BANDWIDTH_MONITOR_VISIBLE));
        jDialog.setVisible(isVis);
        try {
            if (isVis) {
                timerTask = new TimerTask() {

                    @Override
                    public void run() {
                        counter++;
                        m_trace.addPoint(counter / 60, Daten.downloadInfos.bandwidth); // minutes
                        x_achse.getAxisTitle().setTitle(Daten.downloadInfos.roundBandwidth((long) counter));
//                        SwingUtilities.invokeLater(() -> setInfoText(Daten.downloadInfos));
                    }
                };
                timer.schedule(timerTask, 0, 1_000);
            } else {
                if (timerTask != null) {
                    timerTask.cancel();
                }
                timer.purge();
            }
        } catch (IllegalStateException ignored) {
            DbgMsg.print(ignored.getMessage());
        }
        if (!isVis) {
            jDialog.dispose();
        }
    }

//    public static String setInfoText(DownloadInfos di) {
//        final String HEAD = "<html xmlns=\"http://www.w3.org/1999/xhtml\"><head>"
//                + "<style type=\"text/css\" .sans {font-family: Verdana, Geneva, sans-serif;}</style></head><body>";
//        final String END = "</body></html>";
//
//        String info = HEAD;
//        info += getInfoText();
//        if (di.timeRestAktDownloads > 0 && di.timeRestAllDownloads > 0) {
//            info += "<span class=\"sans\"><b>Restzeit: </b>" + "laufende: " + di.getRestzeit() + ", alle: " + di.getGesamtRestzeit() + "<br /></span>";
//        } else if (di.timeRestAktDownloads > 0) {
//            info += "<span class=\"sans\"><b>Restzeit: </b>laufende: " + di.getRestzeit() + "<br /></span>";
//        } else if (di.timeRestAllDownloads > 0) {
//            info += "<span class=\"sans\"><b>Restzeit: </b>alle: " + di.getGesamtRestzeit() + "<br /></span>";
//        }
//
//        if (di.byteAlleDownloads > 0 || di.byteAktDownloads > 0) {
//            info += "<span class=\"sans\"><b>Größe: </b>";
//            if (di.byteAktDownloads > 0) {
//                info += MVFilmSize.getGroesse(di.byteAktDownloads) + " von " + MVFilmSize.getGroesse(di.byteAlleDownloads) + " MByte" + "<br /></span>";
//            } else {
//                info += MVFilmSize.getGroesse(di.byteAlleDownloads) + " MByte" + "<br /></span>";
//            }
//        }
//        if (di.bandwidth > 0) {
//            info += "<span class=\"sans\"><b>Bandbreite: </b>";
//            info += di.bandwidthStr + "<br /></span>";
//        }
//        info += END;
//        return info;
//    }
//
//    private static String getInfoText() {
//        String textLinks;
//        // Text links: Zeilen Tabelle
//        // nicht gestarted, laufen, fertig OK, fertig fehler
//        int[] starts = Daten.downloadInfos.downloadStarts;
//        if (starts[0] == 1) {
//            textLinks = "<span class=\"sans\"><b>Download:</b> 1";
//        } else {
//            textLinks = "<span class=\"sans\"><b>Downloads:</b> " + starts[0];
//        }
//        boolean print = false;
//        for (int ii = 1; ii < starts.length; ++ii) {
//            if (starts[ii] > 0) {
//                print = true;
//                break;
//            }
//        }
//        if (print) {
//            textLinks += "&nbsp;&nbsp;( ";
//            if (starts[4] == 1) {
//                textLinks += "1 läuft";
//            } else {
//                textLinks += starts[4] + " laufen";
//            }
//            if (starts[3] == 1) {
//                textLinks += ", 1 wartet";
//            } else {
//                textLinks += ", " + starts[3] + " warten";
//            }
//            if (starts[5] > 0) {
//                if (starts[5] == 1) {
//                    textLinks += ", 1 fertig";
//                } else {
//                    textLinks += ", " + starts[5] + " fertig";
//                }
//            }
//            if (starts[6] > 0) {
//                if (starts[6] == 1) {
//                    textLinks += ", 1 fehlerhaft";
//                } else {
//                    textLinks += ", " + starts[6] + " fehlerhaft";
//                }
//            }
//            textLinks += " )";
//        }
//        textLinks += "<br /></span>";
//        return textLinks;
//    }

    private class BeobMaus extends MouseAdapter {

        JCheckBox cbkTop = new JCheckBox("Immer im Vordergrund");
        JCheckBox cbkBorder = new JCheckBox("Rand anzeigen");
        JMenuItem itemClose = new JMenuItem("Ausblenden");
        JSlider jSliderBandwidth = new JSlider();
        JLabel lblBandwidth = new JLabel("Bandbreite");

        public BeobMaus() {
            cbkTop.setSelected(MVConfig.getBool(MVConfig.Configs.SYSTEM_DOWNLOAD_INFO_TOP));
            cbkTop.addActionListener(l -> {
                MVConfig.add(MVConfig.Configs.SYSTEM_DOWNLOAD_INFO_TOP, Boolean.toString(cbkTop.isSelected()));
                GuiFunktionen.setParent(jDialog, MVConfig.getBool(MVConfig.Configs.SYSTEM_DOWNLOAD_INFO_TOP) ? parent : (Frame) null);
            });
            cbkBorder.setSelected(MVConfig.getBool(MVConfig.Configs.SYSTEM_DOWNLOAD_INFO_DECORATED));
            cbkBorder.addActionListener(l -> {
                MVConfig.add(MVConfig.Configs.SYSTEM_DOWNLOAD_INFO_DECORATED, Boolean.toString(cbkBorder.isSelected()));
                GuiFunktionen.setDialogDecorated(jDialog, panel, MVConfig.getBool(MVConfig.Configs.SYSTEM_DOWNLOAD_INFO_DECORATED));
            });
            itemClose.addActionListener(l -> beenden());

            jSliderBandwidth.setMinimum(5); //50 kByte/s
            jSliderBandwidth.setMaximum(100); //1_000 kByte/s
            jSliderBandwidth.setToolTipText("");
            setSlider();
            jSliderBandwidth.addChangeListener(e -> {
                if (stopBeob) {
                    return;
                }
                int b = jSliderBandwidth.getValue() * 10;
                MVConfig.add(MVConfig.Configs.SYSTEM_BANDBREITE_KBYTE, String.valueOf(b));
                Listener.notify(Listener.EREIGNIS_BANDBREITE, MVBandwidthMonitorLWin.class.getName());
                setTextBandwith("Bandbreite: ", lblBandwidth, null);
            });
//            // Slider zum Einstellen der Bandbreite
//            Listener.addListener(new Listener(Listener.EREIGNIS_BANDBREITE, MVBandwidthMonitorLWin.class.getSimpleName()) {
//                @Override
//                public void ping() {
//                    setSlider();
//                }
//            });
        }

        private void setSlider() {
            stopBeob = true;
            setSliderBandwith(jSliderBandwidth);
            setTextBandwith("Bandbreite: ", lblBandwidth, null);
            stopBeob = false;

        }

        @Override
        public void mousePressed(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        @Override
        public void mouseReleased(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        private void showMenu(MouseEvent evt) {
            JPopupMenu jPopupMenu = new JPopupMenu();

            jPopupMenu.add(lblBandwidth);
            jPopupMenu.add(jSliderBandwidth);
            jPopupMenu.addSeparator();
            jPopupMenu.add(cbkTop);
            jPopupMenu.add(cbkBorder);
            jPopupMenu.addSeparator();
            jPopupMenu.add(itemClose);

            //anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jPanelChart = new javax.swing.JPanel();

        jPanelChart.setPreferredSize(new java.awt.Dimension(32767, 32767));

        javax.swing.GroupLayout jPanelChartLayout = new javax.swing.GroupLayout(jPanelChart);
        jPanelChart.setLayout(jPanelChartLayout);
        jPanelChartLayout.setHorizontalGroup(
            jPanelChartLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 400, Short.MAX_VALUE)
        );
        jPanelChartLayout.setVerticalGroup(
            jPanelChartLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 250, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanelChart, javax.swing.GroupLayout.DEFAULT_SIZE, 400, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanelChart, javax.swing.GroupLayout.DEFAULT_SIZE, 250, Short.MAX_VALUE)
        );
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel jPanelChart;
    // End of variables declaration//GEN-END:variables
}
