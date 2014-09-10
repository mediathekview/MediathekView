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
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.TimerTask;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import mediathek.daten.Daten;
import mediathek.daten.DownloadInfos;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVConfig;
import mediathek.tool.MVFilmSize;

/**
 * This class will manage and display the download bandwidth chart display.
 */
public class MVBandwidthInfo extends javax.swing.JDialog {

    private double counter = 0; // double sonst "läuft" die Chart nicht
    private JCheckBoxMenuItem menuItem = null;
    private Trace2DLtd m_trace = new Trace2DLtd(300);
    private IAxis x_achse = null;
    private boolean stopBeob = false;
    /**
     * Timer for collecting sample data.
     */
    private final java.util.Timer timer = new java.util.Timer(false);
    private TimerTask timerTask = null;

    /** Creates new form MVBandwidthInfo
     *
     * @param parent
     * @param menuItem */
    public MVBandwidthInfo(JFrame parent, final JCheckBoxMenuItem menuItem) {
        //super((JFrame) null, false);
        super(parent, false);
        initComponents();

        this.menuItem = menuItem;
        setTitle("Bandbreite");
        GuiFunktionen.setSize(MVConfig.SYSTEM_GROESSE_INFODIALOG, this, parent);

        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                menuItem.setSelected(false);
                toggleVisibility();
            }
        });

        Chart2D chart = new Chart2D();
        chart.setPaintLabels(true);
        chart.setUseAntialiasing(true);
        chart.setToolTipType(Chart2D.ToolTipType.VALUE_SNAP_TO_TRACEPOINTS);

        x_achse = chart.getAxisX();
        x_achse.getAxisTitle().setTitle("Minuten");
        x_achse.setPaintScale(true);
        x_achse.setVisible(true);
        x_achse.setPaintGrid(false);
        x_achse.setMajorTickSpacing(10);
        x_achse.setMinorTickSpacing(1);

        IAxis y_achse = chart.getAxisY();
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

        // Slider zum Einstellen der Bandbreite
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_BANDBREITE, MVBandwidthMonitor.class.getSimpleName()) {
            @Override
            public void ping() {
                setSliderBandwith();
            }
        });
        jEditorPaneInfo.setText("");
        jEditorPaneInfo.setEditable(false);
        jEditorPaneInfo.setFocusable(false);
        jEditorPaneInfo.setContentType("text/html");
        jSliderBandwidth.setMajorTickSpacing(10);
        jSliderBandwidth.setMinorTickSpacing(5);
        jSliderBandwidth.setToolTipText("");
        setSliderBandwith();
        jSliderBandwidth.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                if (stopBeob) {
                    return;
                }
                int b = jSliderBandwidth.getValue() * 10;
                jLabelBandwith.setText(b + " kByte/s");
                Daten.mVConfig.add(MVConfig.SYSTEM_BANDBREITE_KBYTE, String.valueOf(b));
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_BANDBREITE, MVBandwidthMonitor.class.getName());
            }
        });
        jSplitPane1.setOneTouchExpandable(true);
    }

    public int getDividerLocation() {
        return jSplitPane1.getDividerLocation();
    }

    public void setDividerLocation(int div) {
        jSplitPane1.setDividerLocation(div);
    }

    private void setSliderBandwith() {
        stopBeob = true;
        int bandbreite;
        try {
            bandbreite = Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_BANDBREITE_KBYTE));
        } catch (Exception ex) {
            bandbreite = 0;
            Daten.mVConfig.add(MVConfig.SYSTEM_BANDBREITE_KBYTE, "0");
        }
        jSliderBandwidth.setValue(bandbreite / 10);
        if (bandbreite == 0) {
            jLabelBandwith.setText("aus");

        } else {
            jLabelBandwith.setText(bandbreite + " kByte/s");
        }
        stopBeob = false;
    }

    /**
     * Show/hide bandwidth display. Take also care about the used timer.
     */
    public void toggleVisibility() {
        final boolean isSelected = menuItem.isSelected();
        Daten.mVConfig.add(MVConfig.SYSTEM_BANDWIDTH_MONITOR_VISIBLE, Boolean.toString(menuItem.isSelected()));
        setVisible(isSelected);
        try {
            if (menuItem.isSelected()) {
                timerTask = new TimerTask() {
                    DownloadInfos di = Daten.listeDownloads.getInfos();

                    @Override
                    public void run() {
                        Daten.listeDownloads.getInfos();

                        counter++;
                        m_trace.addPoint(counter / 60, di.bandwidth); // minutes
                        x_achse.getAxisTitle().setTitle(di.roundBandwidth((long) counter));
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                setInfoText(di);
                            }
                        });
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
            System.out.println(ignored.getMessage());
        }
    }

    private void setInfoText(DownloadInfos di) {
        final String HEAD = "<html xmlns=\"http://www.w3.org/1999/xhtml\"><head>"
                + "<style type=\"text/css\" .sans {font-family: Verdana, Geneva, sans-serif;}</style></head><body>";
        final String END = "</body></html>";

        String info = HEAD;
        info += Daten.listeDownloads.getInfo();
        if (di.timeRestAktDownloads > 0 && di.timeRestAllDownloads > 0) {
            info += "<span class=\"sans\"><b>Restzeit: </b>" + "laufende: " + di.getRestzeit() + ", alle: " + di.getGesamtRestzeit() + "<br /></span>";
        } else if (di.timeRestAktDownloads > 0) {
            info += "<span class=\"sans\"><b>Restzeit: </b>laufende: " + di.getRestzeit() + "<br /></span>";
        } else if (di.timeRestAllDownloads > 0) {
            info += "<span class=\"sans\"><b>Restzeit: </b>alle: " + di.getGesamtRestzeit() + "<br /></span>";
        }

        if (di.byteAlleDownloads > 0 || di.byteAktDownloads > 0) {
            info += "<span class=\"sans\"><b>Größe: </b>";
            if (di.byteAktDownloads > 0) {
                info += MVFilmSize.getGroesse(di.byteAktDownloads) + " von " + MVFilmSize.getGroesse(di.byteAlleDownloads) + " MByte" + "<br /></span>";
            } else {
                info += MVFilmSize.getGroesse(di.byteAlleDownloads) + " MByte" + "<br /></span>";
            }
        }
        if (di.bandwidth > 0) {
            info += "<span class=\"sans\"><b>Bandbreite: </b>";
            info += di.roundBandwidth() + "<br /></span>";
        }
        info += END;
        jEditorPaneInfo.setText(info);
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jSplitPane1 = new javax.swing.JSplitPane();
        jPanelChart = new javax.swing.JPanel();
        jPanelInfo = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jSliderBandwidth = new javax.swing.JSlider();
        jLabelBandwith = new javax.swing.JLabel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jEditorPaneInfo = new javax.swing.JEditorPane();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        jSplitPane1.setDividerLocation(120);
        jSplitPane1.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);

        jPanelChart.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));

        javax.swing.GroupLayout jPanelChartLayout = new javax.swing.GroupLayout(jPanelChart);
        jPanelChart.setLayout(jPanelChartLayout);
        jPanelChartLayout.setHorizontalGroup(
            jPanelChartLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 375, Short.MAX_VALUE)
        );
        jPanelChartLayout.setVerticalGroup(
            jPanelChartLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 115, Short.MAX_VALUE)
        );

        jSplitPane1.setTopComponent(jPanelChart);

        jPanelInfo.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));

        jLabel1.setText("Max:");

        jLabelBandwith.setText("10 kByte/s");

        jEditorPaneInfo.setEditable(false);
        jScrollPane1.setViewportView(jEditorPaneInfo);

        javax.swing.GroupLayout jPanelInfoLayout = new javax.swing.GroupLayout(jPanelInfo);
        jPanelInfo.setLayout(jPanelInfoLayout);
        jPanelInfoLayout.setHorizontalGroup(
            jPanelInfoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelInfoLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelInfoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane1)
                    .addGroup(jPanelInfoLayout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jSliderBandwidth, javax.swing.GroupLayout.DEFAULT_SIZE, 222, Short.MAX_VALUE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabelBandwith)))
                .addContainerGap())
        );
        jPanelInfoLayout.setVerticalGroup(
            jPanelInfoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelInfoLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelInfoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jLabelBandwith)
                    .addGroup(jPanelInfoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addComponent(jSliderBandwidth, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addComponent(jLabel1)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 79, Short.MAX_VALUE)
                .addContainerGap())
        );

        jSplitPane1.setRightComponent(jPanelInfo);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jSplitPane1)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jSplitPane1)
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JEditorPane jEditorPaneInfo;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabelBandwith;
    private javax.swing.JPanel jPanelChart;
    private javax.swing.JPanel jPanelInfo;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSlider jSliderBandwidth;
    private javax.swing.JSplitPane jSplitPane1;
    // End of variables declaration//GEN-END:variables

}
