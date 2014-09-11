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

import com.explodingpixels.macwidgets.HudWindow;
import com.jidesoft.utils.SystemInfo;
import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.IAxis;
import info.monitorenter.gui.chart.labelformatters.LabelFormatterAutoUnits;
import info.monitorenter.gui.chart.rangepolicies.RangePolicyForcedPoint;
import info.monitorenter.gui.chart.traces.Trace2DLtd;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.TimerTask;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.basic.BasicSplitPaneDivider;
import javax.swing.plaf.basic.BasicSplitPaneUI;
import mediathek.daten.Daten;
import static mediathek.daten.Daten.mVConfig;
import mediathek.daten.DownloadInfos;
import mediathek.tool.Funktionen;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVConfig;
import mediathek.tool.MVFilmSize;

public class MVDownloadInfo extends javax.swing.JPanel {

    private double counter = 0; // double sonst "läuft" die Chart nicht
    private JCheckBoxMenuItem menuItem = null;
    private Trace2DLtd m_trace = new Trace2DLtd(300);
    private IAxis x_achse = null;
    private boolean stopBeob = false;
    private HudWindow hudWindow = null;
    private JDialog jDialog = null;
    private JFrame parent = null;
    /**
     * Timer for collecting sample data.
     */
    private final java.util.Timer timer = new java.util.Timer(false);
    private TimerTask timerTask = null;

    /** Creates new form MVBandwidthInfo_
     *
     * @param parent
     * @param menuItem */
    public MVDownloadInfo(JFrame parent, final JCheckBoxMenuItem menuItem) {
        initComponents();
        this.parent = parent;
        this.menuItem = menuItem;
//        if (!SystemInfo.isMacOSX()) {
//            parent = null;
//        }
        if (!SystemInfo.isMacOSX()) {
            jDialog = new JDialog(parent, "Bandbreite");
        } else {
            hudWindow = new HudWindow("Bandbreite", parent);
            hudWindow.makeResizeable();
            jDialog = hudWindow.getJDialog();
        }
        jDialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        jDialog.addWindowListener(new WindowAdapter() {
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
        if (Funktionen.getOs() == Funktionen.OperatingSystemType.LINUX) {
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
                jLabelBandwidth.setText(b + " kByte/s");
                Daten.mVConfig.add(MVConfig.SYSTEM_BANDBREITE_KBYTE, String.valueOf(b));
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_BANDBREITE, MVBandwidthMonitor.class.getName());
            }
        });

        if (!SystemInfo.isMacOSX()) {
            jDialog.setContentPane(this);
        } else {
            hudWindow.setContentPane(this);
        }

        jSplitPane1.setDividerSize(15);
        // size
        jPanelChart.setMinimumSize(new Dimension());
        jPanelInfo.setMinimumSize(new Dimension());
        if (GuiFunktionen.setSize(MVConfig.SYSTEM_GROESSE_INFODIALOG, jDialog, parent)) {
            try {
                if (Daten.mVConfig.get(MVConfig.SYSTEM_DIVIDER_INFODIALOG_MIN_MAX).equals("max")) {
                    addHListener(1.0);
                } else if (Daten.mVConfig.get(MVConfig.SYSTEM_DIVIDER_INFODIALOG_MIN_MAX).equals("min")) {
                    addHListener(0.0);
                } else {
                    int divider = Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_DIVIDER_INFODIALOG));
                    //System.out.println("Divider: " + divider);
                    jSplitPane1.setDividerLocation(divider);
                }
            } catch (Exception ignored) {
            }
        } else {
            // erster Programmstart
            final Dimension dim = jDialog.getSize();
            dim.height = 170;
            dim.width = 300;
            jDialog.setSize(dim);
            jSplitPane1.setDividerLocation(200);
            addHListener(1.0);
        }
    }

    private void addHListener(double div) {
        final double d = div;

        jSplitPane1.addHierarchyListener(new HierarchyListener() {
            @Override
            public void hierarchyChanged(HierarchyEvent e) {
                if ((e.getChangeFlags() & HierarchyEvent.SHOWING_CHANGED) != 0) {
                    jSplitPane1.setDividerLocation(d);
                    if (d > 0 && d < 1) {
                        //jSplitPane1.setDividerLocation(d);
                    } else {
                        BasicSplitPaneUI ui = (BasicSplitPaneUI) jSplitPane1.getUI();
                        BasicSplitPaneDivider divider = ui.getDivider();
                        JButton button = (JButton) divider.getComponent(d == 0 ? 0 : 1);
                        button.doClick();
                    }
                }
            }
        });
    }

    public JDialog getDialog() {
        return jDialog;
    }

    public double getDividerLocation() {
        jPanelChart.setMinimumSize(new Dimension());
        jPanelInfo.setMinimumSize(new Dimension()); // nur dann ist der Divider zwischen 1...MAX
        final double MIN = jSplitPane1.getMinimumDividerLocation(); // 1
        final double MAX = jSplitPane1.getMaximumDividerLocation(); // MAX
        final double akt = jSplitPane1.getDividerLocation();        // akt Pos zwischen 1 .... MAX

        double divider = (akt - MIN) / (MAX - MIN);
        if (divider < 0) {
            divider = 0.0;
        } else if (divider > 1) {
            divider = 1.0;
        }

        if (divider == 0) {
            mVConfig.add(MVConfig.SYSTEM_DIVIDER_INFODIALOG_MIN_MAX, "min");
        } else if (divider == 1) {
            mVConfig.add(MVConfig.SYSTEM_DIVIDER_INFODIALOG_MIN_MAX, "max");
        } else {
            mVConfig.add(MVConfig.SYSTEM_DIVIDER_INFODIALOG_MIN_MAX, "");
        }
        Daten.mVConfig.add(MVConfig.SYSTEM_DIVIDER_INFODIALOG, String.valueOf(jSplitPane1.getDividerLocation()));

        return divider;
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
            jLabelBandwidth.setText("aus");

        } else {
            jLabelBandwidth.setText(bandbreite + " kByte/s");
        }
        stopBeob = false;
    }

    /**
     * Show/hide bandwidth display. Take also care about the used timer.
     */
    public void toggleVisibility() {
        final boolean isSelected = menuItem.isSelected();
        Daten.mVConfig.add(MVConfig.SYSTEM_BANDWIDTH_MONITOR_VISIBLE, Boolean.toString(menuItem.isSelected()));
        jDialog.setVisible(isSelected);
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

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jSplitPane1 = new javax.swing.JSplitPane();
        jPanelChart = new javax.swing.JPanel();
        jPanelInfo = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jSliderBandwidth = new javax.swing.JSlider();
        jLabelBandwidth = new javax.swing.JLabel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jEditorPaneInfo = new javax.swing.JEditorPane();

        jSplitPane1.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane1.setOneTouchExpandable(true);

        javax.swing.GroupLayout jPanelChartLayout = new javax.swing.GroupLayout(jPanelChart);
        jPanelChart.setLayout(jPanelChartLayout);
        jPanelChartLayout.setHorizontalGroup(
            jPanelChartLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 398, Short.MAX_VALUE)
        );
        jPanelChartLayout.setVerticalGroup(
            jPanelChartLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 100, Short.MAX_VALUE)
        );

        jSplitPane1.setTopComponent(jPanelChart);

        jLabel1.setText("Max:");

        jLabelBandwidth.setText("10 kByte/s");

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
                        .addComponent(jSliderBandwidth, javax.swing.GroupLayout.DEFAULT_SIZE, 245, Short.MAX_VALUE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabelBandwidth)))
                .addContainerGap())
        );
        jPanelInfoLayout.setVerticalGroup(
            jPanelInfoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelInfoLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelInfoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jLabelBandwidth)
                    .addGroup(jPanelInfoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addComponent(jSliderBandwidth, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addComponent(jLabel1)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 136, Short.MAX_VALUE)
                .addContainerGap())
        );

        jSplitPane1.setRightComponent(jPanelInfo);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jSplitPane1)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jSplitPane1)
        );
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JEditorPane jEditorPaneInfo;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabelBandwidth;
    private javax.swing.JPanel jPanelChart;
    private javax.swing.JPanel jPanelInfo;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSlider jSliderBandwidth;
    private javax.swing.JSplitPane jSplitPane1;
    // End of variables declaration//GEN-END:variables
}
