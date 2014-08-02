package mediathek;

import com.explodingpixels.macwidgets.HudWindow;
import com.jidesoft.utils.SystemInfo;
import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.IAxis;
import info.monitorenter.gui.chart.labelformatters.LabelFormatterMV;
import info.monitorenter.gui.chart.traces.Trace2DLtd;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.text.DecimalFormat;
import java.util.LinkedList;
import java.util.TimerTask;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.WindowConstants;
import mediathek.controller.starter.Start;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;
import mediathek.tool.Funktionen;
import mediathek.tool.MVConfig;

/**
 * This class will manage and display the download bandwidth chart display.
 */
class MVBandwidthMonitor {

    private double counter = 0.0;
    private HudWindow hudWindow = null;
    private JCheckBoxMenuItem menuItem = null;
    private Trace2DLtd m_trace = new Trace2DLtd(1000);

    /**
     * Timer for collecting sample data.
     */
    private java.util.Timer timer = new java.util.Timer(false);

    public MVBandwidthMonitor(JFrame parent, final JCheckBoxMenuItem menuItem) {
        this.menuItem = menuItem;
        if (!SystemInfo.isMacOSX()) {
            parent = null;
        }
        hudWindow = new HudWindow("Bandbreite", parent);
        hudWindow.makeResizeable();

        JDialog hudDialog = hudWindow.getJDialog();

        hudDialog.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        hudDialog.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                menuItem.setSelected(false);
                Daten.mVConfig.add(MVConfig.SYSTEM_ANSICHT_BANDWIDTH, Boolean.toString(menuItem.isSelected()));
            }
        });

        if (Funktionen.getOs() == Funktionen.OS_LINUX) {
            hudDialog.setBackground(null);
            //setup chart display
            Chart2D chart = new Chart2D();
            chart.setOpaque(true);
            chart.setPaintLabels(true);
            chart.setUseAntialiasing(true);
            chart.setToolTipType(Chart2D.ToolTipType.VALUE_SNAP_TO_TRACEPOINTS);

            //setup trace point handling
            m_trace.setColor(Color.RED);
            m_trace.setName("");

            chart.addTrace(m_trace);

            IAxis x_achse = chart.getAxisX();
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
            y_achse.setMajorTickSpacing(10);
            y_achse.setMinorTickSpacing(1);
            y_achse.setFormatter(new LabelFormatterMV());

            JPanel panel = new JPanel();
            panel.setOpaque(true);
            panel.setLayout(new BorderLayout(0, 0));
            panel.add(chart, BorderLayout.CENTER);
            hudWindow.setContentPane(panel);

        } else {
            //setup chart display
            Chart2D chart = new Chart2D();
            chart.setOpaque(false);
            chart.setPaintLabels(false);
            chart.setUseAntialiasing(true);

            //setup trace point handling
            m_trace.setColor(Color.GREEN);
            m_trace.setName("KB/s");

            chart.addTrace(m_trace);

            IAxis x_achse = chart.getAxisX();
            x_achse.getAxisTitle().setTitle("");
            x_achse.setPaintScale(false);
            x_achse.setVisible(false);

            JPanel panel = new JPanel();
            panel.setOpaque(false);
            panel.setLayout(new BorderLayout(0, 0));
            panel.add(chart, BorderLayout.CENTER);
            hudWindow.setContentPane(panel);

            chart.removeAxisYLeft(chart.getAxisY());
        }
        final Dimension dim = hudDialog.getSize();
        dim.height = 150;
        dim.width = 300;
        hudDialog.setSize(dim);
    }

    /**
     * Show/hide bandwidth display. Take also care about the used timer.
     */
    public void toggleVisibility() {
        final boolean isSelected = menuItem.isSelected();
        Daten.mVConfig.add(MVConfig.SYSTEM_ANSICHT_BANDWIDTH, Boolean.toString(menuItem.isSelected()));
        hudWindow.getJDialog().setVisible(isSelected);
        try {
            if (menuItem.isSelected()) {
                TimerTask task = new TimerTask() {
                    @Override
                    public void run() {
                        double bandwidth = 0.0;
                        //only count running/active downloads and calc accumulated progress..
                        LinkedList<DatenDownload> activeDownloadList = Daten.listeDownloads.getListOfStartsNotFinished(Start.QUELLE_ALLE);
                        for (DatenDownload download : activeDownloadList) {
                            if (download.start != null && download.start.status == Start.STATUS_RUN) {
                                bandwidth += download.start.bandbreite;
                            }
                        }
                        activeDownloadList.clear();

                        if (bandwidth < 0.0) {
                            bandwidth = 0.0;
                        }

//                        if (bandwidth > 0.0) {
//                            bandwidth /= 1024.0; // convert to KByte
//                        }
                        counter++;

                        //m_trace.addPoint(counter / 60, roundBandwidth(bandwidth)); // minutes
                        //m_trace.addPoint(counter / 60, roundBandwidth(Daten.guiDebug.getJSpinner().getValue() + counter)); // minutes
                        if (counter % 300 == 0) {
                            m_trace.addPoint(counter / 60, Daten.guiDebug.getJSpinner().getValue() * 0.95); // minutes
                        } else {
                            m_trace.addPoint(counter / 60, Daten.guiDebug.getJSpinner().getValue()); // minutes
                        }                        //m_trace.addPoint(counter / 60, 199521); // minutes
                    }
                };
                if (Daten.debug) {
                    timer.schedule(task, 0, 10);
                } else {
                    timer.schedule(task, 0, 1000);
                }
            } else {
                timer.purge();
            }
        } catch (IllegalStateException ignored) {
            System.out.println(ignored.getMessage());
        }
    }

    private double roundBandwidth(double d) {
        int i = 0;
        while (d > 100) {
            ++i;
            d /= 10;
        }
        d = Math.round(d);
        d = d * Math.pow(10, i);
        return d;
    }
//    private double roundBandwidth(double d) {
//        int i = 0;
//        while (d > 100) {
//            ++i;
//            d /= 10;
//        }
//        d = Math.round(d);
//        d = d * Math.pow(10, i);
//        return d;
//    }
}
