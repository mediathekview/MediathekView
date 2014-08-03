package mediathek;

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

    private long counter = 0;
    private HudWindow hudWindow = null;
    private JCheckBoxMenuItem menuItem = null;
    private Chart2D chart = new Chart2D();
    private Trace2DLtd m_trace = new Trace2DLtd(100);
    private IAxis x_achse = null;
    private IAxis y_achse = null;

    /**
     * Timer for collecting sample data.
     */
    private java.util.Timer timer = new java.util.Timer(false);
    TimerTask timerTask = null;

    public MVBandwidthMonitor(JFrame parent, final JCheckBoxMenuItem menuItem) {
        this.menuItem = menuItem;
        if (!SystemInfo.isMacOSX()) {
            parent = null;
        }
        hudWindow = new HudWindow("Bandbreite", parent);
        hudWindow.makeResizeable();

        JDialog hudDialog = hudWindow.getJDialog();

        hudDialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        hudDialog.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                menuItem.setSelected(false);
                toggleVisibility();
            }
        });

        JPanel panel = new JPanel();
        chart.setPaintLabels(true);
        chart.setUseAntialiasing(true);
        chart.setToolTipType(Chart2D.ToolTipType.VALUE_SNAP_TO_TRACEPOINTS);
        if (Funktionen.getOs() == Funktionen.OS_LINUX) {
            hudDialog.setBackground(null);
            chart.setOpaque(true);
            m_trace.setColor(Color.RED);
            panel.setOpaque(true);
        } else {
            chart.setOpaque(false);
            m_trace.setColor(Color.GREEN);
            panel.setOpaque(false);
        }
        x_achse = chart.getAxisX();
        x_achse.getAxisTitle().setTitle("Minuten");
        x_achse.setPaintScale(true);
        x_achse.setVisible(true);
        x_achse.setPaintGrid(false);
        x_achse.setMajorTickSpacing(10);
        x_achse.setMinorTickSpacing(1);

        y_achse = chart.getAxisY();
        y_achse.getAxisTitle().setTitle("");
        y_achse.setPaintScale(true);
        y_achse.setVisible(true);
        y_achse.setPaintGrid(true);
        y_achse.setMajorTickSpacing(5);
        y_achse.setMinorTickSpacing(1);
        y_achse.setFormatter(new LabelFormatterAutoUnits());
        y_achse.setRangePolicy(new RangePolicyForcedPoint());

        m_trace.setName("");
        chart.addTrace(m_trace);
        panel.setLayout(new BorderLayout(0, 0));
        panel.add(chart, BorderLayout.CENTER);
        hudWindow.setContentPane(panel);
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
                timerTask = new TimerTask() {
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

                        counter++;

//                        m_trace.addPoint(counter, Daten.guiDebug.getJSpinner().getValue()); // minutes
//                        x_achse.getAxisTitle().setTitle(roundBandwidth(Daten.guiDebug.getJSpinner().getValue(), counter));
                        m_trace.addPoint(counter, bandwidth); // minutes
                        x_achse.getAxisTitle().setTitle(roundBandwidth(bandwidth, counter));
                    }
                };
                if (Daten.debug) {
                    timer.schedule(timerTask, 0, 100);
                } else {
                    timer.schedule(timerTask, 0, 1000);
                }
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

    private String roundBandwidth(double bandw, long time) {

        if (bandw > 1000000) {
            return time / 60 + ":" + (time % 60 < 10 ? "0" + time % 60 : time % 60) + " Minuten / " + new DecimalFormat("####0.00").format(bandw / 1000000) + " MByte/s";
        } else if (bandw > 1000) {
            return time / 60 + ":" + (time % 60 < 10 ? "0" + time % 60 : time % 60) + " Minuten / " + Math.round(bandw / 1000) + " kByte/s";
        } else {
            return time / 60 + ":" + (time % 60 < 10 ? "0" + time % 60 : time % 60) + " Minuten / " + Math.round(bandw) + " Byte/s";
        }
    }
}
