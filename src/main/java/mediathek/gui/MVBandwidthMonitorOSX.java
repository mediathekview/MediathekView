package mediathek.gui;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.IAxis;
import info.monitorenter.gui.chart.labelformatters.LabelFormatterAutoUnits;
import info.monitorenter.gui.chart.rangepolicies.RangePolicyForcedPoint;
import info.monitorenter.gui.chart.traces.Trace2DLtd;

import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.text.DecimalFormat;
import java.util.LinkedList;
import java.util.TimerTask;
import javax.swing.*;
import mSearch.tool.DbgMsg;
import mSearch.tool.Listener;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;

/**
 * This class will manage and display the download bandwidth chart display.
 */
public class MVBandwidthMonitorOSX implements IBandwidthMonitor {

    private double counter = 0; // double sonst "läuft" die Chart nicht
    private JDialog hudDialog = null;
    private final Trace2DLtd m_trace = new Trace2DLtd(300);
    private IAxis<?> x_achse = null;

    /**
     * Timer for collecting sample data.
     */
    private final java.util.Timer timer = new java.util.Timer(false);
    private TimerTask timerTask = null;

    private void createDialog(JFrame parent) {
        hudDialog = new JDialog(parent);
        hudDialog.setTitle("Bandbreite");
        hudDialog.setResizable(true);
        hudDialog.setType(Window.Type.UTILITY);
        hudDialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        hudDialog.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                beenden();
            }
        });

    }

    private void calculateHudPosition() {
        final GraphicsDevice gd = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice();
        final DisplayMode dm = gd.getDisplayMode();
        hudDialog.setLocation(dm.getWidth() - DEFAULT_WIDTH, 0);
    }

    private Chart2D createChart() {
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

        return chart;
    }

    public MVBandwidthMonitorOSX(JFrame parent) {
        createDialog(parent);

        hudDialog.setLayout(new BorderLayout(0, 0));
        hudDialog.getContentPane().add(createChart(),BorderLayout.CENTER);
        hudDialog.setSize(DEFAULT_WIDTH,DEFAULT_HEIGHT);

        Listener.addListener(new Listener(Listener.EREIGNIS_BANDWIDTH_MONITOR, MVBandwidthMonitorOSX.class.getSimpleName()) {
            @Override
            public void ping() {
                setVisibility();
            }
        });

        calculateHudPosition();

        setVisibility();
    }

    private static final int DEFAULT_WIDTH = 300;
    private static final int DEFAULT_HEIGHT = 150;

    private void beenden() {
        MVConfig.add(MVConfig.Configs.SYSTEM_BANDWIDTH_MONITOR_VISIBLE, Boolean.toString(false));
        Listener.notify(Listener.EREIGNIS_BANDWIDTH_MONITOR, MVBandwidthMonitorLWin.class.getSimpleName());
        setVisibility();
    }

    /**
     * Show/hide bandwidth display. Take also care about the used timer.
     */
    public void setVisibility() {
        final boolean isVis = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BANDWIDTH_MONITOR_VISIBLE));
        hudDialog.setVisible(isVis);
        try {
            if (isVis) {
                timerTask = new TimerTask() {
                    @Override
                    public void run() {
                        double bandwidth = 0.0; // bytes per second
                        //only count running/active downloads and calc accumulated progress..
                        LinkedList<DatenDownload> activeDownloadList = Daten.listeDownloads.getListOfStartsNotFinished(DatenDownload.QUELLE_ALLE);
                        for (DatenDownload download : activeDownloadList) {
                            if (download.start != null && download.start.status == Start.STATUS_RUN) {
                                bandwidth += download.start.bandbreite;
                            }
                        }

                        if (bandwidth < 0.0) {
                            bandwidth = 0.0;
                        }

                        counter++;

                        m_trace.addPoint(counter / 60, bandwidth); // minutes
                        x_achse.getAxisTitle().setTitle(roundBandwidth(bandwidth, (long) counter));
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
            hudDialog.dispose();
        }

    }

    private String roundBandwidth(double bandw, long time) {
        if (bandw > 1_000_000.0) {
            return time / 60 + ":" + (time % 60 < 10 ? "0" + time % 60 : time % 60) + " Minuten / " + new DecimalFormat("####0.00").format(bandw / 1_000_000.0) + " MByte/s";
        } else if (bandw > 1_000.0) {
            return time / 60 + ":" + (time % 60 < 10 ? "0" + time % 60 : time % 60) + " Minuten / " + Math.round(bandw / 1_000.0) + " kByte/s";
        } else {
            return time / 60 + ":" + (time % 60 < 10 ? "0" + time % 60 : time % 60) + " Minuten / " + Math.round(bandw) + " Byte/s";
        }
    }

    @Override
    public void writeConfig() {
        //do nothing for OSX
    }
}

