package mediathek;

import com.explodingpixels.macwidgets.HudWindow;
import com.jidesoft.utils.SystemInfo;
import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
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
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.CategoryAxis;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.block.BlockBorder;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.category.BarRenderer;
import org.jfree.chart.renderer.category.LineAndShapeRenderer;
import org.jfree.chart.renderer.xy.XYDotRenderer;
import org.jfree.chart.renderer.xy.XYSplineRenderer;
import org.jfree.chart.title.TextTitle;
import org.jfree.data.category.CategoryDataset;
import org.jfree.data.category.DefaultCategoryDataset;
import org.jfree.data.xy.DefaultXYDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

/**
 * This class will manage and display the download bandwidth chart display.
 */
class MVBandwidthMonitor {

    private long counter = 0;
    private HudWindow hudWindow = null;
    private JCheckBoxMenuItem menuItem = null;
    XYSeriesCollection dataset = new XYSeriesCollection();
    XYSeries series1 = new XYSeries("Punkte1");
    XYPlot plot = null;
    final int MAXDATE = 300;

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
        }

        XYDotRenderer dot = new XYDotRenderer();
        dot.setDotHeight(1);
        dot.setDotWidth(1);
        XYSplineRenderer splineRenderer = new XYSplineRenderer();
        splineRenderer.setBaseShapesVisible(false);

        NumberAxis xax = new NumberAxis("x");
        NumberAxis yax = new NumberAxis("y");
        createDataset();
        plot = new XYPlot(dataset, xax, yax, splineRenderer);
        JFreeChart chart = new JFreeChart(plot);
        ChartPanel chartPanel = new ChartPanel(chart);

        ValueAxis domainAxis = plot.getDomainAxis();
        domainAxis.setAutoRange(true);
        domainAxis.setFixedAutoRange(MAXDATE);

        ValueAxis rangeAxis = plot.getRangeAxis();
        rangeAxis.setAutoRange(true);

        JPanel panel = new JPanel();
        panel.setOpaque(true);
        panel.setLayout(new BorderLayout(0, 0));
        panel.add(chartPanel, BorderLayout.CENTER);
        hudWindow.setContentPane(panel);

        final Dimension dim = hudDialog.getSize();
        dim.height = 150;
        dim.width = 300;
        hudDialog.setSize(dim);
    }

    private void createDataset() {
        for (int i = 0; i < MAXDATE; ++i) {
            series1.add(i, 0);
        }
        dataset.addSeries(series1);
    }

    private void addRowDataset(long value, long time) {
        // create the dataset...
        series1.remove(0);
        series1.add(time, value);

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
                        long bandwidth = 0;
                        //only count running/active downloads and calc accumulated progress..
                        LinkedList<DatenDownload> activeDownloadList = Daten.listeDownloads.getListOfStartsNotFinished(Start.QUELLE_ALLE);
                        for (DatenDownload download : activeDownloadList) {
                            if (download.start != null && download.start.status == Start.STATUS_RUN) {
                                bandwidth += download.start.bandbreite;
                            }
                        }
                        activeDownloadList.clear();

                        if (bandwidth < 0) {
                            bandwidth = 0;
                        }

//                        if (bandwidth > 0.0) {
//                            bandwidth /= 1024.0; // convert to KByte
//                        }
                        counter++;
                        if (counter > 24 * 60 * 60) {
                            counter = 0;
                        }
                        //addRowDataset(bandwidth, counter);
                        addRowDataset(Daten.guiDebug.getJSpinner().getValue(), counter);
                        //m_trace.addPoint(counter / 60, roundBandwidth(bandwidth)); // minutes
                        //m_trace.addPoint(counter / 60, roundBandwidth(Daten.guiDebug.getJSpinner().getValue() + counter)); // minutes
                        //m_trace.addPoint(counter / 60, 199521); // minutes
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
