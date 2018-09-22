package mediathek.gui.bandwidth;

import javafx.animation.Animation;
import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.chart.AreaChart;
import javafx.scene.chart.NumberAxis;
import javafx.scene.chart.XYChart;
import javafx.util.Duration;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.daten.ListeDownloads;
import mediathek.gui.messages.BandwidthMonitorStateChangedEvent;
import mediathek.tool.GuiFunktionen;
import net.engio.mbassy.listener.Handler;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * This class will manage and display the download bandwidth chart display.
 */
public class BandwidthMonitorController {

    private static final int DEFAULT_WIDTH = 300;
    private static final int DEFAULT_HEIGHT = 150;
    private static final int TIMELINE_SIZE = 60;
    private final XYChart.Series<Number, Number> series = new XYChart.Series<>();
    private final AtomicInteger time = new AtomicInteger();
    private JDialog hudDialog = null;
    private Timeline updateMemoryTimer;
    private final ListeDownloads listeDownloads;

    public BandwidthMonitorController(JFrame parent) {
        listeDownloads = Daten.getInstance().getListeDownloads();
        createDialog(parent);
        createFXPanel();

        if (!GuiFunktionen.setSize(MVConfig.Configs.SYSTEM_GROESSE_INFODIALOG, hudDialog, null)) {
            hudDialog.setSize(DEFAULT_WIDTH, DEFAULT_HEIGHT);
            calculateHudPosition();
        }

        createUpdateTimer();

        Daten.getInstance().getMessageBus().subscribe(this);

        setVisibility();
    }

    private void createFXPanel() {
        hudDialog.setLayout(new BorderLayout(0, 0));
        final JFXPanel fxPanel = new JFXPanel();
        hudDialog.getContentPane().add(fxPanel, BorderLayout.CENTER);
        Platform.runLater(() -> fxPanel.setScene(new Scene(createChart())));
    }

    public void close() {
        updateMemoryTimer.stop();
        hudDialog.dispose();
    }

    @Handler
    private void handleBandwidthMonitorStateChangedEvent(BandwidthMonitorStateChangedEvent e) {
        SwingUtilities.invokeLater(this::setVisibility);
    }

    private void createDialog(JFrame parent) {
        hudDialog = new JDialog(parent);
        hudDialog.setTitle("Bandbreite");
        hudDialog.setResizable(true);
        hudDialog.setType(Window.Type.UTILITY);
        hudDialog.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        hudDialog.addComponentListener(new ComponentAdapter() {
            @Override
            public void componentShown(ComponentEvent e) {
                Platform.runLater(() -> updateMemoryTimer.play());
            }

            @Override
            public void componentHidden(ComponentEvent e) {
                Platform.runLater(() -> updateMemoryTimer.stop());
                updateListeners();
            }
        });
    }

    private void calculateHudPosition() {
        final GraphicsDevice gd = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice();
        final DisplayMode dm = gd.getDisplayMode();
        hudDialog.setLocation(dm.getWidth() - DEFAULT_WIDTH, 0);
    }


    /**
     * remove too old data from data series.
     */
    private void cleanupDataSeries() {
        if (series.getData().size() > TIMELINE_SIZE) {
            series.getData().subList(0, series.getData().size() - TIMELINE_SIZE).clear();
        }
    }

    /**
     * Calculate to current bandwidth usage.
     *
     * @return Used bandwidth in Megabits per second.
     */
    private int calculateBandwidthUsage() {
        int bandwidth = 0; // bytes per second
        //only count running/active downloads and calc accumulated progress..
        var activeDownloadList = listeDownloads.getListOfStartsNotFinished(DatenDownload.QUELLE_ALLE);
        for (DatenDownload download : activeDownloadList) {
            if (download.start != null && download.start.status == Start.STATUS_RUN) {
                bandwidth += download.start.bandbreite;
            }
        }
        activeDownloadList.clear();

        //convert to MBits per second
        bandwidth = bandwidth * 8 / 1000 / 1000;

        return bandwidth;
    }

    private void createUpdateTimer() {
        updateMemoryTimer = new Timeline(new KeyFrame(Duration.seconds(1), event -> {
            series.getData().add(new XYChart.Data<>(time.incrementAndGet(), calculateBandwidthUsage()));
            cleanupDataSeries();
        }));
        updateMemoryTimer.setCycleCount(Animation.INDEFINITE);
    }

    private AreaChart createChart() {
        NumberAxis xAxis = new NumberAxis();
        xAxis.setForceZeroInRange(false);
        xAxis.setTickUnit(5d);
        xAxis.setTickLabelsVisible(false);

        NumberAxis yAxis = new NumberAxis();
        yAxis.setForceZeroInRange(false);
        yAxis.setLabel("MBit/s");

        AreaChart<Number, Number> areaChart = new AreaChart<>(xAxis, yAxis);
        areaChart.createSymbolsProperty().setValue(false);
        areaChart.setLegendVisible(false);
        areaChart.setAnimated(false);
        areaChart.getData().add(series);

        return areaChart;
    }

    private void updateListeners() {
        MVConfig.add(MVConfig.Configs.SYSTEM_BANDWIDTH_MONITOR_VISIBLE, Boolean.toString(false));
        Daten.getInstance().getMessageBus().publishAsync(new BandwidthMonitorStateChangedEvent());
    }

    /**
     * Show/hide bandwidth display. Take also care about the used timer.
     */
    public void setVisibility() {
        final boolean isVis = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BANDWIDTH_MONITOR_VISIBLE));
        hudDialog.setVisible(isVis);
    }

    public void writeConfig() {
        GuiFunktionen.getSize(MVConfig.Configs.SYSTEM_GROESSE_INFODIALOG, hudDialog);
    }
}

