package mediathek.gui.bandwidth;

import eu.hansolo.tilesfx.Tile;
import eu.hansolo.tilesfx.TileBuilder;
import javafx.animation.Animation;
import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.paint.Color;
import javafx.scene.paint.Stop;
import javafx.util.Duration;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.daten.ListeDownloads;
import mediathek.gui.messages.BandwidthMonitorStateChangedEvent;
import mediathek.javafx.tool.JavaFxUtils;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.GuiFunktionen;
import net.engio.mbassy.listener.Handler;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.time.ZoneId;
import java.time.ZonedDateTime;

/**
 * This class will manage and display the download bandwidth chart display.
 */
public class BandwidthMonitorController {

    private static final int DEFAULT_WIDTH = 300;
    private static final int DEFAULT_HEIGHT = 150;
    private final ListeDownloads listeDownloads;
    private JDialog hudDialog = null;
    private Timeline updateMemoryTimer = null;
    private Tile bandwidthTile;
    private JFXPanel fxPanel;

    public BandwidthMonitorController(JFrame parent) {
        listeDownloads = Daten.getInstance().getListeDownloads();
        createDialog(parent);
        JavaFxUtils.invokeInFxThreadAndWait(() -> {
            fxPanel.setScene(new Scene(createTile()));
            createUpdateTimer();
        });

        if (!GuiFunktionen.setSize(MVConfig.Configs.SYSTEM_GROESSE_INFODIALOG, hudDialog, null)) {
            hudDialog.setSize(DEFAULT_WIDTH, DEFAULT_HEIGHT);
            calculateHudPosition();
        }

        Daten.getInstance().getMessageBus().subscribe(this);

        setVisibility();
    }

    public void close() {
        JavaFxUtils.invokeInFxThreadAndWait(() -> {
            if (updateMemoryTimer != null)
                updateMemoryTimer.stop();
        });
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
        hudDialog.setLayout(new BorderLayout(0, 0));
        fxPanel = new JFXPanel();
        hudDialog.getContentPane().add(fxPanel, BorderLayout.CENTER);
    }

    private void calculateHudPosition() {
        final GraphicsDevice gd = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice();
        final DisplayMode dm = gd.getDisplayMode();
        hudDialog.setLocation(dm.getWidth() - DEFAULT_WIDTH, 0);
    }

    /**
     * Calculate to current bandwidth usage.
     *
     * @return Used bandwidth in Megabits per second.
     */
    private double calculateBandwidthUsage() {
        double bandwidth = 0d; // bytes per second
        //only count running/active downloads and calc accumulated progress..
        var activeDownloadList = listeDownloads.getListOfStartsNotFinished(DatenDownload.QUELLE_ALLE);
        for (DatenDownload download : activeDownloadList) {
            if (download.start != null && download.start.status == Start.STATUS_RUN) {
                bandwidth += download.start.bandbreite;
            }
        }
        activeDownloadList.clear();

        //convert to MBits per second
        bandwidth = bandwidth * 8d / 1000d / 1000d;
        if (bandwidth < 0d)
            bandwidth = 0d;

        return bandwidth;
    }

    private void createUpdateTimer() {
        updateMemoryTimer = new Timeline(new KeyFrame(Duration.seconds(1), event -> bandwidthTile.setValue(calculateBandwidthUsage())));
        updateMemoryTimer.setCycleCount(Animation.INDEFINITE);
    }

    private Tile createTile() {
        bandwidthTile = TileBuilder.create()
                .skinType(Tile.SkinType.SPARK_LINE)
                .prefSize(400, 400)
                .unit("MBit/s")
                .minValue(0)
                .maxValue(2 * 1024)
                .decimals(0)
                .tickLabelDecimals(0)
                .time(ZonedDateTime.now(ZoneId.of("Europe/Berlin")))
                .gradientStops(new Stop(0, Color.web("#1CAF4D")),
                        new Stop(0.0075, Color.web("#1CAF4D")),
                        new Stop(0.00751, Color.web("#91CA40")),
                        new Stop(0.01166, Color.web("#91CA40")),
                        new Stop(0.01167, Color.web("#F8C610")),
                        new Stop(0.01666, Color.web("#F8C610")),
                        new Stop(0.01667, Color.web("#F29222")),
                        new Stop(0.025, Color.web("#F29222")),
                        new Stop(0.02501, Color.web("#EC1D24")),
                        new Stop(1.0, Color.web("#EC1D24")))
                .strokeWithGradient(true)
                .averagingPeriod(96)
                .averageVisible(true)
                .averagingEnabled(true)
                .smoothing(true)
                .build();

        bandwidthTile.setValue(0d);

        return bandwidthTile;
    }

    private void updateListeners() {
        ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.APPLICATION_UI_BANDWIDTH_MONITOR_VISIBLE,false);
        Daten.getInstance().getMessageBus().publishAsync(new BandwidthMonitorStateChangedEvent());
    }

    /**
     * Show/hide bandwidth display. Take also care about the used timer.
     */
    public void setVisibility() {
        final var vis = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_UI_BANDWIDTH_MONITOR_VISIBLE,false);
        hudDialog.setVisible(vis);
    }

    public void writeConfig() {
        GuiFunktionen.getSize(MVConfig.Configs.SYSTEM_GROESSE_INFODIALOG, hudDialog);
    }
}

