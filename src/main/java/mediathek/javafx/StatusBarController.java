package mediathek.javafx;

import javafx.application.Platform;
import javafx.beans.property.IntegerProperty;
import javafx.collections.ObservableList;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.ProgressIndicator;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Region;
import mSearch.Config;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.config.Daten;
import mediathek.gui.messages.FilmListWriteStartEvent;
import mediathek.gui.messages.FilmListWriteStopEvent;
import net.engio.mbassy.listener.Handler;
import org.controlsfx.control.StatusBar;

public class StatusBarController {
    private final Label progressLabel = new Label("");
    private final ProgressBar progressBar = new ProgressBar();
    /**
     * The new javafx based status bar
     */
    private final StatusBar statusBar = new StatusBar();
    private final FilmlistAgeLabel filmlistAgeLabel;
    private final FilmListInformationLabel filmListInformationLabel;
    private final SelectedItemsLabel selectedItemsLabel;
    private final GarbageCollectionButton btnGc = new GarbageCollectionButton();
    private Pane progressPane;
    private MemoryMonitor memoryMonitor;
    private final MemoryMonitorButton memButton = new MemoryMonitorButton(memoryMonitor);
    private Pane filmListWriterProgressPane = null;

    public StatusBarController(Daten daten, MemoryMonitor memoryMonitor, IntegerProperty selectedItemsProperty) {
        this.memoryMonitor = memoryMonitor;

        selectedItemsLabel = new SelectedItemsLabel(selectedItemsProperty);
        filmlistAgeLabel = new FilmlistAgeLabel(daten);
        filmListInformationLabel = new FilmListInformationLabel(daten, daten.getMediathekGui().tabPaneIndexProperty());

        daten.getMessageBus().subscribe(this);

        createProgressPane();

        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                addProgressItems();

                if (Config.isDebuggingEnabled())
                    Platform.runLater(() -> statusBar.setText(event.senderUrl));
            }

            @Override
            public void progress(ListenerFilmeLadenEvent event) {
                updateProgressBar(event);
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                Platform.runLater(() -> progressBar.setProgress(0d));
                removeProgressItems();
                if (Config.isDebuggingEnabled())
                    Platform.runLater(() -> statusBar.setText(""));
            }
        });
    }

    private Pane createFilmListWriterProgress() {
        filmListWriterProgressPane = new HBox();
        filmListWriterProgressPane.setMinWidth(Region.USE_PREF_SIZE);
        filmListWriterProgressPane.getChildren().addAll(
                new CenteredBorderPane(new ProgressIndicator()),
                new CenteredBorderPane(new Label("Schreibe Filmliste...")),
                new VerticalSeparator());

        return filmListWriterProgressPane;
    }

    @Handler
    private void handleFilmListWriterStartEvent(FilmListWriteStartEvent e) {
        Platform.runLater(() -> {
            if (filmListWriterProgressPane == null)
                filmListWriterProgressPane = createFilmListWriterProgress();

            statusBar.getRightItems().add(filmListWriterProgressPane);
        });
    }

    @Handler
    private void handleFilmListWriterStopEvent(FilmListWriteStopEvent e) {
        Platform.runLater(() -> {
            statusBar.getRightItems().remove(filmListWriterProgressPane);
            filmListWriterProgressPane = null;
        });
    }

    private void createProgressPane() {
        HBox hb = new HBox();
        hb.setSpacing(5d);
        hb.setMinWidth(Region.USE_PREF_SIZE);
        hb.getChildren().addAll(new VerticalSeparator(),
                new CenteredBorderPane(progressLabel),
                new CenteredBorderPane(progressBar)
        );

        progressPane = hb;
    }

    private void updateProgressBar(ListenerFilmeLadenEvent event) {
        Platform.runLater(() -> {
            if (!progressBar.isVisible())
                progressBar.setVisible(true);

            if (event.max == 0 || event.progress == event.max) {
                progressBar.setProgress(-1d);
                progressLabel.setText(event.text);
            } else {
                final double max = (double) event.max;
                final double progress = (double) event.progress;

                progressBar.setProgress(progress / max);
                progressLabel.setText(event.text);
            }
        });
    }

    private void addProgressItems() {
        Platform.runLater(() -> {
            ObservableList<Node> rightItems = statusBar.getRightItems();
            rightItems.add(progressPane);

        });
    }

    private void removeProgressItems() {
        Platform.runLater(() -> {
            ObservableList<Node> rightItems = statusBar.getRightItems();
            rightItems.remove(progressPane);
        });
    }

    private void setupLeftPane() {
        ObservableList<Node> leftItems = statusBar.getLeftItems();
        leftItems.add(new CenteredBorderPane(selectedItemsLabel));
        leftItems.add(new VerticalSeparator());

        if (Config.isDebuggingEnabled()) {
            leftItems.add(btnGc);
            leftItems.add(memButton);
            leftItems.add(new VerticalSeparator());
        }
        CenteredBorderPane pane = new CenteredBorderPane(filmListInformationLabel);
        leftItems.add(pane);
        leftItems.add(new VerticalSeparator());
    }

    private void setupRightPane() {
        ObservableList<Node> rightItems = statusBar.getRightItems();
        CenteredBorderPane pane = new CenteredBorderPane(filmlistAgeLabel);
        rightItems.add(pane);
    }

    private Scene createStatusBarScene() {
        //reset text
        statusBar.setText("");

        setupLeftPane();
        setupRightPane();

        return new Scene(statusBar);
    }

    public void installStatusBar(JFXPanel jfxPanel) {
        Platform.runLater(() -> jfxPanel.setScene(createStatusBarScene()));
    }
}
