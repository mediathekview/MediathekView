package mediathek.javafx;

import javafx.application.Platform;
import javafx.beans.property.IntegerProperty;
import javafx.collections.ObservableList;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import mSearch.Config;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.config.Daten;
import org.controlsfx.control.StatusBar;

public class StatusBarController {
    private Label progressLabel = new Label("");
    private ProgressBar progressBar = new ProgressBar();
    private Pane progressPane;
    /**
     * The new javafx based status bar
     */
    private StatusBar statusBar = new StatusBar();
    private MemoryMonitor memoryMonitor;
    private FilmlistAgeLabel filmlistAgeLabel;
    private FilmListInformationLabel filmListInformationLabel;
    private SelectedItemsLabel selectedItemsLabel;
    private GarbageCollectionButton btnGc = new GarbageCollectionButton();
    private MemoryMonitorButton memButton = new MemoryMonitorButton(memoryMonitor);

    public StatusBarController(Daten daten, MemoryMonitor memoryMonitor, IntegerProperty selectedItemsProperty) {
        this.memoryMonitor = memoryMonitor;

        selectedItemsLabel = new SelectedItemsLabel(selectedItemsProperty);
        filmlistAgeLabel = new FilmlistAgeLabel(daten);
        filmListInformationLabel = new FilmListInformationLabel(daten, daten.getMediathekGui().tabPaneIndexProperty());

        createProgressPane();

        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                addProgressItems();

                Platform.runLater(() -> statusBar.setText(event.senderUrl));
            }

            @Override
            public void progress(ListenerFilmeLadenEvent event) {
                Platform.runLater(() -> progressLabel.setText(event.senderUrl));

                updateProgressBar(event);
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                Platform.runLater(() -> progressBar.setProgress(0d));
                removeProgressItems();
                Platform.runLater(() -> statusBar.setText(""));
            }
        });
    }

    private void createProgressPane() {
        HBox hb = new HBox();
        hb.setSpacing(5d);
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
