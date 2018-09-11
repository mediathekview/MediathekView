package mediathek.javafx;

import javafx.application.Platform;
import javafx.collections.ObservableList;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Region;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.MediathekGui;
import mediathek.config.Config;
import mediathek.config.Daten;
import mediathek.javafx.filmlist.FilmListInfoPane;
import org.controlsfx.control.StatusBar;

public class StatusBarController {
    private final Label progressLabel = new Label("");
    private final ProgressBar progressBar = new ProgressBar();
    /**
     * The new javafx based status bar
     */
    private final StatusBar statusBar = new StatusBar();
    private final FilmListInfoPane filmListInfoPane;
    private final FilmListInformationLabel filmListInformationLabel;
    private final GarbageCollectionButton btnGc = new GarbageCollectionButton();
    private Pane progressPane;

    public StatusBarController(Daten daten) {
        filmListInfoPane = new FilmListInfoPane(daten);
        filmListInformationLabel = new FilmListInformationLabel(daten, MediathekGui.ui().tabPaneIndexProperty());

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

    public StatusBar getStatusBar() {
        return statusBar;
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
            //fix strange exception that duplicate was added...
            if (!rightItems.contains(progressPane))
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

        if (Config.isDebuggingEnabled()) {
            leftItems.add(btnGc);
            leftItems.add(new VerticalSeparator());
        }
        CenteredBorderPane pane = new CenteredBorderPane(filmListInformationLabel);
        leftItems.add(pane);
        leftItems.add(new VerticalSeparator());
    }

    private void setupRightPane() {
        statusBar.getRightItems().add(filmListInfoPane);
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
