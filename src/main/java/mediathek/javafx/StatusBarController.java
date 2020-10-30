package mediathek.javafx;

import javafx.application.Platform;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Region;
import mediathek.config.Config;
import mediathek.config.Daten;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.javafx.filmlist.FilmListAgeLabel;
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
    private final GarbageCollectionButton btnGc = new GarbageCollectionButton();
    private Pane progressPane;

    public StatusBarController(Daten daten) {
        filmListInfoPane = new FilmListInfoPane(daten);

        daten.getMessageBus().subscribe(this);

        createProgressPane();

        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                addProgressItems();

                if (Config.isDebugModeEnabled())
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
                if (Config.isDebugModeEnabled())
                    Platform.runLater(() -> statusBar.setText(""));
            }
        });
    }

    public FilmListAgeLabel getFilmlistAgeLabel() {
        return filmListInfoPane.getFilmListAgeLabel();
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
            } else {
                final double max = event.max;
                final double progress = event.progress;

                progressBar.setProgress(progress / max);
            }
            progressLabel.setText(event.text);
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

        if (Config.isDebugModeEnabled()) {
            leftItems.add(btnGc);
            leftItems.add(new VerticalSeparator());
        }
    }

    private void setupRightPane() {
        statusBar.getRightItems().add(filmListInfoPane);
    }

    public StatusBar createStatusBar() {
        //reset text
        statusBar.setText("");

        setupLeftPane();
        setupRightPane();

        return statusBar;
    }
}
