package mediathek.javafx;

import javafx.application.Platform;
import javafx.beans.property.IntegerProperty;
import javafx.collections.ObservableList;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.layout.BorderPane;
import mSearch.Config;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.config.Daten;
import org.controlsfx.control.StatusBar;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.GlyphFont;
import org.controlsfx.glyphfont.GlyphFontRegistry;

import javax.swing.*;

public class StatusBarController {
    private Label debugLabel = new Label("");
    private ProgressBar progressBar = new ProgressBar();
    private Button btnFilmListStop;
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

        GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");
        btnFilmListStop = new Button("", fontAwesome.create(FontAwesome.Glyph.STOP));
        btnFilmListStop.setOnAction(e -> SwingUtilities.invokeLater(() -> daten.getFilmeLaden().setStop(true)));

        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                addProgressItems();

                Platform.runLater(() -> statusBar.setText(event.senderUrl));
            }

            @Override
            public void progress(ListenerFilmeLadenEvent event) {
                Platform.runLater(() -> debugLabel.setText(event.senderUrl));

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

    private void updateProgressBar(ListenerFilmeLadenEvent event) {
        Platform.runLater(() -> {
            if (!progressBar.isVisible()) {
                progressBar.setVisible(true);
                btnFilmListStop.setVisible(true);
            }

            if (event.max == 0 || event.progress == event.max) {
                progressBar.setProgress(-1d);
                debugLabel.setText(event.text);
            } else {
                final double max = (double) event.max;
                final double progress = (double) event.progress;

                progressBar.setProgress(progress / max);
                debugLabel.setText(event.text);
            }
        });
    }

    private void addProgressItems() {
        Platform.runLater(() -> {
            ObservableList<Node> rightItems = statusBar.getRightItems();
            //CenteredBorderPane pane = new CenteredBorderPane(debugLabel);
            rightItems.add(debugLabel);

            //pane = new CenteredBorderPane(progressBar);
            rightItems.add(progressBar);

            //pane = new CenteredBorderPane(btnFilmListStop);
            rightItems.add(btnFilmListStop);
        });
    }

    private void removeProgressItems() {
        Platform.runLater(() -> {
            ObservableList<Node> rightItems = statusBar.getRightItems();
            rightItems.remove(debugLabel);
            rightItems.remove(progressBar);
            rightItems.remove(btnFilmListStop);
        });
    }

    private void setupLeftPane() {
        ObservableList<Node> leftItems = statusBar.getLeftItems();
        CenteredBorderPane pane = new CenteredBorderPane(selectedItemsLabel);
        leftItems.add(pane);
        leftItems.add(new VerticalSeparator());

        if (Config.isDebuggingEnabled()) {
            leftItems.add(btnGc);
            leftItems.add(memButton);
            leftItems.add(new VerticalSeparator());
        }
        pane = new CenteredBorderPane(filmListInformationLabel);
        leftItems.add(pane);
        leftItems.add(new VerticalSeparator());
    }

    private void setupRightPane() {
        ObservableList<Node> rightItems = statusBar.getRightItems();

        /*CenteredBorderPane pane = new CenteredBorderPane(debugLabel);
        rightItems.add(pane);
        progressBar.setProgress(-1d);
        pane = new CenteredBorderPane(progressBar);
        rightItems.add(pane);
        rightItems.add(btnFilmListStop);*/
        rightItems.add(new VerticalSeparator());
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

    /**
     * A BorderPane subclass which automatically centers nodes
     */
    class CenteredBorderPane extends BorderPane {
        public CenteredBorderPane(Node node) {
            super();
            setCenter(node);
            setCenterShape(true);
        }
    }
}
