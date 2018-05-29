package mediathek.javafx;

import javafx.application.Platform;
import javafx.beans.property.IntegerProperty;
import javafx.collections.ObservableList;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Separator;
import mSearch.Config;
import mediathek.config.Daten;
import org.controlsfx.control.StatusBar;

public class StatusBarController {
    /**
     * The new javafx based status bar
     */
    private StatusBar statusBar = new StatusBar();
    private Daten daten;
    private MemoryMonitor memoryMonitor;
    private FilmlistAgeLabel filmlistAgeLabel;
    private SelectedItemsLabel selectedItemsLabel;

    public StatusBarController(Daten daten, MemoryMonitor memoryMonitor, IntegerProperty selectedItemsProperty) {
        this.daten = daten;
        this.memoryMonitor = memoryMonitor;
        selectedItemsLabel = new SelectedItemsLabel(selectedItemsProperty);

        filmlistAgeLabel = new FilmlistAgeLabel(daten);
    }

    private Scene createStatusBarScene() {
        ObservableList<Node> leftItems = statusBar.getLeftItems();
        ObservableList<Node> rightItems = statusBar.getRightItems();

        if (Config.isDebuggingEnabled()) {
            leftItems.add(selectedItemsLabel);
            leftItems.add(new Separator());

            Button btnGc = new GarbageCollectionButton();
            leftItems.add(btnGc);

            MemoryMonitorButton memButton = new MemoryMonitorButton(memoryMonitor);
            leftItems.add(memButton);
        }
        //reset text
        statusBar.setText("");
        rightItems.add(filmlistAgeLabel);

        return new Scene(statusBar);
    }

    public void installStatusBar(JFXPanel jfxPanel) {
        Platform.runLater(() -> jfxPanel.setScene(createStatusBarScene()));
    }

    public StatusBar getStatusBar() {
        return statusBar;
    }
}
