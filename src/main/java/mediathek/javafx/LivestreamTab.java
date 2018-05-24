package mediathek.javafx;

import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.control.ListView;
import javafx.scene.layout.VBox;

public class LivestreamTab extends JFXPanel {
    public LivestreamTab() {
        setScene(createScene());
    }

    private Scene createScene() {
        VBox root = new VBox();
        root.setFillWidth(true);

        ListView<String> listView = new ListView<>();
        listView.getItems().addAll("Hello", "World", "Humpdi", "Dumpdi", "Media", "thekView", "bla");
        root.getChildren().addAll(listView);

        listView.requestFocus();
        return new Scene(root);
    }
}
