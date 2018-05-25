package mediathek.javafx;

import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.control.ListView;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.text.Text;
import mediathek.daten.LiveStreamItem;

public class LivestreamTab extends JFXPanel {
    //private static final Logger logger = LogManager.getLogger(LivestreamTab.class);
    private ListView<LiveStreamItem> listView;
    private Text entriesLabel;
    private ObservableList<LiveStreamItem> liveStreamList;

    public LivestreamTab(ObservableList<LiveStreamItem> list) {
        liveStreamList = list;

        setScene(createScene());
    }

    private Scene createScene() {
        listView = new ListView<>(liveStreamList);
        BorderPane pane = new BorderPane();
        pane.setCenter(listView);

        HBox hb = new HBox();
        hb.setSpacing(5d);

        entriesLabel = new Text();
        entriesLabel.setText(Integer.toString(liveStreamList.size()) + " livestreams");

        listView.getItems().addListener((ListChangeListener<LiveStreamItem>) c -> entriesLabel.setText(Integer.toString(c.getList().size()) + " Livestreams")
        );

        hb.getChildren().addAll(entriesLabel);

        pane.setBottom(hb);

        listView.requestFocus();
        return new Scene(pane);
    }
}
