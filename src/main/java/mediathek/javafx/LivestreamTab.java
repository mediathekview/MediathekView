package mediathek.javafx;

import javafx.application.Platform;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.control.ListView;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.text.Text;
import mSearch.daten.DatenFilm;
import mSearch.daten.ListeFilme;
import mediathek.config.Daten;
import mediathek.daten.LiveStreamItem;
import mediathek.gui.messages.FilmListReadStopEvent;
import net.engio.mbassy.listener.Handler;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import java.util.stream.Collectors;

public class LivestreamTab extends JFXPanel {
    private static final Logger logger = LogManager.getLogger(LivestreamTab.class);
    private ListView<LiveStreamItem> listView;
    private final Daten daten;
    private Text entriesLabel;
    private ObservableList<LiveStreamItem> liveStreamList;

    public LivestreamTab(ObservableList<LiveStreamItem> list, Daten daten) {
        liveStreamList = list;
        this.daten = daten;

        daten.getMessageBus().subscribe(this);

        setScene(createScene());
    }

    /**
     * Filter the livestreams out of the film list after ist has finished reading.
     */
    @Handler
    public void handleFilmListReadEvent(FilmListReadStopEvent msg) {
        Platform.runLater(() -> {
            liveStreamList.clear();
            getLiveStreamData();
        });
    }

    private void getLiveStreamData() {
        ListeFilme listeFilme = daten.getListeFilme();
        List<DatenFilm> livestreamFilmList = listeFilme.parallelStream()
                .filter(film -> film.getThema().equalsIgnoreCase(ListeFilme.THEMA_LIVE))
                .collect(Collectors.toList());

        for (DatenFilm film : livestreamFilmList) {
            try {
                LiveStreamItem item = new LiveStreamItem();
                item.setSender(film.getSender());
                item.setTitel(film.getTitle());
                item.setUrl(new URL(film.getUrl()));
                liveStreamList.add(item);
            } catch (MalformedURLException ex) {
                logger.error(ex);
            }
        }
        livestreamFilmList.clear();
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
