package mediathek.javafx.filmlist;

import javafx.application.Platform;
import javafx.scene.layout.HBox;
import mediathek.config.Daten;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.javafx.CenteredBorderPane;
import mediathek.javafx.VerticalSeparator;

import javax.swing.*;

/**
 * Pane which will display common information about the current filmlist.
 * Also handles updating the subcomponents based on a TimerEvent.
 */
public class FilmListInfoPane extends HBox {
    private final FilmListCreationDateLabel filmListCreationDateLabel;

    public FilmListInfoPane(Daten daten) {
        super();
        setSpacing(4d);

        SwingUtilities.invokeLater(() -> daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                daten.getMessageBus().unsubscribe(this);
                Platform.runLater(() -> setVisible(false));
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                daten.getMessageBus().subscribe(this);
                Platform.runLater(() -> {
                    filmListCreationDateLabel.computeCreationDate();
                    setVisible(true);
                });
            }
        }));

        filmListCreationDateLabel = new FilmListCreationDateLabel(daten);
        FilmListAgeLabel filmListAgeLabel = new FilmListAgeLabel(daten);
        getChildren().addAll(new CenteredBorderPane(filmListCreationDateLabel),
                new VerticalSeparator(),
                new CenteredBorderPane(filmListAgeLabel));
    }
}
