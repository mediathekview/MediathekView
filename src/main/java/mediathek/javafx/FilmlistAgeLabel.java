package mediathek.javafx;

import javafx.application.Platform;
import mSearch.daten.ListeFilme;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.config.Daten;
import mediathek.gui.messages.TimerEvent;
import mediathek.javafx.tool.ComputedLabel;
import net.engio.mbassy.listener.Handler;

import javax.swing.*;

/**
 * Computed label which will display the creation date and age of the current film list.
 */
public class FilmlistAgeLabel extends ComputedLabel {
    private final Daten daten;

    public FilmlistAgeLabel(Daten daten) {
        super();
        this.daten = daten;
        daten.getMessageBus().subscribe(this);

        SwingUtilities.invokeLater(() -> daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                daten.getMessageBus().unsubscribe(this);
                Platform.runLater(() -> setVisible(false));
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                daten.getMessageBus().subscribe(this);
                Platform.runLater(() -> setVisible(true));
            }
        }));
    }

    private void computeAge() {
        final ListeFilme listeFilme = daten.getListeFilme();

        String strText = "Filmliste erstellt: ";
        strText += listeFilme.genDate();
        strText += " Uhr  ";

        final int sekunden = listeFilme.getAge();

        if (sekunden != 0) {
            strText += "||  Alter: ";
            final int minuten = sekunden / 60;
            String strSekunde = String.valueOf(sekunden % 60);
            String strMinute = String.valueOf(minuten % 60);
            String strStunde = String.valueOf(minuten / 60);
            if (strSekunde.length() < 2) {
                strSekunde = '0' + strSekunde;
            }
            if (strMinute.length() < 2) {
                strMinute = '0' + strMinute;
            }
            if (strStunde.length() < 2) {
                strStunde = '0' + strStunde;
            }
            strText += strStunde + ':' + strMinute + ':' + strSekunde + ' ';
        }

        setComputedText(strText);
    }

    @Handler
    public void handleTimerEvent(TimerEvent e) {
        Platform.runLater(this::computeAge);
    }
}
