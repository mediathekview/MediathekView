package mediathek.javafx;

import javafx.application.Platform;
import javafx.scene.control.Label;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.config.Daten;
import mediathek.gui.messages.TimerEvent;
import net.engio.mbassy.listener.Handler;

import javax.swing.*;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Computed label which will display the creation date and age of the current film list.
 */
public class FilmlistAgeLabel extends Label {
    private AtomicBoolean timerStopped = new AtomicBoolean(false);
    private Daten daten;

    public FilmlistAgeLabel(Daten daten) {
        super();
        this.daten = daten;
        daten.getMessageBus().subscribe(this);

        SwingUtilities.invokeLater(() -> {
            daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
                @Override
                public void start(ListenerFilmeLadenEvent event) {
                    timerStopped.set(true);
                    daten.getMessageBus().unsubscribe(this);
                }

                @Override
                public void progress(ListenerFilmeLadenEvent event) {
                }

                @Override
                public void fertig(ListenerFilmeLadenEvent event) {
                    timerStopped.set(false);
                    daten.getMessageBus().subscribe(this);
                }
            });
        });
    }

    private void computeAge() {
        String strText = "Filmliste erstellt: ";
        strText += daten.getListeFilme().genDate();
        strText += " Uhr  ";

        final int sekunden = daten.getListeFilme().getAge();

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
        Platform.runLater(() -> {
            if (!timerStopped.get()) {
                if (!isVisible())
                    setVisible(true);
                computeAge();
            } else
                setVisible(false);
        });
    }

    private double width = 0d;

    /**
     * This sets the text of the label and adjusts the width of the label to prevent "jumping" text.
     *
     * @param text
     */
    public void setComputedText(String text) {
        setText(text);
        double curWidth = getWidth();
        if (curWidth >= width) {
            width = curWidth;
            setMinWidth(width);
        }

    }
}
