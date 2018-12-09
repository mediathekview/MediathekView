package mediathek.javafx;

import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.scene.control.Label;
import mediathek.config.Daten;
import mediathek.gui.messages.TimerEvent;
import mediathek.gui.messages.UpdateStatusBarLeftDisplayEvent;
import net.engio.mbassy.listener.Handler;

public class DownloadTabInformationLabel extends Label {
    private static final String TRENNER = "  ||  ";
    private final Daten daten;

    public DownloadTabInformationLabel(Daten daten) {
        super();
        this.daten = daten;

        if (isVisible())
            daten.getMessageBus().subscribe(this);

        visibleProperty().addListener(new ChangeListener<>() {
            @Override
            public void changed(ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) {
                if (newValue) {
                    daten.getMessageBus().subscribe(this);
                } else {
                    daten.getMessageBus().unsubscribe(this);
                }
            }
        });
    }

    @Handler
    private void handleLeftDisplayUpdate(UpdateStatusBarLeftDisplayEvent e) {
        Platform.runLater(this::getInfoTextDownloads);
    }

    private void getInfoTextDownloads() {
        String textLinks;
        final var info = daten.getListeDownloads().getStarts();
        final int anz = daten.getListeDownloads().size();
        final int diff = anz - info.total_starts;

        textLinks = (anz == 1) ? "1 Download" : anz + " Downloads";
        if (diff == 1)
            textLinks += " (1 zurückgestellt)";
        else if (diff > 1) {
            textLinks += " (" + diff + " zurückgestellt)";
        }
        textLinks += TRENNER;
        textLinks += (info.num_abos == 1) ? "1 Abo, " : info.num_abos + " Abos, ";
        textLinks += (info.num_downloads == 1) ? "1 Download" : info.num_downloads + " Downloads";

        textLinks +=" ";
        
        if (info.hasValues()) {
            textLinks += (info.running == 1) ? ", 1 läuft" : ", " + info.running + " laufen";

            if (info.running > 0) {
                textLinks += " (" + daten.getDownloadInfos().getBandwidthStr() + ')';
            }

            textLinks += (info.initialized == 1) ? ", 1 wartet" : ", " + info.initialized + " warten";

            if (info.finished > 0)
                textLinks += (info.finished == 1) ? ", 1 fertig" : ", " + info.finished + " fertig";

            if (info.error > 0)
                textLinks += (info.error == 1) ? ", 1 fehlerhaft" : ", " + info.error + " fehlerhaft";
        }

        setText(textLinks);
    }

    @Handler
    private void handleTimerEvent(TimerEvent e) {
        Platform.runLater(this::getInfoTextDownloads);
    }
}
