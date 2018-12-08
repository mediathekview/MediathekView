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

        boolean print = false;
        if (info.hasValues())
            print = true;

        if (anz == 1) {
            textLinks = "1 Download";
        } else {
            textLinks = anz + " Downloads";
        }

        if (diff == 1) {
            textLinks += " (1 zurückgestellt)";
        } else if (diff > 1) {
            textLinks += " (" + diff + " zurückgestellt)";
        }
        textLinks += TRENNER;
        if (info.num_abos == 1) {
            textLinks += "1 Abo, ";
        } else {
            textLinks += "" + info.num_abos + " Abos, ";
        }
        if (info.num_downloads == 1) {
            textLinks += "1 Download";
        } else {
            textLinks += info.num_downloads + " Downloads";
        }

        textLinks +=" ";
        
        if (print) {
            if (info.running == 1) {
                textLinks += "1 läuft";
            } else {
                textLinks += info.running + " laufen";
            }

            if (info.running > 0) {
                textLinks += " (" + daten.getDownloadInfos().getBandwidthStr() + ')';
            }

            if (info.initialized == 1) {
                textLinks += ", 1 wartet";
            } else {
                textLinks += ", " + info.initialized + " warten";
            }
            if (info.finished > 0) {
                if (info.finished == 1) {
                    textLinks += ", 1 fertig";
                } else {
                    textLinks += ", " + info.finished + " fertig";
                }
            }
            if (info.error > 0) {
                if (info.error == 1) {
                    textLinks += ", 1 fehlerhaft";
                } else {
                    textLinks += ", " + info.error + " fehlerhaft";
                }
            }
        }

        setText(textLinks);
    }

    @Handler
    private void handleTimerEvent(TimerEvent e) {
        Platform.runLater(this::getInfoTextDownloads);
    }
}
