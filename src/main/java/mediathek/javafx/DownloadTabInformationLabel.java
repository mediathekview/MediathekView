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
        // Text links: Zeilen Tabelle
        // nicht gestarted, laufen, fertig OK, fertig fehler
        final int[] starts = daten.getListeDownloads().getStarts();
        final int anz = daten.getListeDownloads().size();
        final int diff = anz - starts[0];

        boolean print = false;
        for (int ii = 1; ii < starts.length; ++ii) {
            if (starts[ii] > 0) {
                print = true;
                break;
            }
        }

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
        if (starts[1] == 1) {
            textLinks += "1 Abo, ";
        } else {
            textLinks += "" + starts[1] + " Abos, ";
        }
        if (starts[2] == 1) {
            textLinks += "1 Download";
        } else {
            textLinks += starts[2] + " Downloads";
        }

        textLinks +=" ";
        
        if (print) {
            if (starts[4] == 1) {
                textLinks += "1 läuft";
            } else {
                textLinks += starts[4] + " laufen";
            }

            if (starts[4] > 0) {
                textLinks += " (" + daten.getDownloadInfos().getBandwidthStr() + ')';
            }

            if (starts[3] == 1) {
                textLinks += ", 1 wartet";
            } else {
                textLinks += ", " + starts[3] + " warten";
            }
            if (starts[5] > 0) {
                if (starts[5] == 1) {
                    textLinks += ", 1 fertig";
                } else {
                    textLinks += ", " + starts[5] + " fertig";
                }
            }
            if (starts[6] > 0) {
                if (starts[6] == 1) {
                    textLinks += ", 1 fehlerhaft";
                } else {
                    textLinks += ", " + starts[6] + " fehlerhaft";
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
