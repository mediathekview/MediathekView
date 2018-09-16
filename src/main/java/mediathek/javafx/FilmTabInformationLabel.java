package mediathek.javafx;

import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.scene.control.Label;
import mediathek.config.Daten;
import mediathek.daten.DatenDownload;
import mediathek.gui.GuiFilme;
import mediathek.gui.messages.TimerEvent;
import mediathek.gui.messages.UpdateStatusBarLeftDisplayEvent;
import net.engio.mbassy.listener.Handler;

public class FilmTabInformationLabel extends Label {
    private static final String TRENNER = "  ||  ";
    private final Daten daten;
    private final GuiFilme tabFilme;

    public FilmTabInformationLabel(Daten daten, GuiFilme tabFilme) {
        super();
        this.daten = daten;
        this.tabFilme = tabFilme;

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
        Platform.runLater(this::setInfoFilme);
    }

    private void setInfoFilme() {
        String textLinks;
        final int gesamt = daten.getListeFilme().size();
        final int rowCount = tabFilme.getTableRowCount();
        final int runs = daten.getListeDownloadsButton().getListOfStartsNotFinished(DatenDownload.QUELLE_BUTTON).size();

        // Anzahl der Filme
        if (gesamt == rowCount) {
            if (rowCount == 1) {
                textLinks = "1 Film";
            } else {
                textLinks = rowCount + " Filme";
            }
        } else {
            if (rowCount == 1) {
                textLinks = "1 Film";
            } else {
                textLinks = rowCount + " Filme";
            }
            textLinks += " (Insgesamt: " + gesamt + " )";
        }
        // laufende Programme
        if (runs == 1) {
            textLinks += TRENNER;
            textLinks += (runs + " laufender Film");
        } else if (runs > 1) {
            textLinks += TRENNER;
            textLinks += (runs + " laufende Filme");
        }
        // auch die Downloads anzeigen
        //FIXME does not work reliably
        textLinks += TRENNER;
        textLinks += getInfoTextDownloads();

        setText(textLinks);
    }

    private String getInfoTextDownloads() {
        String textLinks;
        // Text links: Zeilen Tabelle
        // nicht gestarted, laufen, fertig OK, fertig fehler
        int[] starts = daten.getDownloadInfos().downloadStarts;
        int anz = daten.getListeDownloads().size();

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

        if (print) {
            textLinks += ": ";
        }

        if (print) {
            if (starts[4] == 1) {
                textLinks += "1 läuft";
            } else {
                textLinks += starts[4] + " laufen";
            }

            if (starts[4] > 0) {
                textLinks += " (" + daten.getDownloadInfos().bandwidthStr + ')';
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
        return textLinks;
    }

    @Handler
    private void handleTimerEvent(TimerEvent e) {
        Platform.runLater(this::setInfoFilme);
    }
}
