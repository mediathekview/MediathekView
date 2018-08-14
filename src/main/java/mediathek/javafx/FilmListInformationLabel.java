package mediathek.javafx;

import javafx.application.Platform;
import javafx.beans.property.ObjectProperty;
import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.daten.DatenAbo;
import mediathek.daten.DatenDownload;
import mediathek.gui.messages.TimerEvent;
import mediathek.gui.messages.UpdateStatusBarLeftDisplayEvent;
import mediathek.javafx.tool.ComputedLabel;
import net.engio.mbassy.listener.Handler;

public class FilmListInformationLabel extends ComputedLabel {
    private static final String TRENNER = "  ||  ";
    private final Daten daten;
    private final ObjectProperty<MediathekGui.TabPaneIndex> tabPaneIndexProperty;

    public FilmListInformationLabel(Daten daten, ObjectProperty<MediathekGui.TabPaneIndex> tabPaneIndexProperty) {
        super();
        this.tabPaneIndexProperty = tabPaneIndexProperty;
        this.daten = daten;

        daten.getMessageBus().subscribe(this);
    }

    private void setInfoDefault() {
        String textLinks;
        int gesamt = daten.getListeFilme().size();

        // Anzahl der Filme
        if (gesamt == 1) {
            textLinks = "1 Film";
        } else {
            textLinks = gesamt + " Filme";
        }
        setComputedText(textLinks);
    }

    @Handler
    private void handleLeftDisplayUpdate(UpdateStatusBarLeftDisplayEvent e) {
        Platform.runLater(this::setTextForLeftDisplay);
    }

    private void setTextForLeftDisplay() {
        switch (tabPaneIndexProperty.getValue()) {
            case FILME:
                setInfoFilme();
                break;

            case DOWNLOAD:
                String textLinks = getInfoTextDownloads(true);
                setComputedText(textLinks);
                break;

            case ABO:
                setInfoAbo();
                break;

            default:
                setInfoDefault();
                break;
        }
    }

    private void setInfoAbo() {
        String textLinks;
        int ein = 0;
        int aus = 0;
        int gesamt = daten.getListeAbo().size();
        for (DatenAbo abo : daten.getListeAbo()) {
            if (abo.aboIstEingeschaltet()) {
                ++ein;
            } else {
                ++aus;
            }
        }
        if (gesamt == 1) {
            textLinks = "1 Abo";
        } else {
            textLinks = gesamt + " Abos";
        }
        textLinks += TRENNER + ein + " eingeschaltet, " + aus + " ausgeschaltet";

        setComputedText(textLinks);
    }

    private void setInfoFilme() {
        String textLinks;
        final int gesamt = daten.getListeFilme().size();
        final int anzListe = Daten.guiFilme.getTableRowCount();
        final int runs = daten.getListeDownloadsButton().getListOfStartsNotFinished(DatenDownload.QUELLE_BUTTON).size();

        // Anzahl der Filme
        if (gesamt == anzListe) {
            if (anzListe == 1) {
                textLinks = "1 Film";
            } else {
                textLinks = anzListe + " Filme";
            }
        } else {
            if (anzListe == 1) {
                textLinks = "1 Film";
            } else {
                textLinks = anzListe + " Filme";
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
        textLinks += TRENNER;
        textLinks += getInfoTextDownloads(false);

        setComputedText(textLinks);
    }

    private String getInfoTextDownloads(boolean download) {
        String textLinks;
        // Text links: Zeilen Tabelle
        // nicht gestarted, laufen, fertig OK, fertig fehler
        int[] starts = daten.getDownloadInfos().downloadStarts;
        int anz = daten.getListeDownloads().size();
        int diff = anz - starts[0];

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
        if (download) {
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
            textLinks += TRENNER;
        } else if (print) {
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
    public void handleTimerEvent(TimerEvent e) {
        Platform.runLater(this::setTextForLeftDisplay);
    }
}
