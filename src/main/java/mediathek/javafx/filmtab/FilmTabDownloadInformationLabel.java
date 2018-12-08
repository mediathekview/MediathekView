package mediathek.javafx.filmtab;

import javafx.scene.control.Label;
import mediathek.config.Daten;

public class FilmTabDownloadInformationLabel extends Label {
    private final Daten daten;

    public FilmTabDownloadInformationLabel(Daten daten) {
        super();
        this.daten = daten;
    }

    public void setInfoFilme() {
        setText(getInfoTextDownloads());
    }

    private String getInfoTextDownloads() {
        String textLinks;
        final var listeDownloads = daten.getListeDownloads();
        final int[] starts = listeDownloads.getStarts();
        final int anz = listeDownloads.size();

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
        return textLinks;
    }
}
