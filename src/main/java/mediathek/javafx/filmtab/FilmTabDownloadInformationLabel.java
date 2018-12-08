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
        final var info = listeDownloads.getStarts();
        final int anz = listeDownloads.size();

        boolean print = false;
        if (info.hasValues())
            print = true;

        if (anz == 1) {
            textLinks = "1 Download";
        } else {
            textLinks = anz + " Downloads";
        }

        if (print) {
            textLinks += ": ";

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
        return textLinks;
    }
}
