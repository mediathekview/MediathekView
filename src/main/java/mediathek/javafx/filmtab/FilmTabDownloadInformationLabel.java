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

        textLinks = (anz == 1) ? "1 Download" : anz + " Downloads";

        if (info.hasValues()) {
            textLinks += ": ";

            textLinks += (info.running == 1) ? "1 läuft" : info.running + " laufen";

            if (info.running > 0)
                textLinks += " (" + daten.getDownloadInfos().getBandwidthStr() + ')';

            textLinks += (info.initialized == 1) ? ", 1 wartet" : ", " + info.initialized + " warten";

            if (info.finished > 0)
                textLinks += (info.finished == 1) ? ", 1 fertig" : ", " + info.finished + " fertig";

            if (info.error > 0)
                textLinks += (info.error == 1) ? ", 1 fehlerhaft" : ", " + info.error + " fehlerhaft";
        }

        return textLinks;
    }
}
