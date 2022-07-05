package mediathek.mainwindow;

import mediathek.config.Daten;
import mediathek.gui.messages.DownloadInfoUpdateAvailableEvent;
import mediathek.tool.MessageBus;
import net.engio.mbassy.listener.Handler;

import javax.swing.*;

public class DownloadInformationLabel extends JLabel {
    public DownloadInformationLabel() {
        MessageBus.getMessageBus().subscribe(this);
    }

    @Handler
    private void handleDownloadInfoUpdate(DownloadInfoUpdateAvailableEvent e) {
        SwingUtilities.invokeLater(this::setInfoFilme);
    }

    private void setInfoFilme() {
        setText(getInfoTextDownloads());
    }

    private String getInfoTextDownloads() {
        String textLinks;
        final var daten = Daten.getInstance();
        final var listeDownloads = daten.getListeDownloads();
        final var info = listeDownloads.getStarts();

        textLinks = (info.total_num_download_list_entries == 1) ?
                "1 Download" : info.total_num_download_list_entries + " Downloads";

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
