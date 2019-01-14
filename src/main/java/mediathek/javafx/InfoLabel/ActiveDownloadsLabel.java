package mediathek.javafx.InfoLabel;

import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;
import mediathek.config.Daten;
import mediathek.daten.DownloadStartInfo;

public class ActiveDownloadsLabel extends Label {
    public ActiveDownloadsLabel() {
        setTooltip(new Tooltip("Anzahl der aktiven Downloads"));
    }

    public void updateLabel(Daten daten, DownloadStartInfo info) {
        String numDownloads;

        if (info.hasValues()) {
            numDownloads = (info.running == 1) ? "1 läuft" : info.running + " laufen";

            if (info.running > 0) {
                numDownloads += " (" + daten.getDownloadInfos().getBandwidthStr() + ')';
            }
            setText(numDownloads);
        } else
            setText("");
    }
}
