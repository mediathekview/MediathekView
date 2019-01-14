package mediathek.javafx.InfoLabel;

import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;
import mediathek.daten.DownloadStartInfo;

public class NumDownloadsLabel extends Label {
    public NumDownloadsLabel() {
        setTooltip(new Tooltip("Anzahl der manuellen Downloads in der Downloadliste"));
    }

    public void updateLabel(DownloadStartInfo info) {
        String numDownloads = (info.num_downloads == 1) ? "1 Download" : info.num_downloads + " Downloads";
        setText(numDownloads);
    }
}
