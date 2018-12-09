package mediathek.javafx.InfoLabel;

import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;
import mediathek.daten.DownloadStartInfo;

public class FinishedLabel extends Label {
    public FinishedLabel() {
        setTooltip(new Tooltip("Anzahl der abgeschlossenen Downloads"));
    }

    public void updateLabel(DownloadStartInfo info) {
        if (info.hasValues()) {
            String fin = (info.finished == 1) ? "1 fertig" : info.finished + " fertig";
            setText(fin);
        } else
            setText("");
    }
}
