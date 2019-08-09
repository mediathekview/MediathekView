package mediathek.javafx.InfoLabel;

import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;
import mediathek.daten.DownloadStartInfo;

public class WaitingLabel extends Label {
    public WaitingLabel() {
        setTooltip(new Tooltip("Anzahl der wartenden Downloads"));
    }

    public void updateLabel(DownloadStartInfo info) {
        if (info.hasValues()) {
            String waiting = (info.initialized == 1) ? "1 wartet" : info.initialized + " warten";
            setText(waiting);
        } else
            setText("");
    }
}
