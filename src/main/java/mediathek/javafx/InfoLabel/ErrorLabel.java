package mediathek.javafx.InfoLabel;

import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;
import mediathek.daten.DownloadStartInfo;

public class ErrorLabel extends Label {
    public ErrorLabel() {
        setTooltip(new Tooltip("Anzahl der fehlerhaften Downloads"));
    }

    public void updateLabel(DownloadStartInfo info) {
        if (info.hasValues()) {
            String textLinks = "";

            if (info.error > 0)
                textLinks += info.error + " fehlerhaft";
            setText(textLinks);
        }
    }
}
