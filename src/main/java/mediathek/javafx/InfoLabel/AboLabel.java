package mediathek.javafx.InfoLabel;

import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;
import mediathek.daten.DownloadStartInfo;

public class AboLabel extends Label {
    public AboLabel() {
        setTooltip(new Tooltip("Anzahl der Abos in der Downloadliste"));
    }

    public void updateLabel(DownloadStartInfo info) {
        String abo = (info.num_abos == 1) ? "1 Abo" : info.num_abos + " Abos";
        setText(abo);
    }
}
