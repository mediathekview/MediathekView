package mediathek.gui.tabs.tab_downloads;

import mediathek.daten.DownloadStartInfo;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class AboLabel extends JLabel {
    public AboLabel(@NotNull DownloadStartInfoProperty startInfoProperty) {
        setToolTipText("Anzahl der Abos in der Liste");
        startInfoProperty.addStartInfoChangeListener(evt -> SwingUtilities.invokeLater(() -> process((DownloadStartInfo) evt.getNewValue())));

    }
    private void process(@NotNull DownloadStartInfo info) {
        String abo = (info.num_abos == 1) ? "1 Abo" : info.num_abos + " Abos";
        setText(abo);
    }
}
