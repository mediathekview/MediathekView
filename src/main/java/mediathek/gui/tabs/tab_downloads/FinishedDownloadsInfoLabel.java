package mediathek.gui.tabs.tab_downloads;

import mediathek.daten.DownloadStartInfo;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class FinishedDownloadsInfoLabel extends JLabel {
    public FinishedDownloadsInfoLabel(@NotNull DownloadStartInfoProperty startInfoProperty) {
        setToolTipText("Anzahl der abgeschlossenen Downloads");
        startInfoProperty.addStartInfoChangeListener(evt -> SwingUtilities.invokeLater(() -> process((DownloadStartInfo) evt.getNewValue())));
    }

    private void process(@NotNull DownloadStartInfo info) {
        if (info.hasValues()) {
            String fin = (info.finished == 1) ? "1 fertig" : info.finished + " fertig";
            setText(fin);
        } else
            setText("0 fertig");
    }
}
