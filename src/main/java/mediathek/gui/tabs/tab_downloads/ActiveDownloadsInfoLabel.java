package mediathek.gui.tabs.tab_downloads;

import mediathek.daten.DownloadStartInfo;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class ActiveDownloadsInfoLabel extends JLabel {
    public ActiveDownloadsInfoLabel(@NotNull DownloadStartInfoProperty startInfoProperty) {
        setToolTipText("Anzahl der aktiven Downloads");
        startInfoProperty.addStartInfoChangeListener(evt -> SwingUtilities.invokeLater(() -> process((DownloadStartInfo) evt.getNewValue())));
    }

    private void process(@NotNull DownloadStartInfo info) {
        if (info.hasValues()) {
            var numDownloads = (info.running == 1) ? "1 läuft" : info.running + " laufen";
            setText(numDownloads);
        } else
            setText("0 laufen");
    }
}
