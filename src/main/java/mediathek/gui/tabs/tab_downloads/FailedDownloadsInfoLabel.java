package mediathek.gui.tabs.tab_downloads;

import mediathek.daten.DownloadStartInfo;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class FailedDownloadsInfoLabel extends JLabel {
    public FailedDownloadsInfoLabel(@NotNull DownloadStartInfoProperty startInfoProperty) {
        setToolTipText("Anzahl der fehlerhaften Downloads");
        startInfoProperty.addStartInfoChangeListener(evt -> SwingUtilities.invokeLater(() -> process((DownloadStartInfo) evt.getNewValue())));
    }

    private void process(@NotNull DownloadStartInfo info) {
        if (info.hasValues()) {
            setText(info.error + " fehlerhaft");
        } else
            setText("0 fehlerhaft");
    }
}
