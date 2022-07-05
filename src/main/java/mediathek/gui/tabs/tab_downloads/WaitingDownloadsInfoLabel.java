package mediathek.gui.tabs.tab_downloads;

import mediathek.daten.DownloadStartInfo;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class WaitingDownloadsInfoLabel extends JLabel {
    public WaitingDownloadsInfoLabel(@NotNull DownloadStartInfoProperty startInfoProperty) {
        setToolTipText("Anzahl der wartenden Downloads");
        startInfoProperty.addStartInfoChangeListener(evt -> SwingUtilities.invokeLater(() -> process((DownloadStartInfo) evt.getNewValue())));
    }

    private void process(@NotNull DownloadStartInfo info) {
        if (info.hasValues()) {
            String waiting = (info.initialized == 1) ? "1 wartet" : info.initialized + " warten";
            setText(waiting);
        } else
            setText("");
    }
}
