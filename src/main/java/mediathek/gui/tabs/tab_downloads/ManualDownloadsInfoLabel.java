package mediathek.gui.tabs.tab_downloads;

import mediathek.daten.DownloadStartInfo;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class ManualDownloadsInfoLabel extends JLabel {
    public ManualDownloadsInfoLabel(@NotNull DownloadStartInfoProperty startInfoProperty) {
        setToolTipText("Anzahl der manuellen Downloads in der Downloadliste");
        startInfoProperty.addStartInfoChangeListener(evt -> SwingUtilities.invokeLater(() -> process((DownloadStartInfo) evt.getNewValue())));
    }

    private void process(@NotNull DownloadStartInfo info) {
        String numDownloads = (info.num_downloads == 1) ? "1 Download" : info.num_downloads + " Downloads";
        setText(numDownloads);
    }
}
