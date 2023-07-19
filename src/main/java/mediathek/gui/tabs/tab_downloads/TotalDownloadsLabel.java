package mediathek.gui.tabs.tab_downloads;

import mediathek.daten.DownloadStartInfo;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class TotalDownloadsLabel extends JLabel {
    public TotalDownloadsLabel(@NotNull DownloadStartInfoProperty startInfoProperty) {
        setToolTipText("Gesamtzahl aller Downloads");
        startInfoProperty.addStartInfoChangeListener(evt -> SwingUtilities.invokeLater(() -> process((DownloadStartInfo) evt.getNewValue())));
    }

    private void process(@NotNull DownloadStartInfo info) {
        final int anz = info.total_num_download_list_entries;
        final int diff = anz - info.total_starts;
        String download = "Gesamtdownloads: " + anz;
        if (diff >= 1) {
            download += " (" + diff + " zurückgestellt)";
        }
        setText(download);
    }

}
