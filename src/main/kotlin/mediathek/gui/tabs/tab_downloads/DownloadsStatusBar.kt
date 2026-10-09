package mediathek.gui.tabs.tab_downloads

import org.jdesktop.swingx.JXStatusBar

class DownloadsStatusBar(startInfoProperty: DownloadStartInfoProperty) : JXStatusBar() {
    init {
        add(TotalDownloadsLabel(startInfoProperty))
        add(AboLabel(startInfoProperty))
        add(ManualDownloadsInfoLabel(startInfoProperty))
        add(ActiveDownloadsInfoLabel(startInfoProperty))
        add(WaitingDownloadsInfoLabel(startInfoProperty))
        add(FinishedDownloadsInfoLabel(startInfoProperty))
        add(FailedDownloadsInfoLabel(startInfoProperty))
    }
}
