package mediathek.controller.history;

import mediathek.config.Daten;
import mediathek.gui.messages.history.DownloadHistoryChangedEvent;

public class SeenHistoryController extends MVUsedUrls<DownloadHistoryChangedEvent> {
    public SeenHistoryController() {
        super("history.txt", Daten.getSettingsDirectory_String(), DownloadHistoryChangedEvent.class);
    }
}
