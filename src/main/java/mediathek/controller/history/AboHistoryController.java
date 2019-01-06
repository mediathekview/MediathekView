package mediathek.controller.history;

import mediathek.config.Daten;
import mediathek.gui.messages.history.AboHistoryChangedEvent;

public class AboHistoryController extends MVUsedUrls<AboHistoryChangedEvent> {
    public AboHistoryController() {
        super("downloadAbos.txt", Daten.getSettingsDirectory_String(), AboHistoryChangedEvent.class);
    }
}
