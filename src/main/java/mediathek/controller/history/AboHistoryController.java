package mediathek.controller.history;

import mediathek.gui.messages.history.AboHistoryChangedEvent;

public class AboHistoryController extends MVUsedUrls<AboHistoryChangedEvent> {
    public AboHistoryController() {
        super("downloadAbos.txt", AboHistoryChangedEvent.class);
    }
}
