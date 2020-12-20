package mediathek.gui.history;

import mediathek.gui.messages.history.AboHistoryChangedEvent;
import net.engio.mbassy.listener.Handler;

import javax.swing.*;

public final class AboHistoryPanel extends PanelErledigteUrls {

    public AboHistoryPanel() {
        super();
        workList = daten.getAboHistoryController();

        daten.getMessageBus().subscribe(this);
    }

    @Handler
    private void handleAboHistoryChangeEvent(AboHistoryChangedEvent e) {
        SwingUtilities.invokeLater(this::changeListHandler);
    }
}
