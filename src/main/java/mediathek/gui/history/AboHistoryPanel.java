package mediathek.gui.history;

import mediathek.config.Daten;
import mediathek.controller.history.MVUsedUrl;
import mediathek.gui.messages.history.AboHistoryChangedEvent;
import net.engio.mbassy.listener.Handler;

import javax.swing.*;
import java.util.List;

public final class AboHistoryPanel extends PanelErledigteUrls {

    public AboHistoryPanel(Daten d) {
        super(d);
        workList = daten.getAboHistoryController();

        d.getMessageBus().subscribe(this);
    }

    @Handler
    private void handleAboHistoryChangeEvent(AboHistoryChangedEvent e) {
        SwingUtilities.invokeLater(this::changeListHandler);
    }

    @Override
    protected List<MVUsedUrl> getExportableList() {
        return daten.getAboHistoryController().getSortedList();
    }
}
