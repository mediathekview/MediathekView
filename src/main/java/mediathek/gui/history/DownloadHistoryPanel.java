package mediathek.gui.history;

import mediathek.config.Daten;
import mediathek.controller.history.MVUsedUrl;
import mediathek.gui.messages.history.DownloadHistoryChangedEvent;
import net.engio.mbassy.listener.Handler;

import javax.swing.*;
import java.util.List;

public final class DownloadHistoryPanel extends PanelErledigteUrls {
    public DownloadHistoryPanel(Daten d) {
        super(d);
        workList = daten.getSeenHistoryList();

        d.getMessageBus().subscribe(this);
    }

    @Handler
    private void handleChangeEvent(DownloadHistoryChangedEvent e) {
        SwingUtilities.invokeLater(this::changeListHandler);
    }

    @Override
    protected List<MVUsedUrl> getExportableList() {
        return daten.getSeenHistoryList().getSortedList();
    }
}
