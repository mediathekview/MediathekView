package mediathek.gui.history;

import mSearch.tool.Listener;
import mediathek.config.Daten;
import mediathek.controller.history.MVUsedUrl;

import java.util.List;

public final class DownloadHistoryPanel extends PanelErledigteUrls {
    public DownloadHistoryPanel(Daten d) {
        super(d);
        workList = daten.history;

        initHistory();
    }

    @Override
    protected List<MVUsedUrl> getExportableList() {
        return daten.history.getSortedList();
    }

    private void initHistory() {
        Listener.addListener(new Listener(Listener.EREIGNIS_LISTE_HISTORY_GEAENDERT, PanelErledigteUrls.class.getSimpleName()) {
            @Override
            public void ping() {
                if (jToggleButtonLaden.isSelected())
                    updateModelAndRecalculate(createDataModel());
            }
        });
    }
}
