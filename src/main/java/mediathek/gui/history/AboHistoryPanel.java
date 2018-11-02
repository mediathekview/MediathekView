package mediathek.gui.history;

import mSearch.tool.Listener;
import mediathek.config.Daten;
import mediathek.controller.history.MVUsedUrl;

import java.util.List;

public final class AboHistoryPanel extends PanelErledigteUrls {
    public AboHistoryPanel(Daten d) {
        super(d);
        workList = daten.erledigteAbos;

        initAbo();
    }

    @Override
    protected List<MVUsedUrl> getExportableList() {
        return daten.erledigteAbos.getSortedList();
    }

    private void initAbo() {
        Listener.addListener(new Listener(Listener.EREIGNIS_LISTE_ERLEDIGTE_ABOS_GEAENDERT, PanelErledigteUrls.class.getSimpleName()) {
            @Override
            public void ping() {
                if (jToggleButtonLaden.isSelected())
                    updateModelAndRecalculate(createDataModel());
            }
        });
    }
}
