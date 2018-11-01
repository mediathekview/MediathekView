package mediathek.gui.history;

import mSearch.tool.Listener;
import mediathek.config.Daten;
import mediathek.controller.history.MVUsedUrl;
import mediathek.tool.TModel;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.util.List;

import static mediathek.controller.history.MVUsedUrl.TITLE_HEADER;

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
                if (jToggleButtonLaden.isSelected()) {
                    jTable1.setModel(createDataModel());
                    setsum();
                }
            }
        });
        jButtonLoeschen.addActionListener((ActionEvent e) -> {
            int ret = JOptionPane.showConfirmDialog(this, "Alle Einträge werden gelöscht.", "Löschen?", JOptionPane.YES_NO_OPTION);
            if (ret == JOptionPane.OK_OPTION) {
                workList.alleLoeschen();
            }
        });
        jToggleButtonLaden.addActionListener((ActionEvent e) -> {
            if (jToggleButtonLaden.isSelected()) {
                jButtonLoeschen.setEnabled(true);
                jTable1.setModel(createDataModel());
                setsum();
            } else {
                jButtonLoeschen.setEnabled(false);
                jTable1.setModel(new TModel(null, TITLE_HEADER));
                setsum();
            }
        });
    }
}
