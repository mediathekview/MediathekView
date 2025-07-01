package mediathek.gui.dialogEinstellungen;

import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.tool.ApplicationConfiguration;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;
import java.awt.*;

public class PanelDownload extends JPanel {

    public PanelDownload() {
        initComponents();

        cbkDownloadError.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DOWNLOAD_ERRORMSG)));
        cbkDownloadError.addActionListener(_ -> MVConfig.add(MVConfig.Configs.SYSTEM_DOWNLOAD_ERRORMSG, Boolean.toString(cbkDownloadError.isSelected())));

        var config = ApplicationConfiguration.getConfiguration();
        jCheckBoxBeep.setSelected(config.getBoolean(ApplicationConfiguration.DOWNLOAD_SOUND_BEEP,false));
        jCheckBoxBeep.addActionListener(_ -> config.setProperty(ApplicationConfiguration.DOWNLOAD_SOUND_BEEP,jCheckBoxBeep.isSelected()));

        cbFetchMissingFileSize.setSelected(config.getBoolean(ApplicationConfiguration.DOWNLOAD_FETCH_FILE_SIZE, true));
        cbFetchMissingFileSize.addActionListener(_ -> config.setProperty(ApplicationConfiguration.DOWNLOAD_FETCH_FILE_SIZE, cbFetchMissingFileSize.isSelected()));

        jButtonBeep.addActionListener(_ -> Toolkit.getDefaultToolkit().beep());

        var countdown = ApplicationConfiguration.getConfiguration().getInt(ApplicationConfiguration.DOWNLOAD_CONTINUATION_TIME, Konstanten.DOWNLOAD_CONTINUATION_DEFAULT_TIME);
        if (countdown < 1 || countdown > Konstanten.DOWNLOAD_CONTINUATION_DEFAULT_TIME) {
            countdown = Konstanten.DOWNLOAD_CONTINUATION_DEFAULT_TIME;
        }
        spDefaultDownloadContinuation.setValue(countdown);
        spDefaultDownloadContinuation.addChangeListener(_ -> {
            int val = (int)spDefaultDownloadContinuation.getValue();
            config.setProperty(ApplicationConfiguration.DOWNLOAD_CONTINUATION_TIME, val);
        });

        cbStartDownloadsAutomatically.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN)));
        cbStartDownloadsAutomatically.addActionListener(_ -> MVConfig.add(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN, String.valueOf(cbStartDownloadsAutomatically.isSelected())));

        installRefreshListener();
    }

    private void installRefreshListener() {
        addAncestorListener(new AncestorListener() {
            @Override
            public void ancestorAdded(AncestorEvent event) {
                if (PanelDownload.this.isShowing()) {
                    cbStartDownloadsAutomatically.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN)));
                }
            }

            @Override
            public void ancestorRemoved(AncestorEvent event) {}

            @Override
            public void ancestorMoved(AncestorEvent event) {}
        });
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jPanel2 = new JPanel();
        cbkDownloadError = new JCheckBox();
        jCheckBoxBeep = new JCheckBox();
        jButtonBeep = new JButton();
        cbFetchMissingFileSize = new JCheckBox();
        cbStartDownloadsAutomatically = new JCheckBox();
        var panel1 = new JPanel();
        var label1 = new JLabel();
        spDefaultDownloadContinuation = new JSpinner();

        //======== this ========
        setLayout(new BorderLayout());

        //======== jPanel2 ========
        {
            jPanel2.setLayout(new MigLayout(
                new LC().insets("5").hideMode(3).gridGap("5", "5"),
                // columns
                new AC()
                    .align("left").gap()
                    .fill(),
                // rows
                new AC()
                    .fill().gap()
                    .fill().gap()
                    .gap()
                    .gap()
                    ));

            //---- cbkDownloadError ----
            cbkDownloadError.setText("Bei Downloadfehler eine Fehlermeldung anzeigen");
            jPanel2.add(cbkDownloadError, new CC().cell(0, 0));

            //---- jCheckBoxBeep ----
            jCheckBoxBeep.setText("Nach jedem Download einen \"Beep\" ausgeben");
            jPanel2.add(jCheckBoxBeep, new CC().cell(0, 1));

            //---- jButtonBeep ----
            jButtonBeep.setText("Test");
            jPanel2.add(jButtonBeep, new CC().cell(1, 1));

            //---- cbFetchMissingFileSize ----
            cbFetchMissingFileSize.setText("Fehlende Filmgr\u00f6\u00dfe nachladen");
            jPanel2.add(cbFetchMissingFileSize, new CC().cell(0, 2));

            //---- cbStartDownloadsAutomatically ----
            cbStartDownloadsAutomatically.setText("Downloads automatisch starten");
            cbStartDownloadsAutomatically.setToolTipText("Wie sollen hinzugef\u00fcgte (Mehrfach-)Downloads behandelt werden?");
            jPanel2.add(cbStartDownloadsAutomatically, new CC().cell(0, 3));

            //======== panel1 ========
            {
                panel1.setLayout(new FlowLayout(FlowLayout.LEFT));

                //---- label1 ----
                label1.setText("Standard-Wert f\u00fcr automatische Weiterf\u00fchrung:");
                panel1.add(label1);

                //---- spDefaultDownloadContinuation ----
                spDefaultDownloadContinuation.setModel(new SpinnerNumberModel(1, 1, 60, 1));
                panel1.add(spDefaultDownloadContinuation);
            }
            jPanel2.add(panel1, new CC().cell(0, 4, 2, 1));
        }
        add(jPanel2, BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JCheckBox cbkDownloadError;
    private JCheckBox jCheckBoxBeep;
    private JButton jButtonBeep;
    private JCheckBox cbFetchMissingFileSize;
    private JCheckBox cbStartDownloadsAutomatically;
    private JSpinner spDefaultDownloadContinuation;
    // End of variables declaration//GEN-END:variables
}
