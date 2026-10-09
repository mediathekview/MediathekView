package mediathek.gui.dialogEinstellungen;

import mediathek.config.Konstanten;
import mediathek.config.application.ApplicationConfiguration;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import java.awt.*;

public class PanelDownload extends JPanel {

    public PanelDownload() {
        initComponents();

        var applicationConfiguration = ApplicationConfiguration.getInstance();
        cbkDownloadError.setSelected(applicationConfiguration.getShowDownloadErrorMessage());
        cbkDownloadError.addActionListener(_ -> applicationConfiguration.setShowDownloadErrorMessage(cbkDownloadError.isSelected()));

        jCheckBoxBeep.setSelected(applicationConfiguration.getPlaySoundAfterDownload());
        jCheckBoxBeep.addActionListener(_ -> applicationConfiguration.setPlaySoundAfterDownload(jCheckBoxBeep.isSelected()));

        cbFetchMissingFileSize.setSelected(applicationConfiguration.getFetchMissingDownloadFileSize());
        cbFetchMissingFileSize.addActionListener(_ -> applicationConfiguration.setFetchMissingDownloadFileSize(cbFetchMissingFileSize.isSelected()));

        jButtonBeep.addActionListener(_ -> Toolkit.getDefaultToolkit().beep());

        var countdown = applicationConfiguration.getDownloadContinuationTime();
        if (countdown < 1 || countdown > Konstanten.DOWNLOAD_CONTINUATION_DEFAULT_TIME) {
            countdown = Konstanten.DOWNLOAD_CONTINUATION_DEFAULT_TIME;
        }
        spDefaultDownloadContinuation.setValue(countdown);
        spDefaultDownloadContinuation.addChangeListener(_ -> {
            int val = (int)spDefaultDownloadContinuation.getValue();
            applicationConfiguration.setDownloadContinuationTime(val);
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
            jPanel2.add(panel1, new CC().cell(0, 3, 2, 1));
        }
        add(jPanel2, BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JCheckBox cbkDownloadError;
    private JCheckBox jCheckBoxBeep;
    private JButton jButtonBeep;
    private JCheckBox cbFetchMissingFileSize;
    private JSpinner spDefaultDownloadContinuation;
    // End of variables declaration//GEN-END:variables
}
