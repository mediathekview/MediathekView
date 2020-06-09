package mediathek.gui.dialogEinstellungen;

import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.gui.messages.ParallelDownloadNumberChangedEvent;
import mediathek.tool.ApplicationConfiguration;
import net.engio.mbassy.listener.Handler;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.*;

@SuppressWarnings("serial")
public class PanelDownload extends JPanel {

    @Handler
    private void handleParallelDownloadNumberChange(ParallelDownloadNumberChangedEvent e) {
        SwingUtilities.invokeLater(() -> {
            final int maxNumDownloads = ApplicationConfiguration.getConfiguration().getInt(ApplicationConfiguration.DOWNLOAD_MAX_SIMULTANEOUS_NUM,1);
            jSpinnerAnzahlDownload.setValue(maxNumDownloads);
        });
    }

    public PanelDownload() {
        initComponents();

        Daten.getInstance().getMessageBus().subscribe(this);

        jSpinnerAnzahlDownload.setModel(new SpinnerNumberModel(1, 1, 9, 1));
        final int maxNumDownloads = ApplicationConfiguration.getConfiguration().getInt(ApplicationConfiguration.DOWNLOAD_MAX_SIMULTANEOUS_NUM,1);
        jSpinnerAnzahlDownload.setValue(maxNumDownloads);
        jSpinnerAnzahlDownload.addChangeListener(new BeobSpinnerDownload());

        cbkDownloadError.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DOWNLOAD_ERRORMSG)));
        cbkDownloadError.addActionListener(e -> MVConfig.add(MVConfig.Configs.SYSTEM_DOWNLOAD_ERRORMSG, Boolean.toString(cbkDownloadError.isSelected())));

        jCheckBoxBeep.setSelected(ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.DOWNLOAD_SOUND_BEEP,false));
        jCheckBoxBeep.addActionListener(l -> ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.DOWNLOAD_SOUND_BEEP,jCheckBoxBeep.isSelected()));

        jButtonBeep.addActionListener(ae -> Toolkit.getDefaultToolkit().beep());
    }

    private class BeobSpinnerDownload implements ChangeListener {

        @Override
        public void stateChanged(ChangeEvent arg0) {
            final int maxDownloads = ((Number)jSpinnerAnzahlDownload.getModel().getValue()).intValue();
            ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.DOWNLOAD_MAX_SIMULTANEOUS_NUM, maxDownloads);

            Daten.getInstance().getMessageBus().publishAsync(new ParallelDownloadNumberChangedEvent());
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jPanel2 = new JPanel();
        cbkDownloadError = new JCheckBox();
        var panel1 = new JPanel();
        jCheckBoxBeep = new JCheckBox();
        jButtonBeep = new JButton();
        var panel2 = new JPanel();
        var jLabel3 = new JLabel();
        jSpinnerAnzahlDownload = new JSpinner();

        //======== this ========
        setLayout(new BorderLayout());

        //======== jPanel2 ========
        {
            jPanel2.setLayout(new MigLayout(
                new LC().insets("5").hideMode(3).gridGap("0", "5"), //NON-NLS
                // columns
                new AC()
                    .grow().align("left"), //NON-NLS
                // rows
                new AC()
                    .fill().gap()
                    .fill().gap()
                    .fill()));

            //---- cbkDownloadError ----
            cbkDownloadError.setText("Bei Downloadfehler eine Fehlermeldung anzeigen"); //NON-NLS
            jPanel2.add(cbkDownloadError, new CC().cell(0, 0));

            //======== panel1 ========
            {
                panel1.setLayout(new MigLayout(
                    new LC().insets("0").hideMode(3).gridGap("15", "5"), //NON-NLS
                    // columns
                    new AC()
                        .fill().gap()
                        .fill(),
                    // rows
                    new AC()
                        .fill()));

                //---- jCheckBoxBeep ----
                jCheckBoxBeep.setText("Nach jedem Download einen \"Beep\" ausgeben"); //NON-NLS
                panel1.add(jCheckBoxBeep, new CC().cell(0, 0));

                //---- jButtonBeep ----
                jButtonBeep.setText("Testen"); //NON-NLS
                panel1.add(jButtonBeep, new CC().cell(1, 0));
            }
            jPanel2.add(panel1, new CC().cell(0, 1));

            //======== panel2 ========
            {
                panel2.setLayout(new MigLayout(
                    new LC().insets("0").hideMode(3).gridGap("15", "5"), //NON-NLS
                    // columns
                    new AC()
                        .fill().gap()
                        .fill(),
                    // rows
                    new AC()
                        .fill()));

                //---- jLabel3 ----
                jLabel3.setText("Anzahl gleichzeitiger Downloads:"); //NON-NLS
                panel2.add(jLabel3, new CC().cell(0, 0));

                //---- jSpinnerAnzahlDownload ----
                jSpinnerAnzahlDownload.setModel(new SpinnerNumberModel(1, 1, 9, 1));
                panel2.add(jSpinnerAnzahlDownload, new CC().cell(1, 0));
            }
            jPanel2.add(panel2, new CC().cell(0, 2));
        }
        add(jPanel2, BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JCheckBox cbkDownloadError;
    private JCheckBox jCheckBoxBeep;
    private JButton jButtonBeep;
    private JSpinner jSpinnerAnzahlDownload;
    // End of variables declaration//GEN-END:variables
}
