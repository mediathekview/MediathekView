package mediathek.gui.dialogEinstellungen;

import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.gui.PanelVorlage;
import mediathek.gui.messages.ParallelDownloadNumberChangedEvent;
import mediathek.tool.ApplicationConfiguration;
import net.engio.mbassy.listener.Handler;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.*;

@SuppressWarnings("serial")
public class PanelDownload extends PanelVorlage {

    @Handler
    private void handleParallelDownloadNumberChange(ParallelDownloadNumberChangedEvent e) {
        SwingUtilities.invokeLater(() -> {
            final int maxNumDownloads = ApplicationConfiguration.getConfiguration().getInt(ApplicationConfiguration.DOWNLOAD_MAX_SIMULTANEOUS_NUM,1);
            jSpinnerAnzahlDownload.setValue(maxNumDownloads);
        });
    }

    public PanelDownload(Daten d, JFrame parent) {
        super(d, parent);
        initComponents();
        daten = d;

        daten.getMessageBus().subscribe(this);

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

            daten.getMessageBus().publishAsync(new ParallelDownloadNumberChangedEvent());
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jPanel2 = new JPanel();
        var jLabel3 = new JLabel();
        jSpinnerAnzahlDownload = new JSpinner();
        jCheckBoxBeep = new JCheckBox();
        jButtonBeep = new JButton();
        cbkDownloadError = new JCheckBox();

        //======== this ========

        //======== jPanel2 ========
        {
            jPanel2.setBorder(new TitledBorder("")); //NON-NLS

            //---- jLabel3 ----
            jLabel3.setText("gleichzeitige Downloads laden:"); //NON-NLS

            //---- jSpinnerAnzahlDownload ----
            jSpinnerAnzahlDownload.setModel(new SpinnerNumberModel(1, 1, 9, 1));

            //---- jCheckBoxBeep ----
            jCheckBoxBeep.setText("nach jedem Download einen \"Beep\" ausgeben"); //NON-NLS

            //---- jButtonBeep ----
            jButtonBeep.setText("Testen"); //NON-NLS

            //---- cbkDownloadError ----
            cbkDownloadError.setText("Bei Downloadfehler, Fehlermeldung anzeigen"); //NON-NLS

            GroupLayout jPanel2Layout = new GroupLayout(jPanel2);
            jPanel2.setLayout(jPanel2Layout);
            jPanel2Layout.setHorizontalGroup(
                jPanel2Layout.createParallelGroup()
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(jPanel2Layout.createParallelGroup()
                            .addComponent(cbkDownloadError)
                            .addGroup(jPanel2Layout.createSequentialGroup()
                                .addComponent(jCheckBoxBeep)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jButtonBeep))
                            .addGroup(jPanel2Layout.createSequentialGroup()
                                .addComponent(jLabel3)
                                .addGap(51, 51, 51)
                                .addComponent(jSpinnerAnzahlDownload, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)))
                        .addGap(0, 5, Short.MAX_VALUE))
            );
            jPanel2Layout.setVerticalGroup(
                jPanel2Layout.createParallelGroup()
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(cbkDownloadError)
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel2Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                            .addComponent(jCheckBoxBeep)
                            .addComponent(jButtonBeep))
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel2Layout.createParallelGroup(GroupLayout.Alignment.CENTER)
                            .addComponent(jLabel3)
                            .addComponent(jSpinnerAnzahlDownload, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                        .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
            );
        }

        GroupLayout layout = new GroupLayout(this);
        setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jPanel2, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jPanel2, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                    .addContainerGap(3, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JSpinner jSpinnerAnzahlDownload;
    private JCheckBox jCheckBoxBeep;
    private JButton jButtonBeep;
    private JCheckBox cbkDownloadError;
    // End of variables declaration//GEN-END:variables
}
