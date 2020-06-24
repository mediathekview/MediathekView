package mediathek.gui.dialogEinstellungen;

import mediathek.config.MVConfig;
import mediathek.tool.ApplicationConfiguration;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import java.awt.*;

@SuppressWarnings("serial")
public class PanelDownload extends JPanel {

    public PanelDownload() {
        initComponents();

        cbkDownloadError.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DOWNLOAD_ERRORMSG)));
        cbkDownloadError.addActionListener(e -> MVConfig.add(MVConfig.Configs.SYSTEM_DOWNLOAD_ERRORMSG, Boolean.toString(cbkDownloadError.isSelected())));

        jCheckBoxBeep.setSelected(ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.DOWNLOAD_SOUND_BEEP,false));
        jCheckBoxBeep.addActionListener(l -> ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.DOWNLOAD_SOUND_BEEP,jCheckBoxBeep.isSelected()));

        jButtonBeep.addActionListener(ae -> Toolkit.getDefaultToolkit().beep());
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jPanel2 = new JPanel();
        cbkDownloadError = new JCheckBox();
        jCheckBoxBeep = new JCheckBox();
        jButtonBeep = new JButton();

        //======== this ========
        setLayout(new BorderLayout());

        //======== jPanel2 ========
        {
            jPanel2.setLayout(new MigLayout(
                new LC().insets("5").hideMode(3).gridGap("5", "5"), //NON-NLS
                // columns
                new AC()
                    .align("left").gap() //NON-NLS
                    .fill(),
                // rows
                new AC()
                    .fill().gap()
                    .fill()));

            //---- cbkDownloadError ----
            cbkDownloadError.setText("Bei Downloadfehler eine Fehlermeldung anzeigen"); //NON-NLS
            jPanel2.add(cbkDownloadError, new CC().cell(0, 0));

            //---- jCheckBoxBeep ----
            jCheckBoxBeep.setText("Nach jedem Download einen \"Beep\" ausgeben"); //NON-NLS
            jPanel2.add(jCheckBoxBeep, new CC().cell(0, 1));

            //---- jButtonBeep ----
            jButtonBeep.setText("Test"); //NON-NLS
            jPanel2.add(jButtonBeep, new CC().cell(1, 1));
        }
        add(jPanel2, BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JCheckBox cbkDownloadError;
    private JCheckBox jCheckBoxBeep;
    private JButton jButtonBeep;
    // End of variables declaration//GEN-END:variables
}
