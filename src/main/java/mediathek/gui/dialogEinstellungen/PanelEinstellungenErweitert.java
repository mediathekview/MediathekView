package mediathek.gui.dialogEinstellungen;

import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.gui.messages.ProgramLocationChangedEvent;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MVMessageDialog;
import mediathek.tool.TextCopyPasteHandler;
import net.engio.mbassy.listener.Handler;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jdesktop.swingx.VerticalLayout;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

@SuppressWarnings("serial")
public class PanelEinstellungenErweitert extends JPanel {
    private static final Logger logger = LogManager.getLogger();

    @Handler
    private void handleProgramLocationChangedEvent(ProgramLocationChangedEvent e) {
        SwingUtilities.invokeLater(this::init);
    }

    public PanelEinstellungenErweitert() {
        initComponents();

        init();
        setIcon();

        jCheckBoxAboSuchen.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ABOS_SOFORT_SUCHEN)));
        jCheckBoxAboSuchen.addActionListener(e -> MVConfig.add(MVConfig.Configs.SYSTEM_ABOS_SOFORT_SUCHEN, Boolean.toString(jCheckBoxAboSuchen.isSelected())));
        jCheckBoxDownloadSofortStarten.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DOWNLOAD_SOFORT_STARTEN)));
        jCheckBoxDownloadSofortStarten.addActionListener(e -> MVConfig.add(MVConfig.Configs.SYSTEM_DOWNLOAD_SOFORT_STARTEN, Boolean.toString(jCheckBoxDownloadSofortStarten.isSelected())));

        jButtonProgrammDateimanager.addActionListener(new BeobPfad(MVConfig.Configs.SYSTEM_ORDNER_OEFFNEN, "Dateimanager suchen", jTextFieldProgrammDateimanager));
        jButtonProgrammVideoplayer.addActionListener(new BeobPfad(MVConfig.Configs.SYSTEM_PLAYER_ABSPIELEN, "Videoplayer suchen", jTextFieldVideoplayer));
        jButtonProgrammUrl.addActionListener(new BeobPfad(MVConfig.Configs.SYSTEM_URL_OEFFNEN, "Browser suchen", jTextFieldProgrammUrl));
        jButtonProgrammShutdown.addActionListener(new BeobPfad(MVConfig.Configs.SYSTEM_LINUX_SHUTDOWN, "Shutdown Befehl", jTextFieldProgrammShutdown));

        jTextFieldProgrammDateimanager.setText(MVConfig.get(MVConfig.Configs.SYSTEM_ORDNER_OEFFNEN));
        jTextFieldProgrammDateimanager.getDocument().addDocumentListener(new BeobDoc(MVConfig.Configs.SYSTEM_ORDNER_OEFFNEN, jTextFieldProgrammDateimanager));
        var handler = new TextCopyPasteHandler<>(jTextFieldProgrammDateimanager);
        jTextFieldProgrammDateimanager.setComponentPopupMenu(handler.getPopupMenu());

        jTextFieldVideoplayer.setText(MVConfig.get(MVConfig.Configs.SYSTEM_PLAYER_ABSPIELEN));
        jTextFieldVideoplayer.getDocument().addDocumentListener(new BeobDoc(MVConfig.Configs.SYSTEM_PLAYER_ABSPIELEN, jTextFieldVideoplayer));
        handler = new TextCopyPasteHandler<>(jTextFieldVideoplayer);
        jTextFieldVideoplayer.setComponentPopupMenu(handler.getPopupMenu());

        jTextFieldProgrammUrl.setText(getWebBrowserLocation());
        jTextFieldProgrammUrl.getDocument().addDocumentListener(new BeobDoc(MVConfig.Configs.SYSTEM_URL_OEFFNEN, jTextFieldProgrammUrl));
        handler = new TextCopyPasteHandler<>(jTextFieldProgrammUrl);
        jTextFieldProgrammUrl.setComponentPopupMenu(handler.getPopupMenu());

        jTextFieldProgrammShutdown.setText(MVConfig.get(MVConfig.Configs.SYSTEM_LINUX_SHUTDOWN));
        if (jTextFieldProgrammShutdown.getText().isEmpty()) {
            jTextFieldProgrammShutdown.setText(Konstanten.SHUTDOWN_LINUX);
            MVConfig.add(MVConfig.Configs.SYSTEM_LINUX_SHUTDOWN, Konstanten.SHUTDOWN_LINUX);
        }
        jTextFieldProgrammShutdown.getDocument().addDocumentListener(new BeobDoc(MVConfig.Configs.SYSTEM_LINUX_SHUTDOWN, jTextFieldProgrammShutdown));
        handler = new TextCopyPasteHandler<>(jTextFieldProgrammShutdown);
        jTextFieldProgrammShutdown.setComponentPopupMenu(handler.getPopupMenu());

        if (!SystemUtils.IS_OS_LINUX) {
            jTextFieldProgrammShutdown.setEnabled(false);
            jButtonProgrammShutdown.setEnabled(false);
        }

        Daten.getInstance().getMessageBus().subscribe(this);
    }

    private String getWebBrowserLocation() {
        return MVConfig.get(MVConfig.Configs.SYSTEM_URL_OEFFNEN);
    }

    private void init() {
        jTextFieldProgrammDateimanager.setText(MVConfig.get(MVConfig.Configs.SYSTEM_ORDNER_OEFFNEN));
        jTextFieldProgrammUrl.setText(getWebBrowserLocation());
    }

    private void setIcon() {
        jButtonProgrammDateimanager.setIcon(Icons.ICON_BUTTON_FILE_OPEN);
        jButtonProgrammVideoplayer.setIcon(Icons.ICON_BUTTON_FILE_OPEN);
        jButtonProgrammUrl.setIcon(Icons.ICON_BUTTON_FILE_OPEN);
        jButtonProgrammShutdown.setIcon(Icons.ICON_BUTTON_FILE_OPEN);
    }

    static private class BeobDoc implements DocumentListener {

        MVConfig.Configs config;
        JTextField txt;

        public BeobDoc(MVConfig.Configs config, JTextField txt) {
            this.config = config;
            this.txt = txt;
        }

        @Override
        public void insertUpdate(DocumentEvent e) {
            tus();
        }

        @Override
        public void removeUpdate(DocumentEvent e) {
            tus();
        }

        @Override
        public void changedUpdate(DocumentEvent e) {
            tus();
        }

        private void tus() {
            MVConfig.add(config, txt.getText());
        }

    }

    static private class BeobPfad implements ActionListener {

        MVConfig.Configs config;
        String title;
        JTextField textField;

        public BeobPfad(MVConfig.Configs config, String title, JTextField textField) {
            this.config = config;
            this.title = title;
            this.textField = textField;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            //we can use native chooser on Mac...
            if (SystemUtils.IS_OS_MAC_OSX) {
                FileDialog chooser = new FileDialog(MediathekGui.ui(), title);
                chooser.setMode(FileDialog.LOAD);
                chooser.setVisible(true);
                if (chooser.getFile() != null) {
                    try {
                        File destination = new File(chooser.getDirectory() + chooser.getFile());
                        textField.setText(destination.getAbsolutePath());
                    } catch (Exception ex) {
                        logger.error("BeobPfad.actionPerformed", ex);
                    }
                }
            } else {
                int returnVal;
                JFileChooser chooser = new JFileChooser();
                if (!textField.getText().isEmpty()) {
                    chooser.setCurrentDirectory(new File(textField.getText()));
                } else {
                    chooser.setCurrentDirectory(new File(GuiFunktionen.getHomePath()));
                }
                chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                returnVal = chooser.showOpenDialog(null);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        textField.setText(chooser.getSelectedFile().getAbsolutePath());
                    } catch (Exception ex) {
                        logger.error("BeobPfad.actionPerformed", ex);
                    }
                }
            }
            // merken und prüfen
            MVConfig.add(config, textField.getText());
            String programm = textField.getText();
            if (!programm.isEmpty()) {
                try {
                    if (!new File(programm).exists()) {
                        MVMessageDialog.showMessageDialog(MediathekGui.ui(), "Das Programm:  " + "\"" + programm + "\"" + "  existiert nicht!", "Fehler", JOptionPane.ERROR_MESSAGE);
                    } else if (!new File(programm).canExecute()) {
                        MVMessageDialog.showMessageDialog(MediathekGui.ui(), "Das Programm:  " + "\"" + programm + "\"" + "  kann nicht ausgeführt werden!", "Fehler", JOptionPane.ERROR_MESSAGE);
                    }
                } catch (Exception ignored) {
                }
            }

        }

    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jPanel6 = new JPanel();
        jCheckBoxAboSuchen = new JCheckBox();
        jCheckBoxDownloadSofortStarten = new JCheckBox();
        var jPanel2 = new JPanel();
        jTextFieldProgrammDateimanager = new JTextField();
        jButtonProgrammDateimanager = new JButton();
        var jLabel1 = new JLabel();
        var jLabel2 = new JLabel();
        jTextFieldVideoplayer = new JTextField();
        jButtonProgrammVideoplayer = new JButton();
        var jPanel4 = new JPanel();
        jTextFieldProgrammUrl = new JTextField();
        jButtonProgrammUrl = new JButton();
        var jPanel3 = new JPanel();
        jButtonProgrammShutdown = new JButton();
        jTextFieldProgrammShutdown = new JTextField();

        //======== this ========
        setLayout(new VerticalLayout(5));

        //======== jPanel6 ========
        {
            jPanel6.setBorder(new TitledBorder("Nach dem Neuladen der Filmliste")); //NON-NLS
            jPanel6.setLayout(new MigLayout(
                new LC().insets("5").hideMode(3).gridGap("5", "5"), //NON-NLS
                // columns
                new AC()
                    .grow().fill(),
                // rows
                new AC()
                    .fill().gap()
                    .fill()));

            //---- jCheckBoxAboSuchen ----
            jCheckBoxAboSuchen.setText("Abos automatisch suchen"); //NON-NLS
            jCheckBoxAboSuchen.setToolTipText("<html>Nach dem Neuladen einer Filmliste wird dann sofort nach neuen Abos gesucht.<br>Ansonsten muss man im Tab Download auf <i>Downloadliste aktualisieren</i> klicken.</html>"); //NON-NLS
            jPanel6.add(jCheckBoxAboSuchen, new CC().cell(0, 0));

            //---- jCheckBoxDownloadSofortStarten ----
            jCheckBoxDownloadSofortStarten.setText("Downloads aus Abos sofort starten"); //NON-NLS
            jCheckBoxDownloadSofortStarten.setToolTipText("<html>Neu angelegte Downloads (aus Abos) werden sofort gestartet.<br>Ansonsten muss man sie selbst starten.</html>"); //NON-NLS
            jPanel6.add(jCheckBoxDownloadSofortStarten, new CC().cell(0, 1));
        }
        add(jPanel6);

        //======== jPanel2 ========
        {
            jPanel2.setBorder(new TitledBorder("Tab Downloads")); //NON-NLS
            jPanel2.setLayout(new MigLayout(
                new LC().insets("5").hideMode(3).gridGap("5", "5"), //NON-NLS
                // columns
                new AC()
                    .grow().fill().gap()
                    .fill(),
                // rows
                new AC()
                    .fill().gap()
                    .fill().gap()
                    .fill().gap()
                    .fill()));

            //---- jTextFieldProgrammDateimanager ----
            jTextFieldProgrammDateimanager.setToolTipText("<html>Im Tab <i>Downloads</i> kann man mit der rechten Maustaste den Downloadordner (Zielordner) des jeweiligen Downloads \u00f6ffnen.<br>Normalerweise wird der Dateimanager des Betriebssystems gefunden und ge\u00f6ffnet.<br><br>Klappt das nicht, kann hier ein Programm daf\u00fcr angegeben werden.</html>"); //NON-NLS
            jPanel2.add(jTextFieldProgrammDateimanager, new CC().cell(0, 1));

            //---- jButtonProgrammDateimanager ----
            jButtonProgrammDateimanager.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png"))); //NON-NLS
            jButtonProgrammDateimanager.setToolTipText("Programm ausw\u00e4hlen"); //NON-NLS
            jPanel2.add(jButtonProgrammDateimanager, new CC().cell(1, 1));

            //---- jLabel1 ----
            jLabel1.setText("Datei-Manager zum \u00d6ffnen des Downloadordners:"); //NON-NLS
            jPanel2.add(jLabel1, new CC().cell(0, 0, 2, 1));

            //---- jLabel2 ----
            jLabel2.setText("Videoplayer zum Abspielen gespeicherter Filme:"); //NON-NLS
            jPanel2.add(jLabel2, new CC().cell(0, 2, 2, 1));

            //---- jTextFieldVideoplayer ----
            jTextFieldVideoplayer.setToolTipText("<html>Im Tab <i>Downloads</i> kann man den gespeicherten Film in einem Videoplayer \u00f6ffnen.<br>Normalerweise wird der Videoplayer des Betriebssystems gefunden und ge\u00f6ffnet.<br>Klappt das nicht, kann hier ein Programm als Alternative angegeben werden.</html>"); //NON-NLS
            jPanel2.add(jTextFieldVideoplayer, new CC().cell(0, 3));

            //---- jButtonProgrammVideoplayer ----
            jButtonProgrammVideoplayer.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png"))); //NON-NLS
            jButtonProgrammVideoplayer.setToolTipText("Programm ausw\u00e4hlen"); //NON-NLS
            jPanel2.add(jButtonProgrammVideoplayer, new CC().cell(1, 3));
        }
        add(jPanel2);

        //======== jPanel4 ========
        {
            jPanel4.setBorder(new TitledBorder("Webbrowser zum \u00d6ffnen von URLs")); //NON-NLS
            jPanel4.setToolTipText("<html>Wenn das Programm versucht, einen Link zu \u00f6ffnen und die Standardanwendung nicht startet, kann damit ein Programm ausgew\u00e4hlt und fest zugeordnet werden.</html>"); //NON-NLS
            jPanel4.setLayout(new MigLayout(
                new LC().insets("5").hideMode(3).gridGap("5", "5"), //NON-NLS
                // columns
                new AC()
                    .grow().fill().gap()
                    .fill(),
                // rows
                new AC()
                    .fill()));
            jPanel4.add(jTextFieldProgrammUrl, new CC().cell(0, 0));

            //---- jButtonProgrammUrl ----
            jButtonProgrammUrl.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png"))); //NON-NLS
            jButtonProgrammUrl.setToolTipText("Programm ausw\u00e4hlen"); //NON-NLS
            jPanel4.add(jButtonProgrammUrl, new CC().cell(1, 0));
        }
        add(jPanel4);

        //======== jPanel3 ========
        {
            jPanel3.setBorder(new TitledBorder("Linux: Aufruf zum Shutdown")); //NON-NLS
            jPanel3.setToolTipText("<html>Unter Linux wird das ausgew\u00e4hlte Programm/Script ausgef\u00fchrt um den Recher herunter zu fahren.<br>M\u00f6gliche Aufrufe sind:<br>\n<ul>\n<li>systemctl poweroff</li>\n<li>poweroff</li>\n<li>sudo shutdown -P now</li>\n<li><b>shutdown -h now</b></li>\n</ul>\n</html>"); //NON-NLS
            jPanel3.setLayout(new MigLayout(
                new LC().insets("5").hideMode(3).gridGap("5", "5"), //NON-NLS
                // columns
                new AC()
                    .grow().fill().gap()
                    .fill(),
                // rows
                new AC()
                    .fill()));

            //---- jButtonProgrammShutdown ----
            jButtonProgrammShutdown.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png"))); //NON-NLS
            jButtonProgrammShutdown.setToolTipText("Programm/Script ausw\u00e4hlen"); //NON-NLS
            jPanel3.add(jButtonProgrammShutdown, new CC().cell(1, 0));

            //---- jTextFieldProgrammShutdown ----
            jTextFieldProgrammShutdown.setText("shutdown -h now"); //NON-NLS
            jPanel3.add(jTextFieldProgrammShutdown, new CC().cell(0, 0));
        }
        add(jPanel3);
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JCheckBox jCheckBoxAboSuchen;
    private JCheckBox jCheckBoxDownloadSofortStarten;
    private JTextField jTextFieldProgrammDateimanager;
    private JButton jButtonProgrammDateimanager;
    private JTextField jTextFieldVideoplayer;
    private JButton jButtonProgrammVideoplayer;
    private JTextField jTextFieldProgrammUrl;
    private JButton jButtonProgrammUrl;
    private JButton jButtonProgrammShutdown;
    private JTextField jTextFieldProgrammShutdown;
    // End of variables declaration//GEN-END:variables
}
