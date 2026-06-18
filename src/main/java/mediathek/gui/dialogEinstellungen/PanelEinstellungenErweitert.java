package mediathek.gui.dialogEinstellungen;

import mediathek.config.application.ApplicationConfiguration;
import mediathek.gui.dialogEinstellungen.shutdown.ShutdownActionComboBox;
import mediathek.gui.messages.ProgramLocationChangedEvent;
import mediathek.tool.MessageBus;
import mediathek.tool.SVGIconUtilities;
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
import java.util.function.Consumer;

public class PanelEinstellungenErweitert extends JPanel {
    private static final Logger logger = LogManager.getLogger();
    private final Frame owner;

    @Handler
    private void handleProgramLocationChangedEvent(ProgramLocationChangedEvent e) {
        SwingUtilities.invokeLater(this::init);
    }

    public PanelEinstellungenErweitert(Frame owner) {
        this.owner = owner;
        initComponents();

        init();
        setFolderIcons();

        var applicationConfiguration = ApplicationConfiguration.getInstance();
        jCheckBoxAboSuchen.setSelected(applicationConfiguration.getSearchAbosImmediately());
        jCheckBoxAboSuchen.addActionListener(_ -> applicationConfiguration.setSearchAbosImmediately(jCheckBoxAboSuchen.isSelected()));
        jCheckBoxDownloadSofortStarten.setSelected(applicationConfiguration.getStartDownloadsImmediately());
        jCheckBoxDownloadSofortStarten.addActionListener(_ -> applicationConfiguration.setStartDownloadsImmediately(jCheckBoxDownloadSofortStarten.isSelected()));

        jButtonProgrammDateimanager.addActionListener(new BeobPfad(
                owner, applicationConfiguration::setDirectoryOpenProgram, "Dateimanager suchen", jTextFieldProgrammDateimanager));
        jButtonProgrammVideoplayer.addActionListener(new BeobPfad(
                owner, applicationConfiguration::setVideoPlayerProgram, "Videoplayer suchen", jTextFieldVideoplayer));
        jButtonProgrammUrl.addActionListener(new BeobPfad(
                owner, applicationConfiguration::setWebBrowserProgram, "Browser suchen", jTextFieldProgrammUrl));
        jButtonProgrammShutdown.addActionListener(new BeobPfad(
                owner, applicationConfiguration::setLinuxShutdownCommand, "Shutdown Befehl", jTextFieldProgrammShutdown));

        jTextFieldProgrammDateimanager.setText(applicationConfiguration.getDirectoryOpenProgram());
        jTextFieldProgrammDateimanager.getDocument().addDocumentListener(new BeobAppConfigDoc(
                applicationConfiguration::setDirectoryOpenProgram, jTextFieldProgrammDateimanager));
        var handler = new TextCopyPasteHandler<>(jTextFieldProgrammDateimanager);
        jTextFieldProgrammDateimanager.setComponentPopupMenu(handler.getPopupMenu());

        jTextFieldVideoplayer.setText(applicationConfiguration.getVideoPlayerProgram());
        jTextFieldVideoplayer.getDocument().addDocumentListener(new BeobAppConfigDoc(
                applicationConfiguration::setVideoPlayerProgram, jTextFieldVideoplayer));
        handler = new TextCopyPasteHandler<>(jTextFieldVideoplayer);
        jTextFieldVideoplayer.setComponentPopupMenu(handler.getPopupMenu());

        jTextFieldProgrammUrl.setText(applicationConfiguration.getWebBrowserProgram());
        jTextFieldProgrammUrl.getDocument().addDocumentListener(new BeobAppConfigDoc(
                applicationConfiguration::setWebBrowserProgram, jTextFieldProgrammUrl));
        handler = new TextCopyPasteHandler<>(jTextFieldProgrammUrl);
        jTextFieldProgrammUrl.setComponentPopupMenu(handler.getPopupMenu());

        jTextFieldProgrammShutdown.setText(applicationConfiguration.getLinuxShutdownCommand());
        jTextFieldProgrammShutdown.getDocument().addDocumentListener(new BeobAppConfigDoc(
                applicationConfiguration::setLinuxShutdownCommand, jTextFieldProgrammShutdown));
        handler = new TextCopyPasteHandler<>(jTextFieldProgrammShutdown);
        jTextFieldProgrammShutdown.setComponentPopupMenu(handler.getPopupMenu());

        setupJDownloaderFields();
        setupPyLoadFields();
        hideOsSpecificFields();

        MessageBus.getMessageBus().subscribe(this);
    }

    private void setupJDownloaderFields() {
        var applicationConfiguration = ApplicationConfiguration.getInstance();
        jTextFieldJDownloaderUrl.setText(applicationConfiguration.getJDownloaderUrl());
        jTextFieldJDownloaderUrl.getDocument().addDocumentListener(new BeobAppConfigDoc(
                applicationConfiguration::setJDownloaderUrl, jTextFieldJDownloaderUrl));
        var handler = new TextCopyPasteHandler<>(jTextFieldJDownloaderUrl);
        jTextFieldJDownloaderUrl.setComponentPopupMenu(handler.getPopupMenu());
    }

    private void setupPyLoadFields() {
        var applicationConfiguration = ApplicationConfiguration.getInstance();
        jTextFieldPyLoadUrl.setText(applicationConfiguration.getPyLoadUrl());
        jTextFieldPyLoadUrl.getDocument().addDocumentListener(new BeobAppConfigDoc(
                applicationConfiguration::setPyLoadUrl, jTextFieldPyLoadUrl));
        var handler = new TextCopyPasteHandler<>(jTextFieldPyLoadUrl);
        jTextFieldPyLoadUrl.setComponentPopupMenu(handler.getPopupMenu());

        jTextFieldPyLoadUser.setText(applicationConfiguration.getPyLoadUser());
        jTextFieldPyLoadUser.getDocument().addDocumentListener(new BeobAppConfigDoc(
                applicationConfiguration::setPyLoadUser, jTextFieldPyLoadUser));
        handler = new TextCopyPasteHandler<>(jTextFieldPyLoadUser);
        jTextFieldPyLoadUser.setComponentPopupMenu(handler.getPopupMenu());

        jPasswordFieldPyLoadPassword.setText(applicationConfiguration.getPyLoadPassword());
        jPasswordFieldPyLoadPassword.getDocument().addDocumentListener(new PyLoadPasswordDocumentListener());
    }

    private void hideOsSpecificFields() {
        if (!SystemUtils.IS_OS_LINUX) {
            jTextFieldProgrammShutdown.setEnabled(false);
            jButtonProgrammShutdown.setEnabled(false);
            pnlLinuxShutdownCommand.setVisible(false);
        }

        if (!SystemUtils.IS_OS_MAC_OSX) {
            cbDefaultShutdownHelperCommand.setEnabled(false);
            pnlMacShutdownBehaviour.setVisible(false);
        }
    }

    private void init() {
        var applicationConfiguration = ApplicationConfiguration.getInstance();
        jTextFieldProgrammDateimanager.setText(applicationConfiguration.getDirectoryOpenProgram());
        jTextFieldVideoplayer.setText(applicationConfiguration.getVideoPlayerProgram());
        jTextFieldProgrammUrl.setText(applicationConfiguration.getWebBrowserProgram());
    }

    private void setFolderIcons() {
        var icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg");
        jButtonProgrammDateimanager.setIcon(icon);
        jButtonProgrammVideoplayer.setIcon(icon);
        jButtonProgrammUrl.setIcon(icon);
        jButtonProgrammShutdown.setIcon(icon);
    }

    static private class BeobAppConfigDoc implements DocumentListener {

        final Consumer<String> valueWriter;
        final JTextField txt;

        public BeobAppConfigDoc(Consumer<String> valueWriter, JTextField txt) {
            this.valueWriter = valueWriter;
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
            valueWriter.accept(txt.getText());
        }
    }

    private class PyLoadPasswordDocumentListener implements DocumentListener {
        private void update() {
            ApplicationConfiguration.getInstance().setPyLoadPassword(new String(jPasswordFieldPyLoadPassword.getPassword()));
        }

        @Override
        public void insertUpdate(DocumentEvent e) {
            update();
        }

        @Override
        public void removeUpdate(DocumentEvent e) {
            update();
        }

        @Override
        public void changedUpdate(DocumentEvent e) {
            update();
        }
    }

    static private class BeobPfad implements ActionListener {

        final Frame owner;
        final Consumer<String> valueWriter;
        final String title;
        final JTextField textField;

        public BeobPfad(Frame owner, Consumer<String> valueWriter, String title, JTextField textField) {
            this.owner = owner;
            this.valueWriter = valueWriter;
            this.title = title;
            this.textField = textField;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            //we can use native chooser on Mac...
            if (SystemUtils.IS_OS_MAC_OSX) {
                FileDialog chooser = new FileDialog(owner, title);
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
                    chooser.setCurrentDirectory(new File(SystemUtils.USER_HOME));
                }
                chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                returnVal = chooser.showOpenDialog(owner);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        textField.setText(chooser.getSelectedFile().getAbsolutePath());
                    } catch (Exception ex) {
                        logger.error("BeobPfad.actionPerformed", ex);
                    }
                }
            }
            // merken und prüfen
            valueWriter.accept(textField.getText());
            String programm = textField.getText();
            if (!programm.isEmpty()) {
                try {
                    if (!new File(programm).exists()) {
                        JOptionPane.showMessageDialog(owner, "Das Programm:  " + "\"" + programm + "\"" + "  existiert nicht!", "Fehler", JOptionPane.ERROR_MESSAGE);
                    } else if (!new File(programm).canExecute()) {
                        JOptionPane.showMessageDialog(owner, "Das Programm:  " + "\"" + programm + "\"" + "  kann nicht ausgeführt werden!", "Fehler", JOptionPane.ERROR_MESSAGE);
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
        var jPanelJDownloader = new JPanel();
        var label1 = new JLabel();
        jTextFieldJDownloaderUrl = new JTextField();
        var jPanelPyLoad = new JPanel();
        var label2 = new JLabel();
        jTextFieldPyLoadUrl = new JTextField();
        var label3 = new JLabel();
        jTextFieldPyLoadUser = new JTextField();
        var label4 = new JLabel();
        jPasswordFieldPyLoadPassword = new JPasswordField();
        pnlLinuxShutdownCommand = new JPanel();
        jButtonProgrammShutdown = new JButton();
        jTextFieldProgrammShutdown = new JTextField();
        pnlMacShutdownBehaviour = new JPanel();
        cbDefaultShutdownHelperCommand = new ShutdownActionComboBox();

        //======== this ========
        setLayout(new VerticalLayout(5));

        //======== jPanel6 ========
        {
            jPanel6.setBorder(new TitledBorder("Nach dem Neuladen der Filmliste"));
            jPanel6.setLayout(new MigLayout(
                new LC().insets("5").hideMode(3).gridGap("5", "5"),
                // columns
                new AC()
                    .grow().fill(),
                // rows
                new AC()
                    .fill().gap()
                    .fill()));

            //---- jCheckBoxAboSuchen ----
            jCheckBoxAboSuchen.setText("Abos automatisch suchen");
            jCheckBoxAboSuchen.setToolTipText("<html>Nach dem Neuladen einer Filmliste wird dann sofort nach neuen Abos gesucht.<br>Ansonsten muss man im Tab Download auf <i>Downloadliste aktualisieren</i> klicken.</html>");
            jPanel6.add(jCheckBoxAboSuchen, new CC().cell(0, 0));

            //---- jCheckBoxDownloadSofortStarten ----
            jCheckBoxDownloadSofortStarten.setText("Downloads aus Abos sofort starten");
            jCheckBoxDownloadSofortStarten.setToolTipText("<html>Neu angelegte Downloads (aus Abos) werden sofort gestartet.<br>Ansonsten muss man sie selbst starten.</html>");
            jPanel6.add(jCheckBoxDownloadSofortStarten, new CC().cell(0, 1));
        }
        add(jPanel6);

        //======== jPanel2 ========
        {
            jPanel2.setBorder(new TitledBorder("Tab Downloads"));
            jPanel2.setLayout(new MigLayout(
                new LC().insets("5").hideMode(3).gridGap("5", "5"),
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
            jTextFieldProgrammDateimanager.setToolTipText("<html>Im Tab <i>Downloads</i> kann man mit der rechten Maustaste den Downloadordner (Zielordner) des jeweiligen Downloads \u00f6ffnen.<br>Normalerweise wird der Dateimanager des Betriebssystems gefunden und ge\u00f6ffnet.<br><br>Klappt das nicht, kann hier ein Programm daf\u00fcr angegeben werden.</html>");
            jPanel2.add(jTextFieldProgrammDateimanager, new CC().cell(0, 1));

            //---- jButtonProgrammDateimanager ----
            jButtonProgrammDateimanager.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png")));
            jButtonProgrammDateimanager.setToolTipText("Programm ausw\u00e4hlen");
            jPanel2.add(jButtonProgrammDateimanager, new CC().cell(1, 1));

            //---- jLabel1 ----
            jLabel1.setText("Datei-Manager zum \u00d6ffnen des Downloadordners:");
            jPanel2.add(jLabel1, new CC().cell(0, 0, 2, 1));

            //---- jLabel2 ----
            jLabel2.setText("Videoplayer zum Abspielen gespeicherter Filme:");
            jPanel2.add(jLabel2, new CC().cell(0, 2, 2, 1));

            //---- jTextFieldVideoplayer ----
            jTextFieldVideoplayer.setToolTipText("<html>Im Tab <i>Downloads</i> kann man den gespeicherten Film in einem Videoplayer \u00f6ffnen.<br>Normalerweise wird der Videoplayer des Betriebssystems gefunden und ge\u00f6ffnet.<br>Klappt das nicht, kann hier ein Programm als Alternative angegeben werden.</html>");
            jPanel2.add(jTextFieldVideoplayer, new CC().cell(0, 3));

            //---- jButtonProgrammVideoplayer ----
            jButtonProgrammVideoplayer.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png")));
            jButtonProgrammVideoplayer.setToolTipText("Programm ausw\u00e4hlen");
            jPanel2.add(jButtonProgrammVideoplayer, new CC().cell(1, 3));
        }
        add(jPanel2);

        //======== jPanel4 ========
        {
            jPanel4.setBorder(new TitledBorder("Webbrowser zum \u00d6ffnen von URLs"));
            jPanel4.setToolTipText("<html>Wenn das Programm versucht, einen Link zu \u00f6ffnen und die Standardanwendung nicht startet, kann damit ein Programm ausgew\u00e4hlt und fest zugeordnet werden.</html>");
            jPanel4.setLayout(new MigLayout(
                new LC().insets("5").hideMode(3).gridGap("5", "5"),
                // columns
                new AC()
                    .grow().fill().gap()
                    .fill(),
                // rows
                new AC()
                    .fill()));
            jPanel4.add(jTextFieldProgrammUrl, new CC().cell(0, 0));

            //---- jButtonProgrammUrl ----
            jButtonProgrammUrl.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png")));
            jButtonProgrammUrl.setToolTipText("Programm ausw\u00e4hlen");
            jPanel4.add(jButtonProgrammUrl, new CC().cell(1, 0));
        }
        add(jPanel4);

        //======== jPanelJDownloader ========
        {
            jPanelJDownloader.setBorder(new TitledBorder("JDownloader"));
            jPanelJDownloader.setLayout(new MigLayout(
                new LC().insets("5").hideMode(3).gridGap("5", "5"),
                // columns
                new AC()
                    .grow().fill().gap()
                    .fill(),
                // rows
                new AC()
                    .fill().gap()
                    .fill()));

            //---- label1 ----
            label1.setText("JDownloader-URL:");
            jPanelJDownloader.add(label1, new CC().cell(0, 0, 2, 1));

            //---- jTextFieldJDownloaderUrl ----
            jTextFieldJDownloaderUrl.setToolTipText("<html>Wenn jDownloader nicht auf dem lokalen Host installiert ist oder unter einem anderen Port reagieren soll, hier bitte angeben.<br>Default: http://127.0.0.1:9666/flash/add</html>");
            jPanelJDownloader.add(jTextFieldJDownloaderUrl, new CC().cell(0, 1));
        }
        add(jPanelJDownloader);

        //======== jPanelPyLoad ========
        {
            jPanelPyLoad.setBorder(new TitledBorder("pyLoad"));
            jPanelPyLoad.setLayout(new MigLayout(
                new LC().insets("5").hideMode(3).gridGap("5", "5"),
                // columns
                new AC()
                    .grow().fill().gap()
                    .fill(),
                // rows
                new AC()
                    .fill().gap()
                    .fill().gap()
                    .fill().gap()
                    .fill().gap()
                    .fill().gap()
                    .fill()));

            //---- label2 ----
            label2.setText("pyLoad-URL:");
            jPanelPyLoad.add(label2, new CC().cell(0, 0, 2, 1));

            //---- jTextFieldPyLoadUrl ----
            jTextFieldPyLoadUrl.setToolTipText("PyLoad-URL komplett angeben (z.B.: http://127.0.0.1:8000)");
            jPanelPyLoad.add(jTextFieldPyLoadUrl, new CC().cell(0, 1));

            //---- label3 ----
            label3.setText("Benutzer:");
            jPanelPyLoad.add(label3, new CC().cell(0, 2, 2, 1));
            jPanelPyLoad.add(jTextFieldPyLoadUser, new CC().cell(0, 3));

            //---- label4 ----
            label4.setText("Passwort:");
            jPanelPyLoad.add(label4, new CC().cell(0, 4, 2, 1));
            jPanelPyLoad.add(jPasswordFieldPyLoadPassword, new CC().cell(0, 5));
        }
        add(jPanelPyLoad);

        //======== pnlLinuxShutdownCommand ========
        {
            pnlLinuxShutdownCommand.setBorder(new TitledBorder("Linux: Aufruf zum Shutdown"));
            pnlLinuxShutdownCommand.setToolTipText("<html>Unter Linux wird das ausgew\u00e4hlte Programm/Script ausgef\u00fchrt um den Recher herunter zu fahren.<br>M\u00f6gliche Aufrufe sind:<br>\n<ul>\n<li>systemctl poweroff</li>\n<li>poweroff</li>\n<li>sudo shutdown -P now</li>\n<li><b>shutdown -h now</b></li>\n</ul>\n</html>");
            pnlLinuxShutdownCommand.setLayout(new MigLayout(
                new LC().insets("5").hideMode(3).gridGap("5", "5"),
                // columns
                new AC()
                    .grow().fill().gap()
                    .fill(),
                // rows
                new AC()
                    .fill()));

            //---- jButtonProgrammShutdown ----
            jButtonProgrammShutdown.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png")));
            jButtonProgrammShutdown.setToolTipText("Programm/Script ausw\u00e4hlen");
            pnlLinuxShutdownCommand.add(jButtonProgrammShutdown, new CC().cell(1, 0));

            //---- jTextFieldProgrammShutdown ----
            jTextFieldProgrammShutdown.setText("shutdown -h now");
            pnlLinuxShutdownCommand.add(jTextFieldProgrammShutdown, new CC().cell(0, 0));
        }
        add(pnlLinuxShutdownCommand);

        //======== pnlMacShutdownBehaviour ========
        {
            pnlMacShutdownBehaviour.setBorder(new TitledBorder("macOS: Standardverhalten des Hilfsprogramms"));
            pnlMacShutdownBehaviour.setLayout(new MigLayout(
                new LC().insets("5").hideMode(3).gridGap("5", "5"),
                // columns
                new AC()
                    .grow().fill(),
                // rows
                new AC()
                    ));
            pnlMacShutdownBehaviour.add(cbDefaultShutdownHelperCommand, new CC().cell(0, 0));
        }
        add(pnlMacShutdownBehaviour);
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
    private JTextField jTextFieldJDownloaderUrl;
    private JTextField jTextFieldPyLoadUrl;
    private JTextField jTextFieldPyLoadUser;
    private JPasswordField jPasswordFieldPyLoadPassword;
    private JPanel pnlLinuxShutdownCommand;
    private JButton jButtonProgrammShutdown;
    private JTextField jTextFieldProgrammShutdown;
    private JPanel pnlMacShutdownBehaviour;
    private ShutdownActionComboBox cbDefaultShutdownHelperCommand;
    // End of variables declaration//GEN-END:variables
}
