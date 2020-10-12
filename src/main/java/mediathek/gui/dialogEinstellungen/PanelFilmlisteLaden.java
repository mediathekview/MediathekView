package mediathek.gui.dialogEinstellungen;

import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVColor;
import mediathek.config.MVConfig;
import mediathek.gui.messages.FilmListImportTypeChangedEvent;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.*;
import net.engio.mbassy.listener.Handler;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

@SuppressWarnings("serial")
public class PanelFilmlisteLaden extends JPanel {
    public PanelFilmlisteLaden() {
        super();

        Daten.getInstance().getMessageBus().subscribe(this);

        initComponents();
        init();

        final var config = ApplicationConfiguration.getConfiguration();
        cbSign.setSelected(config.getBoolean(ApplicationConfiguration.FILMLIST_LOAD_SIGNLANGUAGE,true));
        cbSign.addActionListener(e -> config.setProperty(ApplicationConfiguration.FILMLIST_LOAD_SIGNLANGUAGE,cbSign.isSelected()));

        cbAudio.setSelected(config.getBoolean(ApplicationConfiguration.FILMLIST_LOAD_AUDIODESCRIPTION,true));
        cbAudio.addActionListener(e -> config.setProperty(ApplicationConfiguration.FILMLIST_LOAD_AUDIODESCRIPTION,cbAudio.isSelected()));

        cbTrailer.setSelected(config.getBoolean(ApplicationConfiguration.FILMLIST_LOAD_TRAILER,true));
        cbTrailer.addActionListener(e -> config.setProperty(ApplicationConfiguration.FILMLIST_LOAD_TRAILER,cbTrailer.isSelected()));

        cbLivestreams.setSelected(config.getBoolean(ApplicationConfiguration.FILMLIST_LOAD_LIVESTREAMS, true));
        cbLivestreams.addActionListener(e -> config.setProperty(ApplicationConfiguration.FILMLIST_LOAD_LIVESTREAMS, cbLivestreams.isSelected()));
    }

    private void init() {
        initRadio();

        final var filmeLaden = Daten.getInstance().getFilmeLaden();
        jButtonLoad.addActionListener(ae -> filmeLaden.loadFilmlist("", false));

        jButtonDateiAuswaehlen.setIcon(Icons.ICON_BUTTON_FILE_OPEN);
        jButtonDateiAuswaehlen.addActionListener(l -> {
            var loadFile = FileDialogs.chooseLoadFileLocation(MediathekGui.ui(),"Filmliste laden", "");
            if (loadFile != null) {
                jTextFieldUrl.setText(loadFile.getAbsolutePath());
            }
        });

        jButtonFilmeLaden.addActionListener(e -> {
            if (jCheckBoxUpdate.isSelected())
                filmeLaden.updateFilmlist(jTextFieldUrl.getText());
            else
                filmeLaden.loadFilmlist(jTextFieldUrl.getText(), false);
        });

        var listener = new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (jRadioButtonManuell.isSelected())
                    GuiFunktionen.setImportArtFilme(FilmListUpdateType.MANUAL);
                else
                    GuiFunktionen.setImportArtFilme(FilmListUpdateType.AUTOMATIC);

                Daten.getInstance().getMessageBus().publishAsync(new FilmListImportTypeChangedEvent());
            }
        };
        jRadioButtonManuell.addActionListener(listener);
        jRadioButtonAuto.addActionListener(listener);

        jTextFieldUrl.getDocument().addDocumentListener(new BeobDateiUrl());
        TextCopyPasteHandler<JTextField> handler = new TextCopyPasteHandler<>(jTextFieldUrl);
        jTextFieldUrl.setComponentPopupMenu(handler.getPopupMenu());
    }

    @Handler
    private void handleFilmListImportTypeChanged(FilmListImportTypeChangedEvent e) {
        SwingUtilities.invokeLater(this::initRadio);
    }

    private void initRadio() {
        switch (GuiFunktionen.getImportArtFilme()) {
            case MANUAL -> jRadioButtonManuell.setSelected(true);
            case AUTOMATIC -> jRadioButtonAuto.setSelected(true);
        }

        jTextFieldUrl.setText(MVConfig.get(MVConfig.Configs.SYSTEM_IMPORT_URL_MANUELL));
        setPanelTabelle(jRadioButtonManuell.isSelected());
    }

    private void setPanelTabelle(boolean manuell) {
        if (manuell) {
            jTextAreaManuell.setBackground(MVColor.FILMLISTE_LADEN_AKTIV.color);
            jTextAreaAuto.setBackground(null);
        } else {
            jTextAreaManuell.setBackground(null);
            jTextAreaAuto.setBackground(MVColor.FILMLISTE_LADEN_AKTIV.color);
        }
    }

    private class BeobDateiUrl implements DocumentListener {

        @Override
        public void insertUpdate(DocumentEvent e) {
            turnOnManualImport();
        }

        @Override
        public void removeUpdate(DocumentEvent e) {
            turnOnManualImport();
        }

        @Override
        public void changedUpdate(DocumentEvent e) {
            turnOnManualImport();
        }

        private void turnOnManualImport() {
            MVConfig.add(MVConfig.Configs.SYSTEM_IMPORT_URL_MANUELL, jTextFieldUrl.getText());
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jPanelAuto = new JPanel();
        jTextAreaAuto = new JTextArea();
        jButtonLoad = new JButton();
        var jPanelManuel = new JPanel();
        var jLabel1 = new JLabel();
        jTextFieldUrl = new JTextField();
        jButtonDateiAuswaehlen = new JButton();
        jButtonFilmeLaden = new JButton();
        jTextAreaManuell = new JTextArea();
        jCheckBoxUpdate = new JCheckBox();
        jRadioButtonAuto = new JRadioButton();
        jRadioButtonManuell = new JRadioButton();
        var jPanel1 = new JPanel();
        cbSign = new JCheckBox();
        cbTrailer = new JCheckBox();
        cbAudio = new JCheckBox();
        cbLivestreams = new JCheckBox();

        //======== this ========
        setMinimumSize(new Dimension(746, 400));
        setPreferredSize(new Dimension(746, 400));
        setLayout(new MigLayout(
            new LC().insets("5").hideMode(3).gridGap("5", "5"),
            // columns
            new AC()
                .fill().gap()
                .grow().fill(),
            // rows
            new AC()
                .fill().gap()
                .fill().gap()
                .fill()));

        //======== jPanelAuto ========
        {
            jPanelAuto.setBorder(new TitledBorder("Die Filmliste automatisch laden"));
            jPanelAuto.setLayout(new MigLayout(
                new LC().insets("5").hideMode(3).gridGap("5", "5"),
                // columns
                new AC()
                    .grow().fill().gap()
                    .grow().fill(),
                // rows
                new AC()
                    .fill().gap()
                    .fill()));

            //---- jTextAreaAuto ----
            jTextAreaAuto.setEditable(false);
            jTextAreaAuto.setColumns(20);
            jTextAreaAuto.setRows(3);
            jTextAreaAuto.setText("Die Filmliste wird beim Programmstart automatisch geladen (wenn sie \u00e4lter als 3h ist). Zus\u00e4tzlich kann sie \u00fcber den Button \"Neue Filmliste laden\" aktualisiert werden. Zum Update werden dann nur noch die Differenzlisten geladen (enthalten nur die neuen Filme).");
            jTextAreaAuto.setMargin(new Insets(4, 4, 4, 4));
            jTextAreaAuto.setWrapStyleWord(true);
            jTextAreaAuto.setLineWrap(true);
            jTextAreaAuto.setFont(jTextAreaAuto.getFont().deriveFont(jTextAreaAuto.getFont().getSize() - 1f));
            jPanelAuto.add(jTextAreaAuto, new CC().cell(0, 0, 2, 1));

            //---- jButtonLoad ----
            jButtonLoad.setText("Filme jetzt laden");
            jPanelAuto.add(jButtonLoad, new CC().cell(1, 1).alignX("trailing").growX(0));
        }
        add(jPanelAuto, new CC().cell(1, 0));

        //======== jPanelManuel ========
        {
            jPanelManuel.setBorder(new TitledBorder("Filmliste nur manuell laden"));
            jPanelManuel.setLayout(new MigLayout(
                new LC().insets("5").hideMode(3).gridGap("5", "5"),
                // columns
                new AC()
                    .fill().gap()
                    .grow().fill().gap()
                    .fill().gap()
                    .fill(),
                // rows
                new AC()
                    .fill().gap()
                    .fill().gap()
                    .fill()));

            //---- jLabel1 ----
            jLabel1.setText("URL/Datei:");
            jPanelManuel.add(jLabel1, new CC().cell(0, 1));
            jPanelManuel.add(jTextFieldUrl, new CC().cell(1, 1, 2, 1));

            //---- jButtonDateiAuswaehlen ----
            jButtonDateiAuswaehlen.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png")));
            jButtonDateiAuswaehlen.setToolTipText("URL oder lokale Filmliste ausw\u00e4hlen");
            jPanelManuel.add(jButtonDateiAuswaehlen, new CC().cell(3, 1).alignX("left").growX(0).width("32:32:32").height("32:32:32"));

            //---- jButtonFilmeLaden ----
            jButtonFilmeLaden.setText("Filme jetzt laden");
            jPanelManuel.add(jButtonFilmeLaden, new CC().cell(2, 2, 2, 1));

            //---- jTextAreaManuell ----
            jTextAreaManuell.setEditable(false);
            jTextAreaManuell.setColumns(20);
            jTextAreaManuell.setRows(2);
            jTextAreaManuell.setText("Die Filmliste wird nur manuell \u00fcber den Button \"Neue Filmliste laden\" geladen. Es wird dann dieser Dialog angezeigt und es kann eine URL/Datei zum Laden angegeben werden.");
            jTextAreaManuell.setMargin(new Insets(4, 4, 4, 4));
            jTextAreaManuell.setWrapStyleWord(true);
            jTextAreaManuell.setLineWrap(true);
            jTextAreaManuell.setFont(jTextAreaManuell.getFont().deriveFont(jTextAreaManuell.getFont().getSize() - 1f));
            jPanelManuel.add(jTextAreaManuell, new CC().cell(0, 0, 4, 1));

            //---- jCheckBoxUpdate ----
            jCheckBoxUpdate.setText("Alte Filmliste nicht l\u00f6schen, nur erweitern");
            jPanelManuel.add(jCheckBoxUpdate, new CC().cell(0, 2, 2, 1));
        }
        add(jPanelManuel, new CC().cell(1, 1));
        add(jRadioButtonAuto, new CC().cell(0, 0).alignY("top").growY(0));
        add(jRadioButtonManuell, new CC().cell(0, 1).alignY("top").growY(0));

        //======== jPanel1 ========
        {
            jPanel1.setBorder(new TitledBorder("Zus\u00e4tzliche Filmdaten laden"));
            jPanel1.setToolTipText("<html>Alle nicht angew\u00e4hlten Eintr\u00e4ge werden beim Laden der Filmliste aus dem Endergebnis herausgefiltert.<br/><b>Die Eintr\u00e4ge werden dauerhaft aus der lokalen Filmliste entfernt.</b><br/>Sie werden erst wieder beim Laden einer neuen Liste vom Server hinzugef\u00fcgt wenn die Einstellungen entsprechend angepasst wurden.</html>");
            jPanel1.setLayout(new MigLayout(
                new LC().insets("5").hideMode(3).gridGap("5", "5"),
                // columns
                new AC()
                    .fill().gap()
                    .fill().gap()
                    .fill().gap()
                    .fill(),
                // rows
                new AC()
                    .fill()));

            //---- cbSign ----
            cbSign.setText("Geb\u00e4rdensprache");
            jPanel1.add(cbSign, new CC().cell(2, 0));

            //---- cbTrailer ----
            cbTrailer.setText("Trailer/Teaser/Vorschau");
            jPanel1.add(cbTrailer, new CC().cell(0, 0));

            //---- cbAudio ----
            cbAudio.setText("H\u00f6rfassungen");
            jPanel1.add(cbAudio, new CC().cell(1, 0));

            //---- cbLivestreams ----
            cbLivestreams.setText("Livestreams");
            jPanel1.add(cbLivestreams, new CC().cell(3, 0));
        }
        add(jPanel1, new CC().cell(1, 2));

        //---- buttonGroup1 ----
        var buttonGroup1 = new ButtonGroup();
        buttonGroup1.add(jRadioButtonAuto);
        buttonGroup1.add(jRadioButtonManuell);
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JTextArea jTextAreaAuto;
    private JButton jButtonLoad;
    private JTextField jTextFieldUrl;
    private JButton jButtonDateiAuswaehlen;
    private JButton jButtonFilmeLaden;
    private JTextArea jTextAreaManuell;
    private JCheckBox jCheckBoxUpdate;
    private JRadioButton jRadioButtonAuto;
    private JRadioButton jRadioButtonManuell;
    private JCheckBox cbSign;
    private JCheckBox cbTrailer;
    private JCheckBox cbAudio;
    private JCheckBox cbLivestreams;
    // End of variables declaration//GEN-END:variables
}
