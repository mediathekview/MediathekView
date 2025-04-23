package mediathek.gui.dialogEinstellungen;

import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.controller.SenderFilmlistLoadApprover;
import mediathek.gui.messages.FilmListImportTypeChangedEvent;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.*;
import mediathek.tool.swing.MultilineLabel;
import net.engio.mbassy.listener.Handler;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.configuration2.Configuration;
import org.jdesktop.swingx.VerticalLayout;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.util.ArrayList;
import java.util.List;

public class PanelFilmlisteLaden extends JPanel {
    private final List<JCheckBox> senderCbList = new ArrayList<>();
    private final Configuration config = ApplicationConfiguration.getConfiguration();

    private void initReloadButton() {
        btnReloadFilmlist.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/arrows-rotate.svg"));
        btnReloadFilmlist.addActionListener(l -> {
            final var daten = Daten.getInstance();
            daten.getListeFilme().clear(); // sonst wird evtl. nur eine Diff geladen
            daten.getFilmeLaden().loadFilmlist("", false);
        });
    }

    public PanelFilmlisteLaden(boolean inSettingsDialog) {
        super();

        MessageBus.getMessageBus().subscribe(this);

        initComponents();
        jButtonDateiAuswaehlen.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg"));
        init();
        initReloadButton();

        setupCheckBoxes();

        btnReloadFilmlist.setVisible(inSettingsDialog);
        //in settings we cannot load a list, therefore these controls make no sense
        if (inSettingsDialog) {
            prepareSettingsLayout();
        }
        else {
            //we are in LoadFilmListDialog integrated...
            panel1.setToolTipText("<html>Bei Änderungen wird eine komplette Filmliste vom Server geladen.<br>" +
                    "Dies funktioniert <b>NICHT im Erweiterungsmodus</b>!!!</html>");
            panel1.setBorder(new TitledBorder("Ausgewählte Sender laden:"));
        }

        setupSenderList();
        //load initial settings
        senderCbList.forEach(cb -> cb.setSelected(SenderFilmlistLoadApprover.isApproved(cb.getText())));
        //now add the item listeners for update
        senderCbList.forEach(cb -> cb.addItemListener(this::senderSelectionItemHandler));

        jRadioButtonManuell.addChangeListener(e -> {
            final var selected = jRadioButtonManuell.isSelected();
            jTextFieldUrl.setEnabled(selected);
            jButtonDateiAuswaehlen.setEnabled(selected);
            jCheckBoxUpdate.setEnabled(selected);
        });

        // Duplicate evaluation
        var enableDuplicateEvaluation = config.getBoolean(
                ApplicationConfiguration.FILM_EVALUATE_DUPLICATES, true);
        cbEvaluateDuplicates.setSelected(enableDuplicateEvaluation);
        cbEvaluateDuplicates.addActionListener(e -> config.setProperty(ApplicationConfiguration.FILM_EVALUATE_DUPLICATES,
                        cbEvaluateDuplicates.isSelected()));
    }

    /**
     * Deactivate controls which are useless in the settings dialog.
     */
    private void prepareSettingsLayout() {
        lblUrl.setVisible(false);
        jTextFieldUrl.setVisible(false);
        jButtonDateiAuswaehlen.setVisible(false);
    }

    private void setupCheckBoxes() {
        cbSign.setSelected(config.getBoolean(ApplicationConfiguration.FilmList.LOAD_SIGN_LANGUAGE,true));
        cbSign.addActionListener(e -> config.setProperty(ApplicationConfiguration.FilmList.LOAD_SIGN_LANGUAGE,cbSign.isSelected()));

        cbAudio.setSelected(config.getBoolean(ApplicationConfiguration.FilmList.LOAD_AUDIO_DESCRIPTION,true));
        cbAudio.addActionListener(e -> config.setProperty(ApplicationConfiguration.FilmList.LOAD_AUDIO_DESCRIPTION,cbAudio.isSelected()));

        cbTrailer.setSelected(config.getBoolean(ApplicationConfiguration.FilmList.LOAD_TRAILER,true));
        cbTrailer.addActionListener(e -> config.setProperty(ApplicationConfiguration.FilmList.LOAD_TRAILER,cbTrailer.isSelected()));

        cbLivestreams.setSelected(config.getBoolean(ApplicationConfiguration.FilmList.LOAD_LIVESTREAMS, true));
        cbLivestreams.addActionListener(e -> config.setProperty(ApplicationConfiguration.FilmList.LOAD_LIVESTREAMS, cbLivestreams.isSelected()));

        jCheckBoxUpdate.setSelected(config.getBoolean(ApplicationConfiguration.FilmList.EXTEND_OLD_FILMLIST, false));
        jCheckBoxUpdate.addActionListener(e -> config.setProperty(ApplicationConfiguration.FilmList.EXTEND_OLD_FILMLIST, jCheckBoxUpdate.isSelected()));
    }

    /**
     * Simplify sender checkbox handling.
     */
    private void setupSenderList() {
        senderCbList.add(checkBox1);
        senderCbList.add(checkBox2);
        senderCbList.add(checkBox3);
        senderCbList.add(checkBox4);
        senderCbList.add(checkBox6);
        senderCbList.add(checkBox8);
        senderCbList.add(checkBox7);
        senderCbList.add(checkBox9);
        senderCbList.add(checkBox10);
        senderCbList.add(checkBox11);
        senderCbList.add(checkBox12);
        senderCbList.add(checkBox13);
        senderCbList.add(checkBox14);
        senderCbList.add(checkBox15);
        senderCbList.add(checkBox16);
        senderCbList.add(checkBox17);
        senderCbList.add(checkBox18);
        senderCbList.add(checkBox19);
        senderCbList.add(checkBox20);
        senderCbList.add(checkBox21);
        senderCbList.add(checkBox22);
        senderCbList.add(checkBox23);
        senderCbList.add(checkBox24);
        senderCbList.add(checkBox25);
        senderCbList.add(checkBox26);
        senderCbList.add(checkBox27);
        senderCbList.add(checkBox28);
        senderCbList.add(checkBox29);
        senderCbList.add(checkBox31);
        senderCbList.add(checkBox32);
    }

    private boolean warningDialogShown;

    private void senderSelectionItemHandler(ItemEvent e) {
        var cb = (JCheckBox)e.getSource();
        var selected = cb.isSelected();
        var sender = cb.getText();
        if (selected)
            SenderFilmlistLoadApprover.approve(sender);
        else
            SenderFilmlistLoadApprover.deny(sender);

        senderSelectionChanged = true;
        SwingUtilities.invokeLater(() -> {
            if (!warningDialogShown) {
                var msg = "<html>Bei Änderungen an den Sendern <b>muss</b> zwingend ein Neustart durchgeführt werden.</html>";
                JOptionPane.showMessageDialog(this, msg, Konstanten.PROGRAMMNAME, JOptionPane.WARNING_MESSAGE);
                warningDialogShown = true;
            }
        });
    }

    private boolean senderSelectionChanged;

    public boolean hasSenderSelectionChanged() {
        return senderSelectionChanged;
    }

    private void init() {
        initRadio();

        jButtonDateiAuswaehlen.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg"));
        jButtonDateiAuswaehlen.addActionListener(e -> {
            var loadFile = FileDialogs.chooseLoadFileLocation(MediathekGui.ui(),"Filmliste laden", "");
            if (loadFile != null) {
                jTextFieldUrl.setText(loadFile.getAbsolutePath());
            }
        });

        var listener = new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (jRadioButtonManuell.isSelected())
                    GuiFunktionen.setFilmListUpdateType(FilmListUpdateType.MANUAL);
                else
                    GuiFunktionen.setFilmListUpdateType(FilmListUpdateType.AUTOMATIC);

                MessageBus.getMessageBus().publishAsync(new FilmListImportTypeChangedEvent());
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
        switch (GuiFunktionen.getFilmListUpdateType()) {
            case MANUAL -> jRadioButtonManuell.setSelected(true);
            case AUTOMATIC -> jRadioButtonAuto.setSelected(true);
        }

        jTextFieldUrl.setText(MVConfig.get(MVConfig.Configs.SYSTEM_IMPORT_URL_MANUELL));
    }

    public JCheckBox getJCheckBoxUpdate() {
        return jCheckBoxUpdate;
    }

    public JTextField getJTextFieldUrl() {
        return jTextFieldUrl;
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
        var multilineLabel1 = new MultilineLabel();
        var jPanelManuel = new JPanel();
        var multilineLabel2 = new MultilineLabel();
        lblUrl = new JLabel();
        jTextFieldUrl = new JTextField();
        jButtonDateiAuswaehlen = new JButton();
        jCheckBoxUpdate = new JCheckBox();
        jRadioButtonAuto = new JRadioButton();
        jRadioButtonManuell = new JRadioButton();
        var separator1 = new JSeparator();
        var panel3 = new JPanel();
        var panel4 = new JPanel();
        cbEvaluateDuplicates = new JCheckBox();
        var panel2 = new JPanel();
        var label1 = new JLabel();
        var jSpinnerDays = new DaysSpinner();
        var label2 = new JLabel();
        btnReloadFilmlist = new JButton();
        var jPanel1 = new JPanel();
        cbSign = new JCheckBox();
        cbTrailer = new JCheckBox();
        cbAudio = new JCheckBox();
        cbLivestreams = new JCheckBox();
        panel1 = new JPanel();
        checkBox1 = new JCheckBox();
        checkBox10 = new JCheckBox();
        checkBox12 = new JCheckBox();
        checkBox16 = new JCheckBox();
        checkBox29 = new JCheckBox();
        checkBox8 = new JCheckBox();
        checkBox25 = new JCheckBox();
        checkBox13 = new JCheckBox();
        checkBox4 = new JCheckBox();
        checkBox21 = new JCheckBox();
        checkBox27 = new JCheckBox();
        checkBox26 = new JCheckBox();
        checkBox3 = new JCheckBox();
        checkBox17 = new JCheckBox();
        checkBox22 = new JCheckBox();
        checkBox9 = new JCheckBox();
        checkBox2 = new JCheckBox();
        checkBox14 = new JCheckBox();
        checkBox18 = new JCheckBox();
        checkBox31 = new JCheckBox();
        checkBox23 = new JCheckBox();
        checkBox11 = new JCheckBox();
        checkBox28 = new JCheckBox();
        checkBox19 = new JCheckBox();
        checkBox32 = new JCheckBox();
        checkBox24 = new JCheckBox();
        checkBox7 = new JCheckBox();
        checkBox15 = new JCheckBox();
        checkBox20 = new JCheckBox();
        checkBox6 = new JCheckBox();

        //======== this ========
        setPreferredSize(new Dimension(740, 506));
        setLayout(new MigLayout(
            new LC().fillX().insets("5").hideMode(3).gridGap("5", "5"), //NON-NLS
            // columns
            new AC()
                .align("label").gap() //NON-NLS
                .size("640").grow().fill(), //NON-NLS
            // rows
            new AC()
                .gap()
                .gap()
                .gap()
                .gap()
                ));

        //======== jPanelAuto ========
        {
            jPanelAuto.setBorder(new TitledBorder("Die Filmliste automatisch laden")); //NON-NLS
            jPanelAuto.setLayout(new MigLayout(
                new LC().insets("5").hideMode(3).gridGap("5", "5"), //NON-NLS
                // columns
                new AC()
                    .grow().fill().gap()
                    .grow().fill(),
                // rows
                new AC()
                    .fill()));

            //---- multilineLabel1 ----
            multilineLabel1.setText("Die Filmliste wird beim Programmstart automatisch geladen (wenn \u00e4lter als 3h). Zus\u00e4tzlich kann sie \u00fcber den Button \"Filmliste laden\" aktualisiert werden."); //NON-NLS
            multilineLabel1.setFont(multilineLabel1.getFont().deriveFont(multilineLabel1.getFont().getSize() - 1f));
            jPanelAuto.add(multilineLabel1, new CC().cell(0, 0, 2, 1));
        }
        add(jPanelAuto, new CC().cell(1, 0));

        //======== jPanelManuel ========
        {
            jPanelManuel.setBorder(new TitledBorder("Filmliste nur manuell laden")); //NON-NLS
            jPanelManuel.setMaximumSize(new Dimension(750, 2147483647));
            jPanelManuel.setLayout(new MigLayout(
                new LC().insets("5").hideMode(3).gridGap("5", "5"), //NON-NLS
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

            //---- multilineLabel2 ----
            multilineLabel2.setText("Die Filmliste wird nur manuell \u00fcber den Button \"Filmliste laden\" aus dem Internet geladen. Alternativ kann auch eine Datei zum Laden angegeben werden."); //NON-NLS
            multilineLabel2.setFont(multilineLabel2.getFont().deriveFont(multilineLabel2.getFont().getSize() - 1f));
            jPanelManuel.add(multilineLabel2, new CC().cell(0, 0, 4, 1));

            //---- lblUrl ----
            lblUrl.setText("Datei:"); //NON-NLS
            jPanelManuel.add(lblUrl, new CC().cell(0, 1));
            jPanelManuel.add(jTextFieldUrl, new CC().cell(1, 1, 2, 1));

            //---- jButtonDateiAuswaehlen ----
            jButtonDateiAuswaehlen.setToolTipText("URL oder lokale Filmliste ausw\u00e4hlen"); //NON-NLS
            jPanelManuel.add(jButtonDateiAuswaehlen, new CC().cell(3, 1).alignX("left").growX(0).width("32:32:32").height("32:32:32")); //NON-NLS

            //---- jCheckBoxUpdate ----
            jCheckBoxUpdate.setText("Alte Filmliste nicht l\u00f6schen, nur erweitern"); //NON-NLS
            jPanelManuel.add(jCheckBoxUpdate, new CC().cell(0, 2, 2, 1));
        }
        add(jPanelManuel, new CC().cell(1, 1));
        add(jRadioButtonAuto, new CC().cell(0, 0).alignY("top").growY(0)); //NON-NLS
        add(jRadioButtonManuell, new CC().cell(0, 1).alignY("top").growY(0)); //NON-NLS
        add(separator1, new CC().cell(0, 2, 2, 1).growX());

        //======== panel3 ========
        {
            panel3.setBorder(new TitledBorder("Einschr\u00e4nkungen f\u00fcr das Laden der Filmliste")); //NON-NLS
            panel3.setLayout(new VerticalLayout());

            //======== panel4 ========
            {
                panel4.setBorder(new TitledBorder("Duplikate (\u00c4nderungen erfordern Neuladen der Filmliste)")); //NON-NLS
                panel4.setLayout(new VerticalLayout());

                //---- cbEvaluateDuplicates ----
                cbEvaluateDuplicates.setText("Erkennung beim Laden der Filmliste einschalten"); //NON-NLS
                panel4.add(cbEvaluateDuplicates);
            }
            panel3.add(panel4);

            //======== panel2 ========
            {
                panel2.setLayout(new MigLayout(
                    new LC().insets("5").hideMode(3), //NON-NLS
                    // columns
                    new AC()
                        .fill().gap()
                        .fill().gap()
                        .fill().gap()
                        .align("left"), //NON-NLS
                    // rows
                    new AC()
                        .fill()));

                //---- label1 ----
                label1.setText("Nur Filme der letzten"); //NON-NLS
                panel2.add(label1, new CC().cell(0, 0).alignX("center").growX(0)); //NON-NLS
                panel2.add(jSpinnerDays, new CC().cell(1, 0));

                //---- label2 ----
                label2.setText("Tage laden."); //NON-NLS
                panel2.add(label2, new CC().cell(2, 0).alignX("center").growX(0)); //NON-NLS

                //---- btnReloadFilmlist ----
                btnReloadFilmlist.setToolTipText("Filmliste jetzt aktualisieren"); //NON-NLS
                panel2.add(btnReloadFilmlist, new CC().cell(3, 0));
            }
            panel3.add(panel2);

            //======== jPanel1 ========
            {
                jPanel1.setBorder(new TitledBorder("Zus\u00e4tzliche Filmdaten laden")); //NON-NLS
                jPanel1.setToolTipText("<html>Alle nicht angew\u00e4hlten Eintr\u00e4ge werden beim Laden der Filmliste aus dem Endergebnis herausgefiltert.<br/><b>Die Eintr\u00e4ge werden dauerhaft aus der lokalen Filmliste entfernt.</b><br/>Sie werden erst wieder beim Laden einer neuen Liste vom Server hinzugef\u00fcgt wenn die Einstellungen entsprechend angepasst wurden.</html>"); //NON-NLS
                jPanel1.setLayout(new MigLayout(
                    new LC().insets("5").hideMode(3).gridGap("5", "5"), //NON-NLS
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
                cbSign.setText("Geb\u00e4rdensprache"); //NON-NLS
                jPanel1.add(cbSign, new CC().cell(2, 0));

                //---- cbTrailer ----
                cbTrailer.setText("Trailer/Teaser/Vorschau"); //NON-NLS
                jPanel1.add(cbTrailer, new CC().cell(0, 0));

                //---- cbAudio ----
                cbAudio.setText("H\u00f6rfassungen"); //NON-NLS
                jPanel1.add(cbAudio, new CC().cell(1, 0));

                //---- cbLivestreams ----
                cbLivestreams.setText("Livestreams"); //NON-NLS
                jPanel1.add(cbLivestreams, new CC().cell(3, 0));
            }
            panel3.add(jPanel1);

            //======== panel1 ========
            {
                panel1.setBorder(new TitledBorder("Diese Sender laden (\u00c4nderungen erfordern Programmneustart und eine vollst\u00e4ndig neue Filmliste)")); //NON-NLS
                panel1.setToolTipText("<html>Die Einstellungen beziehen sich auf den n\u00e4chsten <b>vollst\u00e4ndigen</b> Ladevorgang einer Fillmliste.<br>Es kann somit vorkommen dass die Aktualisierung erst nach Neustart des Programms <br><b>und dem Laden einer kompletten Liste vom Server</b> (keine DIFF-Liste!) sichtbar wird.<br><br>Hier ge\u00e4nderte Einstellungen werden in der Senderliste des Filterdialogs <b>erst nach Neustart</b> sichtbar!</html>"); //NON-NLS
                panel1.setLayout(new MigLayout(
                    new LC().insets("5").hideMode(3).alignX("left").gridGapX("10"), //NON-NLS
                    // columns
                    new AC()
                        .fill().gap()
                        .fill().gap()
                        .fill().gap()
                        .fill().gap()
                        .fill(),
                    // rows
                    new AC()
                        .gap()
                        .gap()
                        .gap()
                        .gap()
                        .gap()
                        ));

                //---- checkBox1 ----
                checkBox1.setText("3Sat"); //NON-NLS
                panel1.add(checkBox1, new CC().cell(0, 0));

                //---- checkBox10 ----
                checkBox10.setText("ARTE.FR"); //NON-NLS
                panel1.add(checkBox10, new CC().cell(1, 0));

                //---- checkBox12 ----
                checkBox12.setText("HR"); //NON-NLS
                panel1.add(checkBox12, new CC().cell(2, 0));

                //---- checkBox16 ----
                checkBox16.setText("PHOENIX"); //NON-NLS
                panel1.add(checkBox16, new CC().cell(3, 0));

                //---- checkBox29 ----
                checkBox29.setText("tagesschau24"); //NON-NLS
                panel1.add(checkBox29, new CC().cell(4, 0));

                //---- checkBox8 ----
                checkBox8.setText("ARD"); //NON-NLS
                panel1.add(checkBox8, new CC().cell(0, 1));

                //---- checkBox25 ----
                checkBox25.setText("ARTE.IT"); //NON-NLS
                panel1.add(checkBox25, new CC().cell(1, 1));

                //---- checkBox13 ----
                checkBox13.setText("KiKA"); //NON-NLS
                panel1.add(checkBox13, new CC().cell(2, 1));

                //---- checkBox4 ----
                checkBox4.setText("Radio Bremen TV"); //NON-NLS
                panel1.add(checkBox4, new CC().cell(3, 1));

                //---- checkBox21 ----
                checkBox21.setText("WDR"); //NON-NLS
                panel1.add(checkBox21, new CC().cell(4, 1));

                //---- checkBox27 ----
                checkBox27.setText("ARD-alpha"); //NON-NLS
                panel1.add(checkBox27, new CC().cell(0, 2));

                //---- checkBox26 ----
                checkBox26.setText("ARTE.PL"); //NON-NLS
                panel1.add(checkBox26, new CC().cell(1, 2));

                //---- checkBox3 ----
                checkBox3.setText("MDR"); //NON-NLS
                panel1.add(checkBox3, new CC().cell(2, 2));

                //---- checkBox17 ----
                checkBox17.setText("RBB"); //NON-NLS
                panel1.add(checkBox17, new CC().cell(3, 2));

                //---- checkBox22 ----
                checkBox22.setText("ZDF"); //NON-NLS
                panel1.add(checkBox22, new CC().cell(4, 2));

                //---- checkBox9 ----
                checkBox9.setText("ARTE.DE"); //NON-NLS
                panel1.add(checkBox9, new CC().cell(0, 3));

                //---- checkBox2 ----
                checkBox2.setText("BR"); //NON-NLS
                panel1.add(checkBox2, new CC().cell(1, 3));

                //---- checkBox14 ----
                checkBox14.setText("NDR"); //NON-NLS
                panel1.add(checkBox14, new CC().cell(2, 3));

                //---- checkBox18 ----
                checkBox18.setText("SR"); //NON-NLS
                panel1.add(checkBox18, new CC().cell(3, 3));

                //---- checkBox31 ----
                checkBox31.setText("ZDFinfo"); //NON-NLS
                panel1.add(checkBox31, new CC().cell(4, 3));

                //---- checkBox23 ----
                checkBox23.setText("ARTE.EN"); //NON-NLS
                panel1.add(checkBox23, new CC().cell(0, 4));

                //---- checkBox11 ----
                checkBox11.setText("DW"); //NON-NLS
                panel1.add(checkBox11, new CC().cell(1, 4));

                //---- checkBox28 ----
                checkBox28.setText("ONE"); //NON-NLS
                panel1.add(checkBox28, new CC().cell(2, 4));

                //---- checkBox19 ----
                checkBox19.setText("SRF"); //NON-NLS
                panel1.add(checkBox19, new CC().cell(3, 4));

                //---- checkBox32 ----
                checkBox32.setText("ZDFneo"); //NON-NLS
                panel1.add(checkBox32, new CC().cell(4, 4));

                //---- checkBox24 ----
                checkBox24.setText("ARTE.ES"); //NON-NLS
                panel1.add(checkBox24, new CC().cell(0, 5));

                //---- checkBox7 ----
                checkBox7.setText("Funk.net"); //NON-NLS
                panel1.add(checkBox7, new CC().cell(1, 5));

                //---- checkBox15 ----
                checkBox15.setText("ORF"); //NON-NLS
                panel1.add(checkBox15, new CC().cell(2, 5));

                //---- checkBox20 ----
                checkBox20.setText("SWR"); //NON-NLS
                panel1.add(checkBox20, new CC().cell(3, 5));

                //---- checkBox6 ----
                checkBox6.setText("ZDF-tivi"); //NON-NLS
                panel1.add(checkBox6, new CC().cell(4, 5));
            }
            panel3.add(panel1);
        }
        add(panel3, new CC().cell(0, 3, 2, 1).growX());

        //---- buttonGroup1 ----
        var buttonGroup1 = new ButtonGroup();
        buttonGroup1.add(jRadioButtonAuto);
        buttonGroup1.add(jRadioButtonManuell);
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JLabel lblUrl;
    private JTextField jTextFieldUrl;
    private JButton jButtonDateiAuswaehlen;
    private JCheckBox jCheckBoxUpdate;
    private JRadioButton jRadioButtonAuto;
    private JRadioButton jRadioButtonManuell;
    private JCheckBox cbEvaluateDuplicates;
    private JButton btnReloadFilmlist;
    private JCheckBox cbSign;
    private JCheckBox cbTrailer;
    private JCheckBox cbAudio;
    private JCheckBox cbLivestreams;
    private JPanel panel1;
    private JCheckBox checkBox1;
    private JCheckBox checkBox10;
    private JCheckBox checkBox12;
    private JCheckBox checkBox16;
    private JCheckBox checkBox29;
    private JCheckBox checkBox8;
    private JCheckBox checkBox25;
    private JCheckBox checkBox13;
    private JCheckBox checkBox4;
    private JCheckBox checkBox21;
    private JCheckBox checkBox27;
    private JCheckBox checkBox26;
    private JCheckBox checkBox3;
    private JCheckBox checkBox17;
    private JCheckBox checkBox22;
    private JCheckBox checkBox9;
    private JCheckBox checkBox2;
    private JCheckBox checkBox14;
    private JCheckBox checkBox18;
    private JCheckBox checkBox31;
    private JCheckBox checkBox23;
    private JCheckBox checkBox11;
    private JCheckBox checkBox28;
    private JCheckBox checkBox19;
    private JCheckBox checkBox32;
    private JCheckBox checkBox24;
    private JCheckBox checkBox7;
    private JCheckBox checkBox15;
    private JCheckBox checkBox20;
    private JCheckBox checkBox6;
    // End of variables declaration//GEN-END:variables
}
