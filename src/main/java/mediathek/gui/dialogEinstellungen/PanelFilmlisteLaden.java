package mediathek.gui.dialogEinstellungen;

import ca.odell.glazedlists.swing.GlazedListsSwing;
import com.jidesoft.swing.CheckBoxList;
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

public class PanelFilmlisteLaden extends JPanel {
    private final Configuration config = ApplicationConfiguration.getConfiguration();
    private boolean warningDialogShown;
    private boolean senderSelectionChanged;

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

        jRadioButtonManuell.addChangeListener(_ -> {
            final var selected = jRadioButtonManuell.isSelected();
            jTextFieldUrl.setEnabled(selected);
            jButtonDateiAuswaehlen.setEnabled(selected);
            jCheckBoxUpdate.setEnabled(selected);
        });

        // Duplicate evaluation
        var enableDuplicateEvaluation = config.getBoolean(
                ApplicationConfiguration.FILM_EVALUATE_DUPLICATES, true);
        cbEvaluateDuplicates.setSelected(enableDuplicateEvaluation);
        cbEvaluateDuplicates.addActionListener(_ -> config.setProperty(ApplicationConfiguration.FILM_EVALUATE_DUPLICATES,
                cbEvaluateDuplicates.isSelected()));
    }

    private void initReloadButton() {
        btnReloadFilmlist.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/arrows-rotate.svg"));
        btnReloadFilmlist.addActionListener(_ -> {
            final var daten = Daten.getInstance();
            daten.getListeFilme().clear(); // sonst wird evtl. nur eine Diff geladen
            daten.getFilmeLaden().loadFilmlist("", hasSenderSelectionChanged());
        });
    }

    private void setupSenderList() {
        var model = GlazedListsSwing.eventComboBoxModelWithThreadProxyList(SenderListBoxModel.getProvidedSenderList());
        senderCheckBoxList.setModel(model);

        final var cblsm = senderCheckBoxList.getCheckBoxListSelectionModel();
        //load initial settings
        for (int i = 0; i < model.getSize(); i++) {
            var item = model.getElementAt(i);
            if (SenderFilmlistLoadApprover.isApproved(item)) {
                cblsm.addSelectionInterval(i, i);
            }
            else {
                cblsm.removeSelectionInterval(i, i);
            }
        }

        //now add the item listeners for update
        cblsm.addListSelectionListener(e -> {
            if (!e.getValueIsAdjusting()) {
                for (int i = 0; i < model.getSize(); i++) {
                    var item = model.getElementAt(i);
                    if (cblsm.isSelectedIndex(i)) {
                        SenderFilmlistLoadApprover.approve(item);
                    }
                    else {
                        SenderFilmlistLoadApprover.deny(item);
                    }
                }

                senderSelectionChanged = true;
                SwingUtilities.invokeLater(() -> {
                    if (!warningDialogShown) {
                        var msg = "<html>Bei Änderungen an den Sendern <b>muss</b> zwingend ein Neustart durchgeführt werden.</html>";
                        JOptionPane.showMessageDialog(this, msg, Konstanten.PROGRAMMNAME, JOptionPane.WARNING_MESSAGE);
                        warningDialogShown = true;
                    }
                });
            }
        });
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
        cbSign.setSelected(config.getBoolean(ApplicationConfiguration.FilmList.LOAD_SIGN_LANGUAGE, true));
        cbSign.addActionListener(_ -> config.setProperty(ApplicationConfiguration.FilmList.LOAD_SIGN_LANGUAGE, cbSign.isSelected()));

        cbAudio.setSelected(config.getBoolean(ApplicationConfiguration.FilmList.LOAD_AUDIO_DESCRIPTION, true));
        cbAudio.addActionListener(_ -> config.setProperty(ApplicationConfiguration.FilmList.LOAD_AUDIO_DESCRIPTION, cbAudio.isSelected()));

        cbTrailer.setSelected(config.getBoolean(ApplicationConfiguration.FilmList.LOAD_TRAILER, true));
        cbTrailer.addActionListener(_ -> config.setProperty(ApplicationConfiguration.FilmList.LOAD_TRAILER, cbTrailer.isSelected()));

        cbLivestreams.setSelected(config.getBoolean(ApplicationConfiguration.FilmList.LOAD_LIVESTREAMS, true));
        cbLivestreams.addActionListener(_ -> config.setProperty(ApplicationConfiguration.FilmList.LOAD_LIVESTREAMS, cbLivestreams.isSelected()));

        jCheckBoxUpdate.setSelected(config.getBoolean(ApplicationConfiguration.FilmList.EXTEND_OLD_FILMLIST, false));
        jCheckBoxUpdate.addActionListener(_ -> config.setProperty(ApplicationConfiguration.FilmList.EXTEND_OLD_FILMLIST, jCheckBoxUpdate.isSelected()));
    }

    public boolean hasSenderSelectionChanged() {
        return senderSelectionChanged;
    }

    private void init() {
        initRadio();

        jButtonDateiAuswaehlen.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg"));
        jButtonDateiAuswaehlen.addActionListener(_ -> {
            var loadFile = FileDialogs.chooseLoadFileLocation(MediathekGui.ui(), "Filmliste laden", "");
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
        var scrollPane1 = new JScrollPane();
        senderCheckBoxList = new CheckBoxList();

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
                        .grow().fill(),
                    // rows
                    new AC()
                        ));

                //======== scrollPane1 ========
                {

                    //---- senderCheckBoxList ----
                    senderCheckBoxList.setMaximumSize(null);
                    senderCheckBoxList.setMinimumSize(new Dimension(100, 50));
                    senderCheckBoxList.setPreferredSize(null);
                    senderCheckBoxList.setLayoutOrientation(JList.VERTICAL_WRAP);
                    senderCheckBoxList.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
                    scrollPane1.setViewportView(senderCheckBoxList);
                }
                panel1.add(scrollPane1, new CC().cell(0, 0));
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
    private CheckBoxList senderCheckBoxList;
    // End of variables declaration//GEN-END:variables
}
