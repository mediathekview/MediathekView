package mediathek.gui.dialogEinstellungen;

import mSearch.tool.ApplicationConfiguration;
import mSearch.tool.Log;
import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVColor;
import mediathek.config.MVConfig;
import mediathek.gui.messages.FilmListImportTypeChangedEvent;
import mediathek.tool.FilmListUpdateType;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.TextCopyPaste;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.lang3.SystemUtils;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

@SuppressWarnings("serial")
public class PanelFilmlisteLaden extends JPanel {
    private final Daten daten;

    public PanelFilmlisteLaden(Daten d) {
        super();
        daten = d;

        daten.getMessageBus().subscribe(this);

        initComponents();
        init();

        final var config = ApplicationConfiguration.getConfiguration();
        cbSign.setSelected(config.getBoolean(ApplicationConfiguration.FILMLIST_LOAD_SIGNLANGUAGE,true));
        cbSign.addActionListener(e -> config.setProperty(ApplicationConfiguration.FILMLIST_LOAD_SIGNLANGUAGE,cbSign.isSelected()));

        cbAudio.setSelected(config.getBoolean(ApplicationConfiguration.FILMLIST_LOAD_AUDIODESCRIPTION,true));
        cbAudio.addActionListener(e -> config.setProperty(ApplicationConfiguration.FILMLIST_LOAD_AUDIODESCRIPTION,cbAudio.isSelected()));

        cbTrailer.setSelected(config.getBoolean(ApplicationConfiguration.FILMLIST_LOAD_TRAILER,true));
        cbTrailer.addActionListener(e -> config.setProperty(ApplicationConfiguration.FILMLIST_LOAD_TRAILER,cbTrailer.isSelected()));
    }

    private void init() {
        initRadio();

        final var filmeLaden = daten.getFilmeLaden();
        jButtonLoad.addActionListener(ae -> filmeLaden.loadFilmlist(""));

        jButtonDateiAuswaehlen.setIcon(Icons.ICON_BUTTON_FILE_OPEN);
        jButtonDateiAuswaehlen.addActionListener(new BeobPfad());

        jButtonFilmeLaden.addActionListener(e -> {
            if (jCheckBoxUpdate.isSelected())
                filmeLaden.updateFilmlist(jTextFieldUrl.getText());
            else
                filmeLaden.loadFilmlist(jTextFieldUrl.getText());
        });

        jRadioButtonManuell.addActionListener(new BeobOption());
        jRadioButtonAuto.addActionListener(new BeobOption());
        jTextFieldUrl.getDocument().addDocumentListener(new BeobDateiUrl());
        jTextFieldUrl.addMouseListener(new TextCopyPaste());
    }

    @Handler
    private void handleFilmListImportTypeChanged(FilmListImportTypeChangedEvent e) {
        SwingUtilities.invokeLater(this::initRadio);
    }

    private void initRadio() {
        switch (GuiFunktionen.getImportArtFilme()) {
            case MANUAL:
                jRadioButtonManuell.setSelected(true);
                break;

            case AUTOMATIC:
                jRadioButtonAuto.setSelected(true);
                break;
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

    private class BeobOption implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            if (jRadioButtonManuell.isSelected())
                GuiFunktionen.setImportArtFilme(FilmListUpdateType.MANUAL);
            else
                GuiFunktionen.setImportArtFilme(FilmListUpdateType.AUTOMATIC);

            daten.getMessageBus().publishAsync(new FilmListImportTypeChangedEvent());
        }
    }

    private class BeobPfad implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            //we can use native chooser on Mac...
            if (SystemUtils.IS_OS_MAC_OSX) {
                FileDialog chooser = new FileDialog(MediathekGui.ui(), "Filmliste laden");
                chooser.setMode(FileDialog.LOAD);
                chooser.setVisible(true);
                if (chooser.getFile() != null) {
                    try {
                        File destination = new File(chooser.getDirectory() + chooser.getFile());
                        jTextFieldUrl.setText(destination.getAbsolutePath());
                    } catch (Exception ex) {
                        Log.errorLog(102036579, ex);
                    }
                }
            } else {
                int returnVal;
                JFileChooser chooser = new JFileChooser();
                if (!jTextFieldUrl.getText().isEmpty()) {
                    chooser.setCurrentDirectory(new File(jTextFieldUrl.getText()));
                }
                chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                chooser.setFileHidingEnabled(false);
                returnVal = chooser.showOpenDialog(null);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        jTextFieldUrl.setText(chooser.getSelectedFile().getAbsolutePath());
                    } catch (Exception ex) {
                        Log.errorLog(733025319, ex);
                    }
                }
            }
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
    private void initComponents() {

        javax.swing.ButtonGroup buttonGroup1 = new javax.swing.ButtonGroup();
        javax.swing.JPanel jPanelAuto = new javax.swing.JPanel();
        javax.swing.JScrollPane jScrollPane2 = new javax.swing.JScrollPane();
        jTextAreaAuto = new javax.swing.JTextArea();
        jButtonLoad = new javax.swing.JButton();
        javax.swing.JPanel jPanelManuel = new javax.swing.JPanel();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        jTextFieldUrl = new javax.swing.JTextField();
        jButtonDateiAuswaehlen = new javax.swing.JButton();
        jButtonFilmeLaden = new javax.swing.JButton();
        javax.swing.JScrollPane jScrollPane3 = new javax.swing.JScrollPane();
        jTextAreaManuell = new javax.swing.JTextArea();
        jCheckBoxUpdate = new javax.swing.JCheckBox();
        jRadioButtonAuto = new javax.swing.JRadioButton();
        jRadioButtonManuell = new javax.swing.JRadioButton();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        cbSign = new javax.swing.JCheckBox();
        cbTrailer = new javax.swing.JCheckBox();
        cbAudio = new javax.swing.JCheckBox();

        jPanelAuto.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 255)), "Die Filmliste automatisch laden"));

        jTextAreaAuto.setEditable(false);
        jTextAreaAuto.setColumns(20);
        jTextAreaAuto.setRows(4);
        jTextAreaAuto.setText("Die Filmliste wird beim Programmstart automatisch geladen (wenn sie\nälter als 3h ist). Zusätzlich kann sie über den Button \"Neue Filmliste laden\"\naktualisiert werden. Zum Update werden dann nur noch die Differenzlisten geladen (enthalten\nnur die neuen Filme).");
        jTextAreaAuto.setMargin(new java.awt.Insets(4, 4, 4, 4));
        jScrollPane2.setViewportView(jTextAreaAuto);

        jButtonLoad.setText("Filme jetzt laden");

        javax.swing.GroupLayout jPanelAutoLayout = new javax.swing.GroupLayout(jPanelAuto);
        jPanelAuto.setLayout(jPanelAutoLayout);
        jPanelAutoLayout.setHorizontalGroup(
            jPanelAutoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelAutoLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelAutoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane2)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelAutoLayout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jButtonLoad)))
                .addContainerGap())
        );
        jPanelAutoLayout.setVerticalGroup(
            jPanelAutoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelAutoLayout.createSequentialGroup()
                .addComponent(jScrollPane2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonLoad)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanelManuel.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 255)), "Filmliste nur manuell laden"));

        jLabel1.setText("URL/Datei:");

        jButtonDateiAuswaehlen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png"))); // NOI18N
        jButtonDateiAuswaehlen.setToolTipText("URL oder lokale Filmliste auswählen");

        jButtonFilmeLaden.setText("Filme jetzt laden");

        jTextAreaManuell.setEditable(false);
        jTextAreaManuell.setColumns(20);
        jTextAreaManuell.setRows(4);
        jTextAreaManuell.setText("Die Filmliste wird nur manuell über den Button \"Neue Filmliste laden\"\ngeladen. Es wird dann dieser Dialog angezeigt und es kann eine URL/Datei zum\nLaden angegeben werden.");
        jTextAreaManuell.setMargin(new java.awt.Insets(4, 4, 4, 4));
        jScrollPane3.setViewportView(jTextAreaManuell);

        jCheckBoxUpdate.setText("alte Filmliste nicht löschen, nur erweitern");

        javax.swing.GroupLayout jPanelManuelLayout = new javax.swing.GroupLayout(jPanelManuel);
        jPanelManuel.setLayout(jPanelManuelLayout);
        jPanelManuelLayout.setHorizontalGroup(
            jPanelManuelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelManuelLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelManuelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelManuelLayout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldUrl)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonDateiAuswaehlen))
                    .addComponent(jScrollPane3)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelManuelLayout.createSequentialGroup()
                        .addComponent(jCheckBoxUpdate)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 263, Short.MAX_VALUE)
                        .addComponent(jButtonFilmeLaden)))
                .addContainerGap())
        );
        jPanelManuelLayout.setVerticalGroup(
            jPanelManuelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelManuelLayout.createSequentialGroup()
                .addComponent(jScrollPane3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanelManuelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jTextFieldUrl, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel1)
                    .addComponent(jButtonDateiAuswaehlen))
                .addGap(18, 18, 18)
                .addGroup(jPanelManuelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonFilmeLaden)
                    .addComponent(jCheckBoxUpdate))
                .addContainerGap())
        );

        jPanelManuelLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonDateiAuswaehlen, jLabel1, jTextFieldUrl});

        buttonGroup1.add(jRadioButtonAuto);

        buttonGroup1.add(jRadioButtonManuell);

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("Zusätzliche Filmdaten laden"));
        jPanel1.setToolTipText("<html>Alle nicht angewählten Einträge werden beim Laden der Filmliste aus dem Endergebnis herausgefiltert.<br/><b>Die Einträge werden dauerhaft aus der lokalen Filmliste entfernt.</b><br/>Sie werden erst wieder beim Laden einer neuen Liste vom Server hinzugefügt wenn die Einstellungen entsprechend angepasst wurden.</html>");

        cbSign.setText("Gebärdensprache");

        cbTrailer.setText("Trailer/Teaser/Vorschau");

        cbAudio.setText("Hörfassungen");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(cbTrailer)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(cbAudio)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(cbSign)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(cbSign)
                    .addComponent(cbAudio)
                    .addComponent(cbTrailer))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jRadioButtonAuto)
                    .addComponent(jRadioButtonManuell))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanelAuto, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanelManuel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jRadioButtonAuto)
                    .addComponent(jPanelAuto, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jRadioButtonManuell)
                    .addComponent(jPanelManuel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JCheckBox cbAudio;
    private javax.swing.JCheckBox cbSign;
    private javax.swing.JCheckBox cbTrailer;
    private javax.swing.JButton jButtonDateiAuswaehlen;
    private javax.swing.JButton jButtonFilmeLaden;
    private javax.swing.JButton jButtonLoad;
    private javax.swing.JCheckBox jCheckBoxUpdate;
    private javax.swing.JRadioButton jRadioButtonAuto;
    private javax.swing.JRadioButton jRadioButtonManuell;
    private javax.swing.JTextArea jTextAreaAuto;
    private javax.swing.JTextArea jTextAreaManuell;
    private javax.swing.JTextField jTextFieldUrl;
    // End of variables declaration//GEN-END:variables
}
