package mediathek.gui.dialog;

import mediathek.config.*;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;
import mediathek.daten.DatenPset;
import mediathek.daten.FilmResolution;
import mediathek.gui.messages.DownloadListChangedEvent;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.*;
import org.apache.commons.configuration2.Configuration;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.nio.file.FileStore;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.Executors;

public class DialogAddDownload extends JDialog {
    private DatenPset pSet;
    private DatenDownload datenDownload;
    private final DatenFilm datenFilm;
    private String orgPfad = "";
    private final Optional<FilmResolution.Enum> requestedResolution;
    private String dateiGroesse_HD = "";
    private String dateiGroesse_Hoch = "";
    private String dateiGroesse_Klein = "";
    private boolean nameGeaendert;
    private boolean stopBeob;
    private final JTextComponent cbPathTextComponent;
    private static final Logger logger = LogManager.getLogger();

    public DialogAddDownload(@NotNull Frame parent, @NotNull DatenFilm film, @Nullable DatenPset pSet, @NotNull Optional<FilmResolution.Enum> requestedResolution) {
        super(parent, true);
        initComponents();

        cbPathTextComponent = ((JTextComponent) jComboBoxPfad.getEditor().getEditorComponent());

        this.requestedResolution = requestedResolution;
        datenFilm = film;
        this.pSet = pSet;

        init();
        packIt();

        setLocationRelativeTo(parent);
    }

    private void packIt() {
        int w = this.getWidth();
        pack();
        this.setSize(w, this.getHeight());
    }

    private void init() {
        // launch async tasks first
        var pool = Executors.newWorkStealingPool();
        var hdFuture = pool.submit(() -> {
            var url = datenFilm.getUrlFuerAufloesung(FilmResolution.Enum.HIGH_QUALITY);
            return datenFilm.getDateigroesse(url);
        });
        var hochFuture = pool.submit(() -> {
            var url = datenFilm.getUrl();
            return datenFilm.getDateigroesse(url);
        });
        var kleinFuture = pool.submit(() -> {
            var url = datenFilm.getUrlFuerAufloesung(FilmResolution.Enum.LOW);
            return datenFilm.getDateigroesse(url);
        });

        jButtonDelHistory.setIcon(Icons.ICON_BUTTON_DEL);
        jComboBoxPset.setModel(new DefaultComboBoxModel<>(Daten.listePset.getListeSpeichern().getObjectDataCombo()));

        jCheckBoxStarten.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN)));
        jCheckBoxStarten.addActionListener(e -> MVConfig.add(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN, String.valueOf(jCheckBoxStarten.isSelected())));
        jButtonZiel.setIcon(Icons.ICON_BUTTON_FILE_OPEN);
        jButtonZiel.setText("");
        if (Daten.listePset.getListeSpeichern().isEmpty()) {
            // Satz mit x, war wohl nix
            beenden(false);
        }

        jButtonZiel.addActionListener(l -> {
            var initialDirectory = "";
            if (!Objects.requireNonNull(jComboBoxPfad.getSelectedItem()).toString().isEmpty()) {
                initialDirectory = jComboBoxPfad.getSelectedItem().toString();
            }
            var directory = FileDialogs.chooseDirectoryLocation(MediathekGui.ui(),"Film speichern",initialDirectory);
            if (directory != null) {
                var selectedDirectory = directory.getAbsolutePath();
                jComboBoxPfad.addItem(selectedDirectory);
                jComboBoxPfad.setSelectedItem(selectedDirectory);

            }
        });

        jButtonOk.addActionListener(e -> {
            if (check()) {
                beenden(true);
            }
        });
        getRootPane().setDefaultButton(jButtonOk);

        EscapeKeyHandler.installHandler(this, () -> beenden(false));

        jButtonAbbrechen.addActionListener(e -> beenden(false));

        if (pSet != null) {
            jComboBoxPset.setSelectedItem(pSet.arr[DatenPset.PROGRAMMSET_NAME]);
        } else {
            pSet = Daten.listePset.getListeSpeichern().get(jComboBoxPset.getSelectedIndex());
        }
        if (Daten.listePset.getListeSpeichern().size() == 1) {
            // macht dann keinen Sinn
            jLabelSet.setVisible(false);
            jComboBoxPset.setVisible(false);
            jComboBoxPset.setEnabled(false);
        } else {
            jComboBoxPset.addActionListener(e -> setupResolutionButtons());
        }
        jTextFieldSender.setText(' ' + datenFilm.getSender() + ":   " + datenFilm.getTitle());
        jTextFieldName.getDocument().addDocumentListener(new DocumentListener() {

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
                if (!stopBeob) {
                    nameGeaendert = true;
                    if (!jTextFieldName.getText().equals(FilenameUtils.checkDateiname(jTextFieldName.getText(), false /*pfad*/))) {
                        jTextFieldName.setBackground(MVColor.DOWNLOAD_FEHLER.color);
                    } else {
                        jTextFieldName.setBackground(javax.swing.UIManager.getDefaults().getColor("TextField.background"));
                    }
                }

            }
        });
        cbPathTextComponent.setOpaque(true);
        cbPathTextComponent.getDocument().addDocumentListener(new DocumentListener() {

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
                if (!stopBeob) {
                    nameGeaendert = true;
                    //perform checks only when OS is not windows
                    if (!SystemUtils.IS_OS_WINDOWS) {
                        String s = cbPathTextComponent.getText();
                        if (!s.equals(FilenameUtils.checkDateiname(s, true))) {
                            jComboBoxPfad.getEditor().getEditorComponent().setBackground(MVColor.DOWNLOAD_FEHLER.color);
                        } else {
                            jComboBoxPfad.getEditor().getEditorComponent().setBackground(Color.WHITE);
                        }
                    }
                    calculateAndCheckDiskSpace();
                }

            }
        });

        var listener = new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setNameFilm();
            }
        };
        jRadioButtonAufloesungHd.addActionListener(listener);
        jRadioButtonAufloesungHd.setEnabled(!datenFilm.getUrlHighQuality().isEmpty());

        jRadioButtonAufloesungKlein.addActionListener(listener);
        jRadioButtonAufloesungKlein.setEnabled(!datenFilm.getUrlKlein().isEmpty());

        jRadioButtonAufloesungHoch.addActionListener(listener);
        jRadioButtonAufloesungHoch.setSelected(true);

        try {
            dateiGroesse_Hoch = hochFuture.get();
        } catch (Exception e) {
            dateiGroesse_Hoch = "";
            logger.error("Failed to retrieve Hoch resolution", e);
        }
        if (!dateiGroesse_Hoch.isEmpty()) {
            jRadioButtonAufloesungHoch.setText(jRadioButtonAufloesungHoch.getText() + "   [ " + dateiGroesse_Hoch + " MB ]");
        }

        if (jRadioButtonAufloesungHd.isEnabled()) {
            try {
                dateiGroesse_HD = hdFuture.get();
            } catch (Exception e) {
                dateiGroesse_HD = "";
                logger.error("Failed to retrieve HD resolution", e);
            }
            if (!dateiGroesse_HD.isEmpty()) {
                jRadioButtonAufloesungHd.setText(jRadioButtonAufloesungHd.getText() + "   [ " + dateiGroesse_HD + " MB ]");
            }
        }

        if (jRadioButtonAufloesungKlein.isEnabled()) {
            try {
                dateiGroesse_Klein = kleinFuture.get();
            } catch (Exception e) {
                dateiGroesse_Klein = "";
                logger.error("Failed to retrieve Klein resolution", e);
            }
            if (!dateiGroesse_Klein.isEmpty()) {
                jRadioButtonAufloesungKlein.setText(jRadioButtonAufloesungKlein.getText() + "   [ " + dateiGroesse_Klein + " MB ]");
            }
        }

        //not needed anymore
        pool.shutdown();

        jButtonDelHistory.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN, "");
            jComboBoxPfad.setModel(new DefaultComboBoxModel<>(new String[]{orgPfad}));
        });

        final Configuration config = ApplicationConfiguration.getConfiguration();
        jCheckBoxPfadSpeichern.setSelected(config.getBoolean(ApplicationConfiguration.DOWNLOAD_SHOW_LAST_USED_PATH, true));
        jCheckBoxPfadSpeichern.addActionListener(e -> config.setProperty(ApplicationConfiguration.DOWNLOAD_SHOW_LAST_USED_PATH, jCheckBoxPfadSpeichern.isSelected()));

        setupResolutionButtons();
        calculateAndCheckDiskSpace();
        nameGeaendert = false;
    }

    private void setNameFilm() {
        // beim ersten mal werden die Standardpfade gesucht
        if (!nameGeaendert) {
            // nur wenn vom Benutzer noch nicht geänert!
            stopBeob = true;
            datenDownload = new DatenDownload(pSet, datenFilm, DatenDownload.QUELLE_DOWNLOAD, null, "", "", getFilmResolution().toString());
            if (datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME].isEmpty()) {
                // dann wird nicht gespeichert ==> eigenntlich falsche Seteinstellungen??
                jTextFieldName.setEnabled(false);
                jComboBoxPfad.setEnabled(false);
                jButtonZiel.setEnabled(false);
                jTextFieldName.setText("");
                jComboBoxPfad.setModel(new DefaultComboBoxModel<>(new String[]{""}));
            } else {
                jTextFieldName.setEnabled(true);
                jComboBoxPfad.setEnabled(true);
                jButtonZiel.setEnabled(true);
                jTextFieldName.setText(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME]);
                setModelPfad(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD], jComboBoxPfad);
                orgPfad = datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD];
            }
            stopBeob = false;
        }
    }

    /**
     * Get the free disk space for a selected path.
     *
     * @return Free disk space in bytes.
     */
    private long getFreeDiskSpace(final String strPath) {
        long usableSpace = 0;
        if (!strPath.isEmpty()) {
            try {
                Path path = Paths.get(strPath);
                if (Files.notExists(path)) {
                    //getParent() may return null...therefore we need to bail out this loop at some point.
                    while (Files.notExists(path) && (path != null)) {
                        path = path.getParent();
                    }
                }

                if (path == null) {
                    //there is no way to determine usable space...
                    usableSpace = 0;
                } else {
                    final FileStore fileStore = Files.getFileStore(path);
                    usableSpace = fileStore.getUsableSpace();
                }
            } catch (Exception ex) {
                logger.error("getFreeDiskSpace Failed",ex);
            }
        }
        return usableSpace;
    }

    private static final String TITLED_BORDER_STRING = "Download-Qualität";

    /**
     * Calculate free disk space on volume and check if the movies can be safely downloaded.
     */
    private void calculateAndCheckDiskSpace() {
        jRadioButtonAufloesungHd.setForeground(Color.black);
        jRadioButtonAufloesungHoch.setForeground(Color.black);
        jRadioButtonAufloesungKlein.setForeground(Color.black);

        try {
            var filmBorder = (TitledBorder)jPanelSize.getBorder();
            long usableSpace = getFreeDiskSpace(cbPathTextComponent.getText());
            if (usableSpace > 0) {
                filmBorder.setTitle(TITLED_BORDER_STRING + " [ Freier Speicherplatz: " + FileUtils.byteCountToDisplaySize(usableSpace) + " ]");
            } else {
                filmBorder.setTitle(TITLED_BORDER_STRING);
            }
            //border needs to be repainted after update...
            jPanelSize.repaint();

            // jetzt noch prüfen, obs auf die Platte passt
            usableSpace /= FileSize.ONE_MiB;
            if (usableSpace > 0) {
                int size;
                if (!dateiGroesse_HD.isEmpty()) {
                    size = Integer.parseInt(dateiGroesse_HD);
                    if (size > usableSpace) {
                        jRadioButtonAufloesungHd.setForeground(Color.red);
                    }
                }
                if (!dateiGroesse_Hoch.isEmpty()) {
                    size = Integer.parseInt(dateiGroesse_Hoch);
                    if (size > usableSpace) {
                        jRadioButtonAufloesungHoch.setForeground(Color.red);
                    }
                }
                if (!dateiGroesse_Klein.isEmpty()) {
                    size = Integer.parseInt(dateiGroesse_Klein);
                    if (size > usableSpace) {
                        jRadioButtonAufloesungKlein.setForeground(Color.red);
                    }
                }
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    public static void setModelPfad(String pfad, JComboBox<String> jcb) {
        ArrayList<String> pfade = new ArrayList<>();
        final boolean showLastUsedPath = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.DOWNLOAD_SHOW_LAST_USED_PATH, true);

        // wenn gewünscht, den letzten verwendeten Pfad an den Anfang setzen
        if (!showLastUsedPath && !pfad.isEmpty()) {
            // aktueller Pfad an Platz 1
            pfade.add(pfad);

        }
        if (!MVConfig.get(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN).isEmpty()) {
            String[] p = MVConfig.get(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN).split("<>");
            for (String s : p) {
                if (!pfade.contains(s)) {
                    pfade.add(s);
                }
            }
        }
        if (showLastUsedPath && !pfad.isEmpty()) {
            // aktueller Pfad zum Schluss
            if (!pfade.contains(pfad)) {
                pfade.add(pfad);
            }
        }
        jcb.setModel(new DefaultComboBoxModel<>(pfade.toArray(new String[0])));
    }

    public static void saveComboPfad(JComboBox<String> jcb, String orgPath) {
        ArrayList<String> pfade = new ArrayList<>();
        String s = Objects.requireNonNull(jcb.getSelectedItem()).toString();

        if (!s.equals(orgPath) || ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.DOWNLOAD_SHOW_LAST_USED_PATH, true)) {
            pfade.add(s);
        }
        for (int i = 0; i < jcb.getItemCount(); ++i) {
            s = jcb.getItemAt(i);
            if (!s.equals(orgPath) && !pfade.contains(s)) {
                pfade.add(s);
            }
        }
        if (!pfade.isEmpty()) {
            s = pfade.get(0);
            for (int i = 1; i < Konstanten.MAX_PFADE_DIALOG_DOWNLOAD && i < pfade.size(); ++i) {
                if (!pfade.get(i).isEmpty()) {
                    s += "<>" + pfade.get(i);
                }
            }
        }
        MVConfig.add(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN, s);
    }

    private boolean isHighQualityRequested() {
        return pSet.arr[DatenPset.PROGRAMMSET_AUFLOESUNG].equals(FilmResolution.Enum.HIGH_QUALITY.toString())
                && !datenFilm.getUrlHighQuality().isEmpty();
    }

    private boolean isLowQualityRequested() {
        return pSet.arr[DatenPset.PROGRAMMSET_AUFLOESUNG].equals(FilmResolution.Enum.LOW.toString()) &&
                !datenFilm.getUrlKlein().isEmpty();
    }

    private boolean highQualityMandated;
    /**
     * Setup the resolution radio buttons based on available download URLs.
     */
    private void setupResolutionButtons() {
        pSet = Daten.listePset.getListeSpeichern().get(jComboBoxPset.getSelectedIndex());
        requestedResolution.ifPresent(it -> highQualityMandated = it == FilmResolution.Enum.HIGH_QUALITY);
        if (highQualityMandated || isHighQualityRequested()) {
            jRadioButtonAufloesungHd.setSelected(true);
        } else if (isLowQualityRequested()) {
            jRadioButtonAufloesungKlein.setSelected(true);
        } else {
            jRadioButtonAufloesungHoch.setSelected(true);
        }

        jCheckBoxInfodatei.setSelected(Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_INFODATEI]));

        if (datenFilm.getUrlSubtitle().isEmpty()) {
            // dann gibts keinen Subtitle
            jCheckBoxSubtitle.setEnabled(false);
        } else {
            jCheckBoxSubtitle.setSelected(Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_SUBTITLE]));
        }
        setNameFilm();
    }

    /**
     * Return the resolution string based on selected {@link javax.swing.JRadioButton}.
     *
     * @return The resolution as a string.
     */
    private FilmResolution.Enum getFilmResolution() {
        if (jRadioButtonAufloesungHd.isSelected()) {
            return FilmResolution.Enum.HIGH_QUALITY;
        } else if (jRadioButtonAufloesungKlein.isSelected()) {
            return FilmResolution.Enum.LOW;
        } else {
            return FilmResolution.Enum.NORMAL;
        }
    }

    private String getFilmSize() {
        if (jRadioButtonAufloesungHd.isSelected()) {
            return dateiGroesse_HD;
        } else if (jRadioButtonAufloesungKlein.isSelected()) {
            return dateiGroesse_Klein;
        } else {
            return dateiGroesse_Hoch;
        }
    }

    private boolean check() {
        var ok = false;
        String pfad = Objects.requireNonNull(jComboBoxPfad.getSelectedItem()).toString();
        String name = jTextFieldName.getText();
        if (datenDownload != null) {
            if (pfad.isEmpty() || name.isEmpty()) {
                MVMessageDialog.showMessageDialog(this, "Pfad oder Name ist leer", "Fehlerhafter Pfad/Name!", JOptionPane.ERROR_MESSAGE);
            } else {
                if (!pfad.substring(pfad.length() - 1).equals(File.separator)) {
                    pfad += File.separator;
                }
                if (GuiFunktionenProgramme.checkPathWriteable(pfad)) {
                    ok = true;
                } else {
                    MVMessageDialog.showMessageDialog(this, "Pfad ist nicht beschreibbar", "Fehlerhafter Pfad!", JOptionPane.ERROR_MESSAGE);
                }
            }
        }
        return ok;
    }

    private void beenden(boolean ok) {
        if (ok) {
            // jetzt wird mit den angegebenen Pfaden gearbeitet
            datenDownload = new DatenDownload(pSet, datenFilm, DatenDownload.QUELLE_DOWNLOAD, null, jTextFieldName.getText(), Objects.requireNonNull(jComboBoxPfad.getSelectedItem()).toString(), getFilmResolution().toString());
            datenDownload.setGroesse(getFilmSize());
            datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI] = Boolean.toString(jCheckBoxInfodatei.isSelected());
            datenDownload.arr[DatenDownload.DOWNLOAD_SUBTITLE] = Boolean.toString(jCheckBoxSubtitle.isSelected());

            final var daten = Daten.getInstance();
            daten.getListeDownloads().addMitNummer(datenDownload);
            daten.getMessageBus().publishAsync(new DownloadListChangedEvent());
            if (jCheckBoxStarten.isSelected()) {
                // und evtl. auch gleich starten
                datenDownload.startDownload();
            }
        }
        saveComboPfad(jComboBoxPfad, orgPfad);
        this.dispose();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.ButtonGroup buttonGroup1 = new javax.swing.ButtonGroup();
        jButtonOk = new javax.swing.JButton();
        jButtonAbbrechen = new javax.swing.JButton();
        jCheckBoxStarten = new javax.swing.JCheckBox();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        jTextFieldName = new javax.swing.JTextField();
        jButtonZiel = new javax.swing.JButton();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        javax.swing.JLabel jLabel4 = new javax.swing.JLabel();
        jComboBoxPfad = new javax.swing.JComboBox<>();
        jButtonDelHistory = new javax.swing.JButton();
        jCheckBoxPfadSpeichern = new javax.swing.JCheckBox();
        jCheckBoxInfodatei = new javax.swing.JCheckBox();
        jLabelSet = new javax.swing.JLabel();
        jComboBoxPset = new javax.swing.JComboBox<>();
        jCheckBoxSubtitle = new javax.swing.JCheckBox();
        jPanelSize = new javax.swing.JPanel();
        jRadioButtonAufloesungHd = new javax.swing.JRadioButton();
        jRadioButtonAufloesungHoch = new javax.swing.JRadioButton();
        jRadioButtonAufloesungKlein = new javax.swing.JRadioButton();
        jTextFieldSender = new javax.swing.JTextField();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Film speichern");
        setMinimumSize(new java.awt.Dimension(646, 372));

        jButtonOk.setText("Ok");

        jButtonAbbrechen.setText("Abbrechen");

        jCheckBoxStarten.setSelected(true);
        jCheckBoxStarten.setText("Download sofort starten");

        jButtonZiel.setText("File");
        jButtonZiel.setToolTipText("Zielpfad auswählen");

        jLabel1.setText("Zielpfad:");

        jLabel4.setText("Dateiname:");

        jComboBoxPfad.setEditable(true);
        jComboBoxPfad.setMaximumRowCount(15);

        jButtonDelHistory.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-del.png"))); // NOI18N
        jButtonDelHistory.setToolTipText("History löschen");

        jCheckBoxPfadSpeichern.setText("Zielpfad speichern");

        jCheckBoxInfodatei.setText("Infodatei anlegen: \"Filmname.txt\"");

        jLabelSet.setText("Set:");

        jCheckBoxSubtitle.setText("Untertitel speichern: \"Filmname.xxx\"");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabelSet)
                            .addComponent(jLabel1))
                        .addGap(30, 30, 30)
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addComponent(jComboBoxPfad, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonZiel)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonDelHistory))
                            .addComponent(jComboBoxPset, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jLabel4)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addComponent(jCheckBoxSubtitle)
                                .addGap(0, 0, Short.MAX_VALUE))
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addComponent(jCheckBoxInfodatei)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 155, Short.MAX_VALUE)
                                .addComponent(jCheckBoxPfadSpeichern))
                            .addComponent(jTextFieldName))))
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabelSet)
                    .addComponent(jComboBoxPset, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel1)
                    .addComponent(jButtonZiel)
                    .addComponent(jComboBoxPfad, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonDelHistory))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel4)
                    .addComponent(jTextFieldName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jCheckBoxPfadSpeichern)
                    .addComponent(jCheckBoxInfodatei))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jCheckBoxSubtitle)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel1Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonZiel, jTextFieldName});

        jPanelSize.setBorder(javax.swing.BorderFactory.createTitledBorder("Download-Qualität"));

        buttonGroup1.add(jRadioButtonAufloesungHd);
        jRadioButtonAufloesungHd.setText("Höchste/Hoch");

        buttonGroup1.add(jRadioButtonAufloesungHoch);
        jRadioButtonAufloesungHoch.setText("Mittel");

        buttonGroup1.add(jRadioButtonAufloesungKlein);
        jRadioButtonAufloesungKlein.setText("Niedrig");

        javax.swing.GroupLayout jPanelSizeLayout = new javax.swing.GroupLayout(jPanelSize);
        jPanelSize.setLayout(jPanelSizeLayout);
        jPanelSizeLayout.setHorizontalGroup(
            jPanelSizeLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelSizeLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jRadioButtonAufloesungHd)
                .addGap(18, 18, 18)
                .addComponent(jRadioButtonAufloesungHoch)
                .addGap(18, 18, 18)
                .addComponent(jRadioButtonAufloesungKlein)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanelSizeLayout.setVerticalGroup(
            jPanelSizeLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelSizeLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelSizeLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jRadioButtonAufloesungHd)
                    .addComponent(jRadioButtonAufloesungHoch)
                    .addComponent(jRadioButtonAufloesungKlein))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jTextFieldSender.setEditable(false);
        jTextFieldSender.setFont(new java.awt.Font("Dialog", 1, 12)); // NOI18N
        jTextFieldSender.setText(" ARD: Tatort, ...");
        jTextFieldSender.setBorder(javax.swing.BorderFactory.createTitledBorder("Film"));

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanelSize, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jCheckBoxStarten)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jButtonOk, javax.swing.GroupLayout.PREFERRED_SIZE, 93, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonAbbrechen))
                    .addComponent(jTextFieldSender))
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonAbbrechen, jButtonOk});

        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTextFieldSender, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanelSize, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jButtonOk)
                        .addComponent(jButtonAbbrechen))
                    .addComponent(jCheckBoxStarten))
                .addContainerGap())
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAbbrechen;
    private javax.swing.JButton jButtonDelHistory;
    private javax.swing.JButton jButtonOk;
    private javax.swing.JButton jButtonZiel;
    private javax.swing.JCheckBox jCheckBoxInfodatei;
    private javax.swing.JCheckBox jCheckBoxPfadSpeichern;
    private javax.swing.JCheckBox jCheckBoxStarten;
    private javax.swing.JCheckBox jCheckBoxSubtitle;
    private javax.swing.JComboBox<String> jComboBoxPfad;
    private javax.swing.JComboBox<String> jComboBoxPset;
    private javax.swing.JLabel jLabelSet;
    private javax.swing.JPanel jPanelSize;
    private javax.swing.JRadioButton jRadioButtonAufloesungHd;
    private javax.swing.JRadioButton jRadioButtonAufloesungHoch;
    private javax.swing.JRadioButton jRadioButtonAufloesungKlein;
    private javax.swing.JTextField jTextFieldName;
    private javax.swing.JTextField jTextFieldSender;
    // End of variables declaration//GEN-END:variables
}
