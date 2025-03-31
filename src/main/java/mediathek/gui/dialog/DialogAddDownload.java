package mediathek.gui.dialog;

import com.github.kokorin.jaffree.StreamType;
import com.github.kokorin.jaffree.ffprobe.FFprobe;
import com.github.kokorin.jaffree.ffprobe.FFprobeResult;
import com.github.kokorin.jaffree.ffprobe.Stream;
import com.github.kokorin.jaffree.process.JaffreeAbnormalExitException;
import com.google.common.util.concurrent.FutureCallback;
import com.google.common.util.concurrent.Futures;
import com.google.common.util.concurrent.JdkFutureAdapters;
import com.google.common.util.concurrent.ListenableFuture;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVColor;
import mediathek.config.MVConfig;
import mediathek.daten.*;
import mediathek.gui.messages.DownloadListChangedEvent;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.*;
import org.apache.commons.configuration2.Configuration;
import org.apache.commons.configuration2.sync.LockMode;
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
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.io.File;
import java.nio.file.FileStore;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

public class DialogAddDownload extends JDialog {
    private static final Logger logger = LogManager.getLogger();
    private static final String NO_DATA_AVAILABLE = "Keine Daten verfügbar.";
    private static final String KEY_LABEL_FOREGROUND = "Label.foreground";
    private static final String KEY_TEXTFIELD_BACKGROUND = "TextField.background";
    private static final String TITLED_BORDER_STRING = "Download-Qualität";
    private final DatenFilm film;
    private final Optional<FilmResolution.Enum> requestedResolution;
    private final ListePset listeSpeichern = Daten.listePset.getListeSpeichern();
    /**
     * The currently selected pSet or null when no selection.
     */
    private DatenPset active_pSet;
    private DatenDownload datenDownload;
    private String orgPfad = "";
    private String dateiGroesse_HQ = "";
    private String dateiGroesse_Hoch = "";
    private String dateiGroesse_Klein = "";
    private boolean nameGeaendert;
    private boolean stopBeob;
    private JTextComponent cbPathTextComponent;
    private Path ffprobePath;
    private ListenableFuture<FFprobeResult> resultListenableFuture;
    private ListenableFuture<String> hqFuture;
    private ListenableFuture<String> hochFuture;
    private ListenableFuture<String> kleinFuture;
    private boolean restoreFetchSize;
    private boolean highQualityMandated;

    public DialogAddDownload(@NotNull Frame parent, @NotNull DatenFilm film, @Nullable DatenPset pSet, @NotNull Optional<FilmResolution.Enum> requestedResolution) {
        super(parent, true);
        initComponents();

        getRootPane().setDefaultButton(jButtonOk);
        EscapeKeyHandler.installHandler(this, this::dispose);

        this.requestedResolution = requestedResolution;
        this.film = film;
        this.active_pSet = pSet;

        setupUI();
        if (SystemUtils.IS_OS_MAC_OSX) {
            pack();
        }
        restoreWindowSizeFromConfig();
        setLocationRelativeTo(parent);

        addComponentListener(new DialogPositionComponentListener());
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
            s = pfade.getFirst();
            for (int i = 1; i < Math.min(Konstanten.MAX_PFADE_DIALOG_DOWNLOAD, pfade.size()); ++i) {
                final var pfad = pfade.get(i);
                if (!pfad.isEmpty()) {
                    s += "<>" + pfad;
                }
            }
        }
        MVConfig.add(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN, s);
    }

    private void restoreWindowSizeFromConfig() {
        var config = ApplicationConfiguration.getConfiguration();
        try {
            config.lock(LockMode.READ);
            final int width = config.getInt(ApplicationConfiguration.AddDownloadDialog.WIDTH);
            final int height = config.getInt(ApplicationConfiguration.AddDownloadDialog.HEIGHT);
            final int x = config.getInt(ApplicationConfiguration.AddDownloadDialog.X);
            final int y = config.getInt(ApplicationConfiguration.AddDownloadDialog.Y);

            setBounds(x, y, width, height);
        } catch (NoSuchElementException ignored) {
            //do not restore anything
        } finally {
            config.unlock(LockMode.READ);
        }

    }

    private void setupFilmQualityRadioButtons() {
        var listener = new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setNameFilm();
                lblStatus.setText("");
                lblAudioInfo.setText("");
                lblBusyIndicator.setBusy(false);
                lblBusyIndicator.setVisible(false);
                if (resultListenableFuture != null) {
                    resultListenableFuture.cancel(true);
                    resultListenableFuture = null;
                }
            }
        };
        jRadioButtonAufloesungHd.addActionListener(listener);
        jRadioButtonAufloesungHd.setEnabled(!film.getHighQualityUrl().isEmpty());

        jRadioButtonAufloesungKlein.addActionListener(listener);
        jRadioButtonAufloesungKlein.setEnabled(!film.getLowQualityUrl().isEmpty());

        jRadioButtonAufloesungHoch.addActionListener(listener);
        jRadioButtonAufloesungHoch.setSelected(true);

        btnRequestLiveInfo.addActionListener(l -> handleRequestLiveFilmInfo());
    }

    /**
     * Return only the first part of the long codec name.
     * @param stream The video stream from ffprobe.
     * @return First entry of long codec name.
     */
    private String getVideoCodecName(@NotNull Stream stream) {
        var name = stream.getCodecLongName();
        logger.trace("video codec long name: {}", name);
        try {
            var splitName = name.split("/");
            return splitName[0].trim();
        }
        catch (Exception e) {
            return name;
        }
    }

    private void handleRequestLiveFilmInfo() {
        var res = getFilmResolution();
        var url = film.getUrlFuerAufloesung(res);

        btnRequestLiveInfo.setEnabled(false);
        lblBusyIndicator.setVisible(true);
        lblBusyIndicator.setBusy(true);
        lblStatus.setText("");
        lblAudioInfo.setText("");

        Future<FFprobeResult> resultFuture = FFprobe.atPath(ffprobePath)
                .setShowStreams(true)
                .setInput(url)
                .executeAsync();
        resultListenableFuture = JdkFutureAdapters.listenInPoolThread(resultFuture);
        Futures.addCallback(resultListenableFuture, new FutureCallback<>() {
            private static final String ERR_MSG_PART = "Server returned ";
            private static final String MSG_UNKNOWN_ERROR = "Unbekannter Fehler aufgetreten.";

            @Override
            public void onSuccess(FFprobeResult result) {
                var audioStreamResult = result.getStreams().stream().filter(stream -> stream.getCodecType() == StreamType.AUDIO).findAny();
                audioStreamResult.ifPresentOrElse(astream -> {
                            var sample_rate = astream.getSampleRate();
                            final String audio_output = getAudioInfo(astream, sample_rate);
                            SwingUtilities.invokeLater(() -> {
                                lblAudioInfo.setForeground(UIManager.getColor(KEY_LABEL_FOREGROUND));
                                lblAudioInfo.setText(audio_output);
                            });
                        },
                        () -> SwingUtilities.invokeLater(() -> {
                            lblAudioInfo.setForeground(UIManager.getColor(KEY_LABEL_FOREGROUND));
                            lblAudioInfo.setText(NO_DATA_AVAILABLE);
                        }));

                var videoStreamResult = result.getStreams().stream().filter(stream -> stream.getCodecType() == StreamType.VIDEO).findAny();
                videoStreamResult.ifPresentOrElse(stream -> {
                    var frame_rate = stream.getAvgFrameRate().intValue();
                    var codecName = getVideoCodecName(stream);
                    final String video_output = getVideoInfoString(stream, frame_rate, codecName);
                    SwingUtilities.invokeLater(() -> {
                        lblStatus.setForeground(UIManager.getColor(KEY_LABEL_FOREGROUND));
                        lblStatus.setText(video_output);
                    });
                }, () -> SwingUtilities.invokeLater(() -> {
                    lblStatus.setForeground(UIManager.getColor(KEY_LABEL_FOREGROUND));
                    lblStatus.setText(NO_DATA_AVAILABLE);
                }));

                SwingUtilities.invokeLater(() -> resetBusyLabelAndButton());
            }

            private int safe_process_bit_rate(Integer in)
            {
                int bits;
                try {
                    bits = in / 1000;
                }
                catch (Exception e) {
                    bits = 0;
                }
                return bits;
            }

            private String getVideoInfoString(Stream stream, int frame_rate, String codecName) {
                int bit_rate = safe_process_bit_rate(stream.getBitRate());
                String video_output;
                if (bit_rate == 0) {
                    video_output = String.format("Video: %dx%d, %d fps (avg), %s", stream.getWidth(), stream.getHeight(), frame_rate, codecName);
                }
                else {
                    video_output = String.format("Video: %dx%d, %d kBit/s, %d fps (avg), %s", stream.getWidth(), stream.getHeight(), bit_rate, frame_rate, codecName);
                }
                return video_output;
            }

            private String getAudioInfo(Stream astream, Integer sample_rate) {
                int bits_per_sample = safe_process_bit_rate(astream.getBitRate());
                String audio_output;
                if (bits_per_sample == 0) {
                    audio_output = String.format("Audio: %d Hz, %s", sample_rate, astream.getCodecLongName());
                }
                else {
                    audio_output = String.format("Audio: %d Hz, %d kBit/s, %s", sample_rate, bits_per_sample, astream.getCodecLongName());
                }
                return audio_output;
            }

            @Override
            public void onFailure(@NotNull Throwable t) {
                // show nothing when task was cancelled...
                if (t instanceof CancellationException) {
                    SwingUtilities.invokeLater(() -> {
                        lblStatus.setText("");
                        lblAudioInfo.setText("");
                        resetBusyLabelAndButton();
                    });
                }
                else if (t instanceof JaffreeAbnormalExitException e) {
                    String final_str = getJaffreeErrorString(e);
                    setupLabels(final_str);
                }
                else {
                    setupLabels(MSG_UNKNOWN_ERROR);
                }
            }

            private void setupLabels(String text) {
                SwingUtilities.invokeLater(() -> {
                    lblStatus.setText(text);
                    lblStatus.setForeground(Color.RED);
                    lblAudioInfo.setText("");
                    resetBusyLabelAndButton();
                });
            }

            private @NotNull String getJaffreeErrorString(JaffreeAbnormalExitException e) {
                String final_str;
                try {
                    var msg = e.getProcessErrorLogMessages().getFirst().message.split(":");
                    var err_msg = msg[msg.length - 1].trim();
                    if (err_msg.startsWith(ERR_MSG_PART)) {
                        final_str = err_msg.substring(ERR_MSG_PART.length());
                    } else {
                        final_str = MSG_UNKNOWN_ERROR;
                    }
                }
                catch (Exception ignored) {
                    final_str = MSG_UNKNOWN_ERROR;
                }
                return final_str;
            }
        }, Daten.getInstance().getDecoratedPool());
    }

    private void resetBusyLabelAndButton() {
        lblBusyIndicator.setBusy(false);
        lblBusyIndicator.setVisible(false);
        btnRequestLiveInfo.setEnabled(true);
    }

    private void detectFfprobeExecutable() {
        try {
            ffprobePath = GuiFunktionenProgramme.findExecutableOnPath("ffprobe").getParent();
        }
        catch (Exception ex) {
            logger.error("ffprobe not found", ex);
            lblBusyIndicator.setText("Hilfsprogramm nicht gefunden!");
            lblBusyIndicator.setForeground(Color.RED);
            btnRequestLiveInfo.setEnabled(false);
        }
    }

    private void setupBusyIndicator() {
        lblBusyIndicator.setText("");
        lblBusyIndicator.setBusy(false);
        lblBusyIndicator.setVisible(false);
        lblStatus.setText("");
        lblAudioInfo.setText("");
    }

    private void setupUI() {
        setupBusyIndicator();
        detectFfprobeExecutable();

        // launch async tasks first
        launchResolutionFutures();

        jCheckBoxStarten.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN)));
        jCheckBoxStarten.addActionListener(e -> MVConfig.add(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN, String.valueOf(jCheckBoxStarten.isSelected())));

        setupZielButton();

        jButtonOk.addActionListener(e -> {
            if (check()) {
                saveComboPfad(jComboBoxPfad, orgPfad);
                saveDownload();
            }
        });

        jButtonAbbrechen.addActionListener(e -> dispose());

        setupPSetComboBox();
        setupSenderTextField();
        setupNameTextField();
        setupPathTextComponent();

        setupFilmQualityRadioButtons();

        setupDeleteHistoryButton();
        setupPfadSpeichernCheckBox();

        waitForFileSizeFutures();

        setupResolutionButtons();
        calculateAndCheckDiskSpace();
        nameGeaendert = false;
    }

    private void launchResolutionFutures() {
        // always fetch file size during dialog ops...
        restoreFetchSize = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.DOWNLOAD_FETCH_FILE_SIZE, true);
        ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.DOWNLOAD_FETCH_FILE_SIZE, true);

        var decoratedPool = Daten.getInstance().getDecoratedPool();
        hqFuture = decoratedPool.submit(() -> {
            var url = film.getUrlFuerAufloesung(FilmResolution.Enum.HIGH_QUALITY);
            return film.getFileSizeForUrl(url);
        });

        Futures.addCallback(hqFuture, new FutureCallback<>() {
            @Override
            public void onSuccess(String result) {
                SwingUtilities.invokeLater(() -> {
                    if (jRadioButtonAufloesungHd.isEnabled()) {
                        dateiGroesse_HQ = result;
                        if (!dateiGroesse_HQ.isEmpty()) {
                            var text = jRadioButtonAufloesungHd.getText();
                            jRadioButtonAufloesungHd.setText(text + "   [ " + dateiGroesse_HQ + " MB ]");
                        }
                    }
                });
            }

            @Override
            public void onFailure(@NotNull Throwable t) {
                SwingUtilities.invokeLater(() -> {
                    dateiGroesse_HQ = "";
                    logger.error("Failed to retrieve HD resolution", t);
                });
            }
        }, decoratedPool);

        hochFuture = decoratedPool.submit(() -> {
            var url = film.getUrlNormalQuality();
            return film.getFileSizeForUrl(url);
        });
        Futures.addCallback(hochFuture, new FutureCallback<>() {
            @Override
            public void onSuccess(String result) {
                SwingUtilities.invokeLater(() -> {
                    dateiGroesse_Hoch = result;
                    if (!dateiGroesse_Hoch.isEmpty()) {
                        var text = jRadioButtonAufloesungHoch.getText();
                        jRadioButtonAufloesungHoch.setText(text + "   [ " + dateiGroesse_Hoch + " MB ]");
                    }
                });
            }

            @Override
            public void onFailure(@NotNull Throwable t) {
                SwingUtilities.invokeLater(() -> {
                    dateiGroesse_Hoch = "";
                    logger.error("Failed to retrieve Hoch resolution", t);
                });
            }
        }, decoratedPool);

        kleinFuture = decoratedPool.submit(() -> {
            var url = film.getUrlFuerAufloesung(FilmResolution.Enum.LOW);
            return film.getFileSizeForUrl(url);
        });
        Futures.addCallback(kleinFuture, new FutureCallback<>() {
            @Override
            public void onSuccess(String result) {
                SwingUtilities.invokeLater(() -> {
                    if (jRadioButtonAufloesungKlein.isEnabled()) {
                        dateiGroesse_Klein = result;
                        if (!dateiGroesse_Klein.isEmpty()) {
                            var text = jRadioButtonAufloesungKlein.getText();
                            jRadioButtonAufloesungKlein.setText(text + "   [ " + dateiGroesse_Klein + " MB ]");
                        }
                    }
                });
            }

            @Override
            public void onFailure(@NotNull Throwable t) {
                SwingUtilities.invokeLater(() -> {
                    dateiGroesse_Klein = "";
                    logger.error("Failed to retrieve Klein resolution", t);
                });
            }
        }, decoratedPool);
    }

    private DefaultComboBoxModel<String> createPSetComboBoxModel() {
        return new DefaultComboBoxModel<>(listeSpeichern.getObjectDataCombo());
    }

    private void setupPSetComboBox() {
        // disable when only one entry...
        if (listeSpeichern.size() == 1) {
            jComboBoxPset.setEnabled(false);
        }

        var model = createPSetComboBoxModel();
        jComboBoxPset.setModel(model);

        if (active_pSet != null) {
            jComboBoxPset.setSelectedItem(active_pSet.getName());
        } else {
            active_pSet = listeSpeichern.get(jComboBoxPset.getSelectedIndex());
        }
        jComboBoxPset.addActionListener(e -> setupResolutionButtons());
    }

    private void setupSenderTextField() {
        jTextFieldSender.setText(' ' + film.getSender() + ":   " + film.getTitle());
        jTextFieldSender.setBackground(UIManager.getColor("Label.background"));
    }

    private void setupNameTextField() {
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
                        jTextFieldName.setBackground(UIManager.getDefaults().getColor(KEY_TEXTFIELD_BACKGROUND));
                    }
                }

            }
        });
    }

    private void setupPathTextComponent() {
        cbPathTextComponent = ((JTextComponent) jComboBoxPfad.getEditor().getEditorComponent());
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
                        final var editor = jComboBoxPfad.getEditor().getEditorComponent();
                        if (!s.equals(FilenameUtils.checkDateiname(s, true))) {
                            editor.setBackground(MVColor.DOWNLOAD_FEHLER.color);
                        } else {
                            editor.setBackground(UIManager.getColor(KEY_TEXTFIELD_BACKGROUND));
                        }
                    }
                    calculateAndCheckDiskSpace();
                }

            }
        });
    }

    private void setupZielButton() {
        jButtonZiel.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg"));
        jButtonZiel.setText("");
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
    }

    private void setupDeleteHistoryButton() {
        jButtonDelHistory.setText("");
        jButtonDelHistory.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/trash-can.svg"));
        jButtonDelHistory.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN, "");
            jComboBoxPfad.setModel(new DefaultComboBoxModel<>(new String[]{orgPfad}));
        });
    }

    private void waitForFileSizeFutures() {
        // for safety wait for all futures here...
        try {
            hqFuture.get();
            hochFuture.get();
            kleinFuture.get();
        } catch (InterruptedException | ExecutionException e) {
            logger.error("Error occured while waiting for file size futures", e);
        }
        finally {
            //reset fetch size state to previous value
            ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.DOWNLOAD_FETCH_FILE_SIZE, restoreFetchSize);
        }
    }

    private void setupPfadSpeichernCheckBox() {
        final Configuration config = ApplicationConfiguration.getConfiguration();
        jCheckBoxPfadSpeichern.setSelected(config.getBoolean(ApplicationConfiguration.DOWNLOAD_SHOW_LAST_USED_PATH, true));
        jCheckBoxPfadSpeichern.addActionListener(e ->
                config.setProperty(ApplicationConfiguration.DOWNLOAD_SHOW_LAST_USED_PATH, jCheckBoxPfadSpeichern.isSelected()));
    }

    private void setNameFilm() {
        // beim ersten mal werden die Standardpfade gesucht
        if (!nameGeaendert) {
            // nur wenn vom Benutzer noch nicht geändert!
            stopBeob = true;
            datenDownload = new DatenDownload(active_pSet, film, DatenDownload.QUELLE_DOWNLOAD, null, "", "", getFilmResolution().toString());
            if (datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME].isEmpty()) {
                // dann wird nicht gespeichert → eigentlich falsche Seteinstellungen?
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

    /**
     * Calculate free disk space on volume and check if the movies can be safely downloaded.
     */
    private void calculateAndCheckDiskSpace() {
        var fgColor = UIManager.getColor(KEY_LABEL_FOREGROUND);
        if (fgColor != null) {
            jRadioButtonAufloesungHd.setForeground(fgColor);
            jRadioButtonAufloesungHoch.setForeground(fgColor);
            jRadioButtonAufloesungKlein.setForeground(fgColor);
        }

        try {
            var filmBorder = (TitledBorder)jPanelSize.getBorder();
            long usableSpace = getFreeDiskSpace(cbPathTextComponent.getText());
            if (usableSpace > 0) {
                filmBorder.setTitle(TITLED_BORDER_STRING + " [ Freier Speicherplatz: " + FileUtils.humanReadableByteCountBinary(usableSpace) + " ]");
            } else {
                filmBorder.setTitle(TITLED_BORDER_STRING);
            }
            //border needs to be repainted after update...
            jPanelSize.repaint();

            // jetzt noch prüfen, obs auf die Platte passt
            usableSpace /= FileSize.ONE_MiB;
            if (usableSpace > 0) {
                int size;
                if (!dateiGroesse_HQ.isEmpty()) {
                    size = Integer.parseInt(dateiGroesse_HQ);
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
            logger.error("calculateAndCheckDiskSpace()", ex);
        }
    }

    private boolean isHighQualityRequested() {
        return active_pSet.arr[DatenPset.PROGRAMMSET_AUFLOESUNG].equals(FilmResolution.Enum.HIGH_QUALITY.toString())
                && film.isHighQuality();
    }

    private boolean isLowQualityRequested() {
        return active_pSet.arr[DatenPset.PROGRAMMSET_AUFLOESUNG].equals(FilmResolution.Enum.LOW.toString()) &&
                !film.getLowQualityUrl().isEmpty();
    }

    /**
     * Setup the resolution radio buttons based on available download URLs.
     */
    private void setupResolutionButtons() {
        active_pSet = listeSpeichern.get(jComboBoxPset.getSelectedIndex());

        prepareResolutionButtons();
        jCheckBoxInfodatei.setSelected(active_pSet.shouldCreateInfofile());

        prepareSubtitleCheckbox();
        setNameFilm();
    }

    private void prepareResolutionButtons() {
        requestedResolution.ifPresent(it -> highQualityMandated = it == FilmResolution.Enum.HIGH_QUALITY);
        if (highQualityMandated || isHighQualityRequested()) {
            jRadioButtonAufloesungHd.setSelected(true);
        } else if (isLowQualityRequested()) {
            jRadioButtonAufloesungKlein.setSelected(true);
        } else {
            jRadioButtonAufloesungHoch.setSelected(true);
        }
    }

    private void prepareSubtitleCheckbox() {
        if (!film.hasSubtitle()) {
            jCheckBoxSubtitle.setEnabled(false);
        } else {
            jCheckBoxSubtitle.setSelected(active_pSet.shouldDownloadSubtitle());
        }
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
            return dateiGroesse_HQ;
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

    private void addDownloadToQueue()
    {
        Daten.getInstance().getListeDownloads().addMitNummer(datenDownload);
        MessageBus.getMessageBus().publishAsync(new DownloadListChangedEvent());

        if (jCheckBoxStarten.isSelected()) {
            datenDownload.startDownload();
        }
    }

    /**
     * Store download in list and start immediately if requested.
     */
    private void saveDownload() {
        // jetzt wird mit den angegebenen Pfaden gearbeitet
        datenDownload = new DatenDownload(active_pSet, film, DatenDownload.QUELLE_DOWNLOAD, null, jTextFieldName.getText(), Objects.requireNonNull(jComboBoxPfad.getSelectedItem()).toString(), getFilmResolution().toString());
        datenDownload.setGroesse(getFilmSize());
        datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI] = Boolean.toString(jCheckBoxInfodatei.isSelected());
        datenDownload.arr[DatenDownload.DOWNLOAD_SUBTITLE] = Boolean.toString(jCheckBoxSubtitle.isSelected());

        addDownloadToQueue();

        dispose();
    }

    private static class DialogPositionComponentListener extends ComponentAdapter {
        @Override
        public void componentResized(ComponentEvent e) {
            storeWindowPosition(e);
        }

        @Override
        public void componentMoved(ComponentEvent e) {
            storeWindowPosition(e);
        }

        private void storeWindowPosition(ComponentEvent e) {
            var config = ApplicationConfiguration.getConfiguration();
            var component = e.getComponent();

            var dims = component.getSize();
            var loc = component.getLocation();
            try {
                config.lock(LockMode.WRITE);
                config.setProperty(ApplicationConfiguration.AddDownloadDialog.WIDTH, dims.width);
                config.setProperty(ApplicationConfiguration.AddDownloadDialog.HEIGHT, dims.height);
                config.setProperty(ApplicationConfiguration.AddDownloadDialog.X, loc.x);
                config.setProperty(ApplicationConfiguration.AddDownloadDialog.Y, loc.y);
            } finally {
                config.unlock(LockMode.WRITE);
            }
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.ButtonGroup buttonGroup1 = new javax.swing.ButtonGroup();
        jButtonOk = new javax.swing.JButton();
        jButtonAbbrechen = new javax.swing.JButton();
        jCheckBoxStarten = new javax.swing.JCheckBox();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        jCheckBoxInfodatei = new javax.swing.JCheckBox();
        jCheckBoxPfadSpeichern = new javax.swing.JCheckBox();
        jCheckBoxSubtitle = new javax.swing.JCheckBox();
        javax.swing.JPanel jPanel7 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        jTextFieldName = new javax.swing.JTextField();
        javax.swing.JLabel jLabelSet = new javax.swing.JLabel();
        jComboBoxPset = new javax.swing.JComboBox<>();
        javax.swing.JLabel jLabel4 = new javax.swing.JLabel();
        javax.swing.JPanel jPanel4 = new javax.swing.JPanel();
        jComboBoxPfad = new javax.swing.JComboBox<>();
        javax.swing.Box.Filler filler1 = new javax.swing.Box.Filler(new java.awt.Dimension(5, 0), new java.awt.Dimension(5, 0), new java.awt.Dimension(5, 32767));
        jButtonZiel = new javax.swing.JButton();
        javax.swing.Box.Filler filler2 = new javax.swing.Box.Filler(new java.awt.Dimension(5, 0), new java.awt.Dimension(5, 0), new java.awt.Dimension(5, 32767));
        jButtonDelHistory = new javax.swing.JButton();
        jPanelSize = new javax.swing.JPanel();
        javax.swing.JPanel jPanel3 = new javax.swing.JPanel();
        btnRequestLiveInfo = new javax.swing.JButton();
        lblBusyIndicator = new org.jdesktop.swingx.JXBusyLabel();
        javax.swing.JPanel jPanel5 = new javax.swing.JPanel();
        lblStatus = new javax.swing.JLabel();
        lblAudioInfo = new javax.swing.JLabel();
        javax.swing.JPanel jPanel6 = new javax.swing.JPanel();
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

        jPanel2.setLayout(new java.awt.GridLayout(2, 2));

        jCheckBoxInfodatei.setText("Lege Infodatei an");
        jCheckBoxInfodatei.setToolTipText("Erzeugt eine Infodatei im Format \"Infodatei.txt\"");
        jPanel2.add(jCheckBoxInfodatei);

        jCheckBoxPfadSpeichern.setText("Zielpfad speichern");
        jPanel2.add(jCheckBoxPfadSpeichern);

        jCheckBoxSubtitle.setText("Untertitel speichern: \"Filmname.xxx\"");
        jPanel2.add(jCheckBoxSubtitle);

        jPanel7.setMaximumSize(new java.awt.Dimension(606, 32767));

        jLabel1.setText("Zielpfad:");

        jLabelSet.setText("Set:");

        jLabel4.setText("Dateiname:");

        jPanel4.setLayout(new javax.swing.BoxLayout(jPanel4, javax.swing.BoxLayout.LINE_AXIS));

        jComboBoxPfad.setEditable(true);
        jPanel4.add(jComboBoxPfad);
        jPanel4.add(filler1);

        jButtonZiel.setText("F");
        jButtonZiel.setToolTipText("Zielpfad auswählen");
        jPanel4.add(jButtonZiel);
        jPanel4.add(filler2);

        jButtonDelHistory.setText("H");
        jButtonDelHistory.setToolTipText("History löschen");
        jPanel4.add(jButtonDelHistory);

        javax.swing.GroupLayout jPanel7Layout = new javax.swing.GroupLayout(jPanel7);
        jPanel7.setLayout(jPanel7Layout);
        jPanel7Layout.setHorizontalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel7Layout.createSequentialGroup()
                        .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabelSet)
                            .addComponent(jLabel1))
                        .addGap(20, 20, 20)
                        .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jComboBoxPset, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                    .addGroup(jPanel7Layout.createSequentialGroup()
                        .addComponent(jLabel4)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldName)))
                .addContainerGap())
        );
        jPanel7Layout.setVerticalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabelSet)
                    .addComponent(jComboBoxPset, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jPanel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel1))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel4)
                    .addComponent(jTextFieldName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, 606, Short.MAX_VALUE)
                    .addComponent(jPanel7, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel7, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(12, 12, 12)
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanelSize.setBorder(javax.swing.BorderFactory.createTitledBorder("Download-Qualität"));

        jPanel3.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.LEFT));

        btnRequestLiveInfo.setText("Codec-Details abrufen...");
        jPanel3.add(btnRequestLiveInfo);
        jPanel3.add(lblBusyIndicator);

        jPanel5.setLayout(new java.awt.GridLayout(2, 1));

        lblStatus.setText("status");
        jPanel5.add(lblStatus);

        lblAudioInfo.setText("audio");
        jPanel5.add(lblAudioInfo);

        jPanel3.add(jPanel5);

        buttonGroup1.add(jRadioButtonAufloesungHd);
        jRadioButtonAufloesungHd.setText("Höchste/Hoch");
        jPanel6.add(jRadioButtonAufloesungHd);

        buttonGroup1.add(jRadioButtonAufloesungHoch);
        jRadioButtonAufloesungHoch.setText("Mittel");
        jPanel6.add(jRadioButtonAufloesungHoch);

        buttonGroup1.add(jRadioButtonAufloesungKlein);
        jRadioButtonAufloesungKlein.setText("Niedrig");
        jPanel6.add(jRadioButtonAufloesungKlein);

        javax.swing.GroupLayout jPanelSizeLayout = new javax.swing.GroupLayout(jPanelSize);
        jPanelSize.setLayout(jPanelSizeLayout);
        jPanelSizeLayout.setHorizontalGroup(
            jPanelSizeLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelSizeLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelSizeLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, 596, Short.MAX_VALUE)
                    .addGroup(jPanelSizeLayout.createSequentialGroup()
                        .addComponent(jPanel6, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanelSizeLayout.setVerticalGroup(
            jPanelSizeLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelSizeLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel6, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jTextFieldSender.setEditable(false);
        jTextFieldSender.setFont(jTextFieldSender.getFont().deriveFont(jTextFieldSender.getFont().getStyle() | java.awt.Font.BOLD));
        jTextFieldSender.setText(" ARD: Tatort, ...");
        jTextFieldSender.setBorder(javax.swing.BorderFactory.createTitledBorder("Film"));

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jTextFieldSender, javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanelSize, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.LEADING, layout.createSequentialGroup()
                        .addComponent(jCheckBoxStarten)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 264, Short.MAX_VALUE)
                        .addComponent(jButtonOk, javax.swing.GroupLayout.PREFERRED_SIZE, 93, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonAbbrechen)))
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, jButtonAbbrechen, jButtonOk);

        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTextFieldSender, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanelSize, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jCheckBoxStarten, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jButtonOk)
                        .addComponent(jButtonAbbrechen)))
                .addContainerGap())
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton btnRequestLiveInfo;
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
    private javax.swing.JPanel jPanelSize;
    private javax.swing.JRadioButton jRadioButtonAufloesungHd;
    private javax.swing.JRadioButton jRadioButtonAufloesungHoch;
    private javax.swing.JRadioButton jRadioButtonAufloesungKlein;
    private javax.swing.JTextField jTextFieldName;
    private javax.swing.JTextField jTextFieldSender;
    private javax.swing.JLabel lblAudioInfo;
    private org.jdesktop.swingx.JXBusyLabel lblBusyIndicator;
    private javax.swing.JLabel lblStatus;
    // End of variables declaration//GEN-END:variables
}
