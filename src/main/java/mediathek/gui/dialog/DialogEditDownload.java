package mediathek.gui.dialog;

import mediathek.config.Konstanten;
import mediathek.config.MVColor;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenProg;
import mediathek.daten.FilmResolution;
import mediathek.swing.IconUtils;
import mediathek.tool.*;
import org.apache.commons.configuration2.sync.LockMode;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.TableColumnModel;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.io.File;
import java.util.NoSuchElementException;

public class DialogEditDownload extends JDialog {
    private final DatenDownload datenDownload;
    public boolean ok;
    private final JTextField[] textfeldListe = new JTextField[DatenDownload.MAX_ELEM];
    private final JLabel[] labelListe = new JLabel[DatenDownload.MAX_ELEM];
    private final JCheckBox jCheckBoxRestart = new JCheckBox();
    private final JCheckBox jCheckBoxDownloadmanager = new JCheckBox();
    private final JCheckBox jCheckBoxUnterbrochen = new JCheckBox();
    private final JCheckBox jCheckBoxInfodatei = new JCheckBox();
    private final JCheckBox jCheckBoxSubtitle = new JCheckBox();
    private final JCheckBox jCheckBoxSpotlight = new JCheckBox();
    private final MVPanelDownloadZiel mVPanelDownloadZiel;
    private final boolean gestartet;
    private String dateiGroesse_HD = "";
    private String dateiGroesse_Hoch = "";
    private String dateiGroesse_Klein = "";
    private final JFrame parent;
    private final String orgProgArray;
    private FilmResolution.Enum resolution = FilmResolution.Enum.NORMAL;
    private final JCheckBox cbHighQuality = new JCheckBox();
    private final JCheckBox cbSubtitleAvailable = new JCheckBox();
    private final TableColumnModel columnModel;
    private final GridBagLayout gridbag = new GridBagLayout();


    public DialogEditDownload(JFrame parent, boolean modal, DatenDownload datenDownload, boolean started, TableColumnModel colModel) {
        super(parent, modal);
        initComponents();
        this.parent = parent;
        this.datenDownload = datenDownload;
        this.gestartet = started;
        columnModel = colModel;

        cbSubtitleAvailable.setEnabled(false);
        cbHighQuality.setEnabled(false);

        orgProgArray = this.datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY];
        mVPanelDownloadZiel = new MVPanelDownloadZiel(parent, this.datenDownload, false);
        mVPanelDownloadZiel.setBorder(BorderFactory.createLineBorder(new Color(204, 204, 204)));
        jRadioButtonResHd.addActionListener(_ -> changeRes());
        jRadioButtonResHi.addActionListener(_ -> changeRes());
        jRadioButtonResLo.addActionListener(_ -> changeRes());
        jButtonOk.addActionListener(_ -> {
            if (check()) {
                dispose();
            }
        });
        jButtonAbbrechen.addActionListener(_ -> dispose());
        getRootPane().setDefaultButton(jButtonOk);

        EscapeKeyHandler.installHandler(this, this::dispose);

        setupResolutionButtons();
        setExtra();

        restoreLocation();

        addComponentListener(new ComponentAdapter() {
            @Override
            public void componentResized(ComponentEvent e) {
                saveLocation();
            }

            @Override
            public void componentMoved(ComponentEvent e) {
                saveLocation();
            }
        });
    }

    private void restoreLocation() {
        var config = ApplicationConfiguration.getConfiguration();
        config.lock(LockMode.READ);
        try {
            var newLocation = new Point();
            newLocation.x = config.getInt(ApplicationConfiguration.EditDownloadDialog.X);
            newLocation.y = config.getInt(ApplicationConfiguration.EditDownloadDialog.Y);
            setLocation(newLocation);

            int w = config.getInt(ApplicationConfiguration.EditDownloadDialog.WIDTH, -1);
            int h = config.getInt(ApplicationConfiguration.EditDownloadDialog.HEIGHT, -1);
            if (w != -1 && h != -1) {
                setSize(w,h);
            }
        } catch (NoSuchElementException ignored) {
        } finally {
            config.unlock(LockMode.READ);
        }
    }

    private void saveLocation() {
        if (!isVisible())
            return;
        var config = ApplicationConfiguration.getConfiguration();
        try {
            config.lock(LockMode.WRITE);
            var location = getLocationOnScreen();
            config.setProperty(ApplicationConfiguration.EditDownloadDialog.X, location.x);
            config.setProperty(ApplicationConfiguration.EditDownloadDialog.Y, location.y);
            config.setProperty(ApplicationConfiguration.EditDownloadDialog.WIDTH, getWidth());
            config.setProperty(ApplicationConfiguration.EditDownloadDialog.HEIGHT, getHeight());
        } finally {
            config.unlock(LockMode.WRITE);
        }
    }

    private void setupResolutionButtons() {
        jRadioButtonResHd.setEnabled(false);
        jRadioButtonResHi.setEnabled(false);
        jRadioButtonResLo.setEnabled(false);
        if (datenDownload.art != DatenDownload.ART_DOWNLOAD && datenDownload.pSet == null) {
            // ansonsten müsste erst der Programmaufruf neu gebaut werden
            jPanelRes.setVisible(false);
            return;
        }
        if (datenDownload.film != null) {
            jRadioButtonResHi.setEnabled(!gestartet);
            jRadioButtonResHi.setSelected(datenDownload.arr[DatenDownload.DOWNLOAD_URL].equals(datenDownload.film.getUrlFuerAufloesung(FilmResolution.Enum.NORMAL)));
            dateiGroesse_Hoch = datenDownload.film.getFileSizeForUrl(datenDownload.film.getUrlFuerAufloesung(FilmResolution.Enum.NORMAL));
            if (!dateiGroesse_Hoch.isEmpty()) {
                jRadioButtonResHi.setText(jRadioButtonResHi.getText() + "   [ " + dateiGroesse_Hoch + " MB ]");
            }

            if (!datenDownload.film.getHighQualityUrl().isEmpty()) {
                jRadioButtonResHd.setEnabled(!gestartet);
                jRadioButtonResHd.setSelected(datenDownload.arr[DatenDownload.DOWNLOAD_URL].equals(datenDownload.film.getUrlFuerAufloesung(FilmResolution.Enum.HIGH_QUALITY)));
                dateiGroesse_HD = datenDownload.film.getFileSizeForUrl(datenDownload.film.getUrlFuerAufloesung(FilmResolution.Enum.HIGH_QUALITY));
                if (!dateiGroesse_HD.isEmpty()) {
                    jRadioButtonResHd.setText(jRadioButtonResHd.getText() + "   [ " + dateiGroesse_HD + " MB ]");
                }
            }

            if (!datenDownload.film.getLowQualityUrl().isEmpty()) {
                jRadioButtonResLo.setEnabled(!gestartet);
                jRadioButtonResLo.setSelected(datenDownload.arr[DatenDownload.DOWNLOAD_URL].equals(datenDownload.film.getUrlFuerAufloesung(FilmResolution.Enum.LOW)));
                dateiGroesse_Klein = datenDownload.film.getFileSizeForUrl(datenDownload.film.getUrlFuerAufloesung(FilmResolution.Enum.LOW));
                if (!dateiGroesse_Klein.isEmpty()) {
                    jRadioButtonResLo.setText(jRadioButtonResLo.getText() + "   [ " + dateiGroesse_Klein + " MB ]");
                }
            }

        }

        resolution = getRadioButtonResolution();
    }

    private FilmResolution.Enum getRadioButtonResolution() {
        FilmResolution.Enum res;
        if (jRadioButtonResHd.isSelected()) {
            res = FilmResolution.Enum.HIGH_QUALITY;
        } else if (jRadioButtonResLo.isSelected()) {
            res = FilmResolution.Enum.LOW;
        } else {
            res = FilmResolution.Enum.NORMAL;
        }

        return res;
    }

    private void changeRes() {
        // RadioButton sind nur enabled wenn "datenDownload.film" vorhanden
        var res = getRadioButtonResolution();
        datenDownload.arr[DatenDownload.DOWNLOAD_URL] = datenDownload.film.getUrlFuerAufloesung(res);
        textfeldListe[DatenDownload.DOWNLOAD_URL].setText(datenDownload.arr[DatenDownload.DOWNLOAD_URL]);

        final String size;
        if (jRadioButtonResHd.isSelected()) {
            size = dateiGroesse_HD;
        } else if (jRadioButtonResLo.isSelected()) {
            size = dateiGroesse_Klein;
        } else {
            size = dateiGroesse_Hoch;
        }
        if (datenDownload.art == DatenDownload.ART_PROGRAMM && datenDownload.pSet != null) {
            // muss noch der Programmaufruf neu gebaut werden
            DatenDownload d = new DatenDownload(datenDownload.pSet, datenDownload.film, datenDownload.quelle, datenDownload.abo,
                    datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME],
                    datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD], res.toString());

            datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF] = d.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF];
            datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY] = d.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY];
            textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF].setText(datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF]);
            textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY].setText(datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY]);
        }
        datenDownload.setGroesse(size);
    }

    private void setExtra() {
        jPanelExtra.removeAll();
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(5, 10, 10, 5);

        jPanelExtra.setLayout(gridbag);
        int zeile = 0;
        var colCount = columnModel.getColumnCount();
        for (int i = 0; i < colCount; ++i) {
            //skip information that is not useful to the user...
            if (doNotShow(i)) {
                continue;
            }
            //do not show Abo information if there is none...
            if (i == DatenDownload.DOWNLOAD_ABO && datenDownload.arr[DatenDownload.DOWNLOAD_ABO].isEmpty())
                continue;
            //do not show geo information if not needed...
            if (i == DatenDownload.DOWNLOAD_GEO && datenDownload.arr[DatenDownload.DOWNLOAD_GEO].isEmpty())
                continue;
            //do not show subtitle url if it is empty...
            if (i == DatenDownload.DOWNLOAD_URL_SUBTITLE && datenDownload.arr[DatenDownload.DOWNLOAD_URL_SUBTITLE].isEmpty())
                continue;

            JLabel label = new JLabel("  " + columnModel.getColumn(i).getHeaderValue() + ": ");
            labelListe[i] = label;
            JTextField textfeld = new JTextField();
            textfeld.setEditable(false);
            textfeld.setText(datenDownload.arr[i]);
            textfeldListe[i] = textfeld;
            addExtraFeld(i, c);
            ++zeile;
            c.gridy = zeile;
        }
        jPanelExtra.validate();
    }

    private boolean doNotShow(int i) {
        return i == DatenDownload.DOWNLOAD_ZURUECKGESTELLT
                || i == DatenDownload.DOWNLOAD_URL_RTMP
                || i == DatenDownload.DOWNLOAD_BUTTON_DEL || i == DatenDownload.DOWNLOAD_BUTTON_START
                || i == DatenDownload.DOWNLOAD_REF
                || i == DatenDownload.DOWNLOAD_BANDBREITE
                || i == DatenDownload.DOWNLOAD_NR
                || i == DatenDownload.DOWNLOAD_FILM_NR
                || i == DatenDownload.DOWNLOAD_HISTORY_URL
                || i == DatenDownload.DOWNLOAD_PROGRESS
                || i == DatenDownload.DOWNLOAD_RESTZEIT;
    }

    private void addExtraFeld(int i, GridBagConstraints c) {
        //Label
        c.gridx = 0;
        c.weightx = 0;
        if (doNotShow(i)) {
            return;
        }

        if (datenDownload.art == DatenDownload.ART_DOWNLOAD
                && (i == DatenDownload.DOWNLOAD_ZIEL_DATEINAME
                || i == DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME
                || i == DatenDownload.DOWNLOAD_ZIEL_PFAD)
                && !gestartet) {
            // Downloadpfad anpassen
            if (i == DatenDownload.DOWNLOAD_ZIEL_DATEINAME) {
                c.gridx = 1;
                c.weightx = 10;
                gridbag.setConstraints(mVPanelDownloadZiel, c);
                jPanelExtra.add(mVPanelDownloadZiel);
            }
        } else {
            switch (i) {
                case DatenDownload.DOWNLOAD_PROGRAMM_RESTART:
                    labelListe[i].setForeground(MVColor.getBlueColor());
                    jCheckBoxRestart.setSelected(datenDownload.isRestart());
                    jCheckBoxRestart.addActionListener(new BeobCheckbox());
                    jCheckBoxRestart.setEnabled(!gestartet);
                    gridbag.setConstraints(labelListe[i], c);
                    jPanelExtra.add(labelListe[i]);
                    c.gridx = 1;
                    c.weightx = 10;
                    gridbag.setConstraints(jCheckBoxRestart, c);
                    jPanelExtra.add(jCheckBoxRestart);
                    if (datenDownload.isDownloadManager()) {
                        jCheckBoxRestart.setEnabled(false);
                    }
                    break;
                case DatenDownload.DOWNLOAD_PROGRAMM_DOWNLOADMANAGER:
                    jCheckBoxDownloadmanager.setSelected(datenDownload.isDownloadManager());
                    // jCheckBoxRestart.addActionListener(new BeobCheckbox());
                    jCheckBoxDownloadmanager.setEnabled(false);
                    gridbag.setConstraints(labelListe[i], c);
                    jPanelExtra.add(labelListe[i]);
                    c.gridx = 1;
                    c.weightx = 10;
                    gridbag.setConstraints(jCheckBoxDownloadmanager, c);
                    jPanelExtra.add(jCheckBoxDownloadmanager);
                    break;
                case DatenDownload.DOWNLOAD_UNTERBROCHEN:
                    // das macht nur Sinn, wenn der Download unterbrochen ist um "es" auszuschalten
                    // Unterbrechung einschalten macht keinen Sinn
                    if (!datenDownload.isInterrupted()) {
                        return;
                    }
                    labelListe[i].setForeground(MVColor.getBlueColor());
                    jCheckBoxUnterbrochen.setSelected(datenDownload.isInterrupted());
                    jCheckBoxUnterbrochen.addActionListener(new BeobCheckbox());
                    jCheckBoxUnterbrochen.setEnabled(!gestartet);
                    gridbag.setConstraints(labelListe[i], c);
                    jPanelExtra.add(labelListe[i]);
                    c.gridx = 1;
                    c.weightx = 10;
                    gridbag.setConstraints(jCheckBoxUnterbrochen, c);
                    jPanelExtra.add(jCheckBoxUnterbrochen);
                    break;
                case DatenDownload.DOWNLOAD_INFODATEI:
                    labelListe[i].setForeground(MVColor.getBlueColor());
                    jCheckBoxInfodatei.setSelected(Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI]));
                    jCheckBoxInfodatei.addActionListener(new BeobCheckbox());
                    jCheckBoxInfodatei.setEnabled(!gestartet);
                    gridbag.setConstraints(labelListe[i], c);
                    jPanelExtra.add(labelListe[i]);
                    c.gridx = 1;
                    c.weightx = 10;
                    gridbag.setConstraints(jCheckBoxInfodatei, c);
                    jPanelExtra.add(jCheckBoxInfodatei);
                    break;
                case DatenDownload.DOWNLOAD_SUBTITLE:
                    labelListe[i].setForeground(MVColor.getBlueColor());
                    jCheckBoxSubtitle.setSelected(Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_SUBTITLE]));
                    jCheckBoxSubtitle.addActionListener(new BeobCheckbox());
                    jCheckBoxSubtitle.setEnabled(!gestartet);
                    gridbag.setConstraints(labelListe[i], c);
                    jPanelExtra.add(labelListe[i]);
                    c.gridx = 1;
                    c.weightx = 10;
                    gridbag.setConstraints(jCheckBoxSubtitle, c);
                    jPanelExtra.add(jCheckBoxSubtitle);
                    break;
                case DatenDownload.DOWNLOAD_SPOTLIGHT:
                    labelListe[i].setForeground(MVColor.getBlueColor());
                    jCheckBoxSpotlight.setSelected(Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_SPOTLIGHT]));
                    jCheckBoxSpotlight.addActionListener(new BeobCheckbox());
                    jCheckBoxSpotlight.setEnabled(!gestartet);
                    gridbag.setConstraints(labelListe[i], c);
                    jPanelExtra.add(labelListe[i]);
                    c.gridx = 1;
                    c.weightx = 10;
                    gridbag.setConstraints(jCheckBoxSpotlight, c);
                    jPanelExtra.add(jCheckBoxSpotlight);
                    break;
                case DatenDownload.DOWNLOAD_HD:
                    cbHighQuality.setSelected(true);
                    gridbag.setConstraints(labelListe[i], c);
                    jPanelExtra.add(labelListe[i]);
                    c.gridx = 1;
                    c.weightx = 10;
                    gridbag.setConstraints(cbHighQuality, c);
                    jPanelExtra.add(cbHighQuality);
                    if (datenDownload.film != null) {
                        cbHighQuality.setVisible(datenDownload.film.isHighQuality());
                    } else {
                        cbHighQuality.setVisible(false);
                    }
                    break;
                case DatenDownload.DOWNLOAD_UT:
                    cbSubtitleAvailable.setSelected(true);
                    gridbag.setConstraints(labelListe[i], c);
                    jPanelExtra.add(labelListe[i]);
                    c.gridx = 1;
                    c.weightx = 10;
                    gridbag.setConstraints(cbSubtitleAvailable, c);
                    jPanelExtra.add(cbSubtitleAvailable);
                    if (datenDownload.film != null) {
                        cbSubtitleAvailable.setVisible(datenDownload.film.hasSubtitle());
                    } else {
                        cbSubtitleAvailable.setVisible(false);
                    }
                    break;
                case DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF:
                    break;
                case DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY:
                    if (datenDownload.art == DatenDownload.ART_PROGRAMM) {
                        // nur bei Downloads über ein Programm
                        if (datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY].isEmpty()) {
                            // Aufruf über Array ist leer -> Win, Mac
                            labelListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY].setForeground(MVColor.getBlueColor());
                            textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF].setEditable(!gestartet);// und wenn noch nicht gestartet
                            textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF].getDocument().addDocumentListener(new BeobachterDocumentTextfeld(DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF));
                            gridbag.setConstraints(labelListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY], c);
                            jPanelExtra.add(labelListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY]);
                            c.gridx = 1;
                            c.weightx = 10;
                            gridbag.setConstraints(textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF], c);
                            jPanelExtra.add(textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF]);
                        } else {
                            // dann ist ein Array vorhanden -> Linux
                            labelListe[i].setForeground(MVColor.getBlueColor());
                            textfeldListe[i].setEditable(!gestartet);// und wenn noch nicht gestartet
                            textfeldListe[i].getDocument().addDocumentListener(new BeobachterDocumentTextfeld(i));
                            gridbag.setConstraints(labelListe[i], c);
                            jPanelExtra.add(labelListe[i]);
                            JPanel jp = new JPanel();
                            jp.setBorder(javax.swing.BorderFactory.createTitledBorder(""));
                            GridBagLayout gb = new GridBagLayout();
                            GridBagConstraints gc = new GridBagConstraints();
                            gc.fill = GridBagConstraints.HORIZONTAL;
                            gc.insets = new Insets(2, 2, 2, 2);
                            jp.setLayout(gb);

                            JButton jButtonReset = new JButton("");
                            jButtonReset.setToolTipText("Reset");
                            jButtonReset.setIcon(IconUtils.of(FontAwesomeSolid.REDO_ALT));
                            jButtonReset.addActionListener(_ -> textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY].setText(orgProgArray));
                            JButton jButtonHelp = new JButton("");
                            jButtonHelp.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg"));
                            jButtonHelp.setToolTipText("Hilfe anzeigen");
                            jButtonHelp.addActionListener(_ -> new DialogHilfe(parent, true, GetFile.getHilfeSuchen(Konstanten.PFAD_HILFETEXT_EDIT_DOWNLOAD_PROG)).setVisible(true));

                            gc.gridy = 0;
                            gc.gridx = 0;
                            gc.weightx = 1;
                            gb.setConstraints(jButtonHelp, gc);
                            jp.add(jButtonHelp);
                            gc.gridx = 1;
                            gc.weightx = 10;
                            gb.setConstraints(textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF], gc);
                            jp.add(textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF]);

                            gc.gridy = 1;
                            gc.gridx = 0;
                            gc.weightx = 1;
                            gb.setConstraints(jButtonReset, gc);
                            jp.add(jButtonReset);
                            gc.gridx = 1;
                            gc.weightx = 10;
                            gb.setConstraints(textfeldListe[i], gc);
                            jp.add(textfeldListe[i]);

                            c.gridx = 1;
                            c.weightx = 10;
                            gridbag.setConstraints(jp, c);
                            jPanelExtra.add(jp);
                        }
                    }
                    break;
                default:
                    switch (i) {
                        case DatenDownload.DOWNLOAD_NR -> textfeldListe[i].setText(String.valueOf(datenDownload.nr));
                        case DatenDownload.DOWNLOAD_FILM_NR -> {
                            if (datenDownload.film != null) {
                                textfeldListe[i].setText(String.valueOf(datenDownload.film.getFilmNr()));
                            }
                        }
                        case DatenDownload.DOWNLOAD_URL -> {
                            if (datenDownload.art == DatenDownload.ART_DOWNLOAD) {
                                // nur bei direkten Downloads
                                labelListe[i].setForeground(MVColor.getBlueColor());
                                textfeldListe[i].setEditable(!gestartet);// und wenn noch nicht gestartet
                                textfeldListe[i].getDocument().addDocumentListener(new BeobachterDocumentTextfeld(i));
                            }
                        }
                        case DatenDownload.DOWNLOAD_GROESSE ->
                                textfeldListe[i].setText(datenDownload.mVFilmSize.toString());
                        case DatenDownload.DOWNLOAD_PROGRESS ->
                                textfeldListe[i].setText(Start.getTextProgress(datenDownload.isDownloadManager(), datenDownload.start));
                        case DatenDownload.DOWNLOAD_RESTZEIT ->
                                textfeldListe[i].setText(datenDownload.getTextRestzeit());
                        case DatenDownload.DOWNLOAD_ART -> {
                            switch (datenDownload.art) {
                                case DatenDownload.ART_DOWNLOAD ->
                                        textfeldListe[i].setText(DatenDownload.ART_DOWNLOAD_TXT);
                                case DatenDownload.ART_PROGRAMM ->
                                        textfeldListe[i].setText(DatenDownload.ART_PROGRAMM_TXT);
                            }
                        }
                        case DatenDownload.DOWNLOAD_QUELLE -> {
                            switch (datenDownload.quelle) {
                                case DatenDownload.QUELLE_ALLE ->
                                        textfeldListe[i].setText(DatenDownload.QUELLE_ALLE_TXT);
                                case DatenDownload.QUELLE_ABO -> textfeldListe[i].setText(DatenDownload.QUELLE_ABO_TXT);
                                case DatenDownload.QUELLE_BUTTON ->
                                        textfeldListe[i].setText(DatenDownload.QUELLE_BUTTON_TXT);
                                case DatenDownload.QUELLE_DOWNLOAD ->
                                        textfeldListe[i].setText(DatenDownload.QUELLE_DOWNLOAD_TXT);
                            }
                        }
                        default -> {
                        }
                    }
                    gridbag.setConstraints(labelListe[i], c);
                    jPanelExtra.add(labelListe[i]);
                    //Textfeld
                    c.gridx = 1;
                    c.weightx = 10;
                    gridbag.setConstraints(textfeldListe[i], c);
                    jPanelExtra.add(textfeldListe[i]);
                    break;
            }
        }
    }

    private static final Logger logger = LogManager.getLogger(DialogEditDownload.class);

    private boolean downloadDateiLoeschen(DatenDownload datenDownload) {
        try {
            File file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
            if (!file.exists()) {
                return true; // gibt nichts zu löschen
            }

            int ret = JOptionPane.showConfirmDialog(parent,
                    "Die Auflösung wurde geändert, der Film kann nicht weitergeführt werden.\n"
                    + "Datei muss zuerst gelöscht werden.", "Film Löschen?", JOptionPane.YES_NO_OPTION);
            if (ret != JOptionPane.OK_OPTION) {
                return false; // user will nicht
            }

            // und jetzt die Datei löschen
            logger.info("Datei löschen: {}", file.getAbsolutePath());
            if (!file.delete()) {
                throw new Exception();
            }
        } catch (Exception ex) {
            MVMessageDialog.showMessageDialog(parent, "Konnte die Datei nicht löschen!", "Film löschen", JOptionPane.ERROR_MESSAGE);
            logger.error("Fehler beim löschen: {}", datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        }
        return true;
    }

    private boolean check() {
        mVPanelDownloadZiel.setPfadName_geaendert();
        if ((jRadioButtonResHd.isSelected() && !(resolution == FilmResolution.Enum.HIGH_QUALITY))
                || (jRadioButtonResLo.isSelected() && !(resolution == FilmResolution.Enum.LOW))
                || (jRadioButtonResHi.isSelected() && !(resolution == FilmResolution.Enum.NORMAL))) {
            // dann wurde die Auflösung geändert -> Film kann nicht weitergeführt werden
            ok = downloadDateiLoeschen(datenDownload);
        } else {
            ok = true;
        }
        return ok;
    }

    private class BeobachterDocumentTextfeld implements DocumentListener {

        final int nr;

        public BeobachterDocumentTextfeld(int n) {
            nr = n;
        }

        @Override
        public void insertUpdate(DocumentEvent arg0) {
            eingabe();
        }

        @Override
        public void removeUpdate(DocumentEvent arg0) {
            eingabe();
        }

        @Override
        public void changedUpdate(DocumentEvent arg0) {
            eingabe();
        }

        private void eingabe() {
            datenDownload.arr[nr] = textfeldListe[nr].getText().trim();
            if (nr == DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY) {
                datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF] = DatenProg.makeProgAufrufArray(datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY]);
                textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF].setText(datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF]);
            }
        }
    }

    private class BeobCheckbox implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_RESTART] = Boolean.toString(jCheckBoxRestart.isSelected());
            datenDownload.arr[DatenDownload.DOWNLOAD_UNTERBROCHEN] = Boolean.toString(jCheckBoxUnterbrochen.isSelected());
            datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI] = Boolean.toString(jCheckBoxInfodatei.isSelected());
            datenDownload.arr[DatenDownload.DOWNLOAD_SUBTITLE] = Boolean.toString(jCheckBoxSubtitle.isSelected());
            datenDownload.arr[DatenDownload.DOWNLOAD_SPOTLIGHT] = Boolean.toString(jCheckBoxSpotlight.isSelected());
        }
    }


    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.ButtonGroup buttonGroup1 = new javax.swing.ButtonGroup();
        javax.swing.JScrollPane jScrollPane1 = new javax.swing.JScrollPane();
        jPanelExtra = new javax.swing.JPanel();
        jPanelRes = new javax.swing.JPanel();
        jRadioButtonResHd = new javax.swing.JRadioButton();
        jRadioButtonResHi = new javax.swing.JRadioButton();
        jRadioButtonResLo = new javax.swing.JRadioButton();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        jButtonOk = new javax.swing.JButton();
        jButtonAbbrechen = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Download ändern");
        setMinimumSize(new java.awt.Dimension(500, 0));

        javax.swing.GroupLayout jPanelExtraLayout = new javax.swing.GroupLayout(jPanelExtra);
        jPanelExtra.setLayout(jPanelExtraLayout);
        jPanelExtraLayout.setHorizontalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
        );
        jPanelExtraLayout.setVerticalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
        );

        jScrollPane1.setViewportView(jPanelExtra);

        jPanelRes.setBorder(javax.swing.BorderFactory.createTitledBorder("Download-Qualität:"));
        jPanelRes.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.LEFT, 10, 5));

        buttonGroup1.add(jRadioButtonResHd);
        jRadioButtonResHd.setText("Höchste/Hoch");
        jPanelRes.add(jRadioButtonResHd);

        buttonGroup1.add(jRadioButtonResHi);
        jRadioButtonResHi.setText("Mittel");
        jPanelRes.add(jRadioButtonResHi);

        buttonGroup1.add(jRadioButtonResLo);
        jRadioButtonResLo.setText("Niedrig");
        jPanelRes.add(jRadioButtonResLo);

        jButtonOk.setText("Ok");
        jPanel1.add(jButtonOk);

        jButtonAbbrechen.setText("Abbrechen");
        jPanel1.add(jButtonAbbrechen);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanelRes, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 547, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanelRes, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAbbrechen;
    private javax.swing.JButton jButtonOk;
    private javax.swing.JPanel jPanelExtra;
    private javax.swing.JPanel jPanelRes;
    private javax.swing.JRadioButton jRadioButtonResHd;
    private javax.swing.JRadioButton jRadioButtonResHi;
    private javax.swing.JRadioButton jRadioButtonResLo;
    // End of variables declaration//GEN-END:variables
}
