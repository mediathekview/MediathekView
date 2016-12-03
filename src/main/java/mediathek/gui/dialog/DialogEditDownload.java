/*    
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *    
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.gui.dialog;

import mSearch.daten.DatenFilm;
import mSearch.tool.Log;
import mSearch.tool.SysMsg;
import mediathek.config.Icons;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenProg;
import mediathek.file.GetFile;
import mediathek.tool.EscBeenden;
import mediathek.tool.MVMessageDialog;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

@SuppressWarnings("serial")
public class DialogEditDownload extends JDialog {
    private final DatenDownload datenDownload;
    public boolean ok = false;
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
    private JFrame parent = null;
    private String orgProgArray = "";
    private String resolution = DatenFilm.AUFLOESUNG_NORMAL;
    private final JLabel jLabelFilmHD = new JLabel();
    private final JLabel jLabelFilmUT = new JLabel();
    private static ImageIcon ja_sw_16 = null;

    public DialogEditDownload(JFrame parent, boolean modal, DatenDownload ddownload, boolean ggestartet) {
        super(parent, modal);
        initComponents();
        this.parent = parent;
        datenDownload = ddownload;
        gestartet = ggestartet;
        jScrollPane1.getVerticalScrollBar().setUnitIncrement(16);
        ja_sw_16 = Icons.ICON_DIALOG_EIN_SW;

        orgProgArray = datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY];
        mVPanelDownloadZiel = new MVPanelDownloadZiel(parent, datenDownload, false);
        mVPanelDownloadZiel.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(204, 204, 204)));
        jRadioButtonResHd.addActionListener(e -> changeRes());
        jRadioButtonResHi.addActionListener(e -> changeRes());
        jRadioButtonResLo.addActionListener(e -> changeRes());
        jButtonOk.addActionListener(e -> {
            if (check()) {
                beenden();
            }
        });
        jButtonAbbrechen.addActionListener(e -> beenden());
        getRootPane().setDefaultButton(jButtonOk);
        new EscBeenden(this) {
            @Override
            public void beenden_() {
                beenden();
            }
        };
        setupResolutionButtons();
        setExtra();
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
            jRadioButtonResHi.setSelected(datenDownload.arr[DatenDownload.DOWNLOAD_URL].equals(datenDownload.film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_NORMAL)));
            dateiGroesse_Hoch = datenDownload.film.getDateigroesse(datenDownload.film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_NORMAL));
            if (!dateiGroesse_Hoch.isEmpty()) {
                jRadioButtonResHi.setText(jRadioButtonResHi.getText() + "   [ " + dateiGroesse_Hoch + " MB ]");
            }

            if (!datenDownload.film.arr[DatenFilm.FILM_URL_HD].isEmpty()) {
                jRadioButtonResHd.setEnabled(!gestartet);
                jRadioButtonResHd.setSelected(datenDownload.arr[DatenDownload.DOWNLOAD_URL].equals(datenDownload.film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_HD)));
                dateiGroesse_HD = datenDownload.film.getDateigroesse(datenDownload.film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_HD));
                if (!dateiGroesse_HD.isEmpty()) {
                    jRadioButtonResHd.setText(jRadioButtonResHd.getText() + "   [ " + dateiGroesse_HD + " MB ]");
                }
            }

            if (!datenDownload.film.arr[DatenFilm.FILM_URL_KLEIN].isEmpty()) {
                jRadioButtonResLo.setEnabled(!gestartet);
                jRadioButtonResLo.setSelected(datenDownload.arr[DatenDownload.DOWNLOAD_URL].equals(datenDownload.film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_KLEIN)));
                dateiGroesse_Klein = datenDownload.film.getDateigroesse(datenDownload.film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_KLEIN));
                if (!dateiGroesse_Klein.isEmpty()) {
                    jRadioButtonResLo.setText(jRadioButtonResLo.getText() + "   [ " + dateiGroesse_Klein + " MB ]");
                }
            }

        }
        if (jRadioButtonResHd.isSelected()) {
            resolution = DatenFilm.AUFLOESUNG_HD;
        } else if (jRadioButtonResLo.isSelected()) {
            resolution = DatenFilm.AUFLOESUNG_KLEIN;
        } else {
            resolution = DatenFilm.AUFLOESUNG_NORMAL;
        }
    }

    private void changeRes() {
        // RadioButton sind nur enabled wenn "datenDownload.film" vorhanden
        final String res;
        if (jRadioButtonResHd.isSelected()) {
            res = DatenFilm.AUFLOESUNG_HD;
        } else if (jRadioButtonResLo.isSelected()) {
            res = DatenFilm.AUFLOESUNG_KLEIN;
        } else {
            res = DatenFilm.AUFLOESUNG_NORMAL;
        }
        datenDownload.arr[DatenDownload.DOWNLOAD_URL] = datenDownload.film.getUrlFuerAufloesung(res);
        datenDownload.arr[DatenDownload.DOWNLOAD_URL_RTMP] = datenDownload.film.getUrlRtmpFuerAufloesung(res);
        textfeldListe[DatenDownload.DOWNLOAD_URL].setText(datenDownload.arr[DatenDownload.DOWNLOAD_URL]);
        textfeldListe[DatenDownload.DOWNLOAD_URL_RTMP].setText(datenDownload.arr[DatenDownload.DOWNLOAD_URL_RTMP]);

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
                    datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD], res);

            datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF] = d.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF];
            datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY] = d.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY];
            textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF].setText(datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF]);
            textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY].setText(datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY]);
        }
        datenDownload.setGroesse(size);
    }

    private void setExtra() {
        jPanelExtra.removeAll();
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(5, 10, 10, 5);

        jPanelExtra.setLayout(gridbag);
        int zeile = 0;
        for (int i = 0; i < DatenDownload.MAX_ELEM; ++i) {
            JLabel label = new JLabel("  " + DatenDownload.COLUMN_NAMES[i] + ": ");
            labelListe[i] = label;
            JTextField textfeld = new JTextField();
            textfeld.setEditable(false);
            textfeld.setText(datenDownload.arr[i]);
            textfeldListe[i] = textfeld;
            addExtraFeld(i, gridbag, c);
            ++zeile;
            c.gridy = zeile;
        }
        jPanelExtra.validate();
    }

    private void addExtraFeld(int i, GridBagLayout gridbag, GridBagConstraints c) {
        //Label
        c.gridx = 0;
        c.weightx = 0;
        if (i == DatenDownload.DOWNLOAD_ZURUECKGESTELLT
                || i == DatenDownload.DOWNLOAD_URL_RTMP
                || i == DatenDownload.DOWNLOAD_BUTTON_DEL || i == DatenDownload.DOWNLOAD_BUTTON_START
                || i == DatenDownload.DOWNLOAD_REF
                || i == DatenDownload.DOWNLOAD_BANDBREITE) {
            // ist eigentlich Unsinn, es anzuzeigen
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
                    labelListe[i].setForeground(Color.BLUE);
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
                    labelListe[i].setForeground(Color.BLUE);
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
                    labelListe[i].setForeground(Color.BLUE);
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
                    labelListe[i].setForeground(Color.BLUE);
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
                    labelListe[i].setForeground(Color.BLUE);
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
                    jLabelFilmHD.setOpaque(false);
                    jLabelFilmHD.setIcon(ja_sw_16);
                    gridbag.setConstraints(labelListe[i], c);
                    jPanelExtra.add(labelListe[i]);
                    c.gridx = 1;
                    c.weightx = 10;
                    gridbag.setConstraints(jLabelFilmHD, c);
                    jPanelExtra.add(jLabelFilmHD);
                    if (datenDownload.film != null) {
                        jLabelFilmHD.setVisible(datenDownload.film.isHD());
                    } else {
                        jLabelFilmHD.setVisible(false);
                    }
                    break;
                case DatenDownload.DOWNLOAD_UT:
                    jLabelFilmUT.setOpaque(false);
                    jLabelFilmUT.setIcon(ja_sw_16);
                    gridbag.setConstraints(labelListe[i], c);
                    jPanelExtra.add(labelListe[i]);
                    c.gridx = 1;
                    c.weightx = 10;
                    gridbag.setConstraints(jLabelFilmUT, c);
                    jPanelExtra.add(jLabelFilmUT);
                    if (datenDownload.film != null) {
                        jLabelFilmUT.setVisible(datenDownload.film.hasUT());
                    } else {
                        jLabelFilmUT.setVisible(false);
                    }
                    break;
                case DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF:
                    break;
                case DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY:
                    if (datenDownload.art == DatenDownload.ART_PROGRAMM) {
                        // nur bei Downloads über ein Programm
                        if (datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY].isEmpty()) {
                            // Aufruf über Array ist leer -> Win, Mac
                            labelListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY].setForeground(Color.BLUE);
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
                            labelListe[i].setForeground(Color.BLUE);
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
                            jButtonReset.setIcon(Icons.ICON_BUTTON_AKTUALISIEREN);
                            jButtonReset.addActionListener(e -> textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY].setText(orgProgArray));
                            JButton jButtonHelp = new JButton("");
                            jButtonHelp.setIcon(Icons.ICON_BUTTON_HELP);
                            jButtonHelp.setToolTipText("Hilfe anzeigen");
                            jButtonHelp.addActionListener(e -> new DialogHilfe(parent, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_EDIT_DOWNLOAD_PROG)).setVisible(true));

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
                        case DatenDownload.DOWNLOAD_NR:
                            textfeldListe[i].setText(datenDownload.nr + "");
                            break;
                        case DatenDownload.DOWNLOAD_FILM_NR:
                            if (datenDownload.film != null) {
                                textfeldListe[i].setText(datenDownload.film.nr + "");
                            }
                            break;
                        case DatenDownload.DOWNLOAD_URL:
                            if (datenDownload.art == DatenDownload.ART_DOWNLOAD) {
                                // nur bei direkten Downloads
                                labelListe[i].setForeground(Color.BLUE);
                                textfeldListe[i].setEditable(!gestartet);// und wenn noch nicht gestartet
                                textfeldListe[i].getDocument().addDocumentListener(new BeobachterDocumentTextfeld(i));
                            }
                            break;
                        case DatenDownload.DOWNLOAD_GROESSE:
                            textfeldListe[i].setText(datenDownload.mVFilmSize.toString());
                            break;
                        case DatenDownload.DOWNLOAD_PROGRESS:
                            textfeldListe[i].setText(Start.getTextProgress(datenDownload.isDownloadManager(), datenDownload.start));
                            break;
                        case DatenDownload.DOWNLOAD_RESTZEIT:
                            textfeldListe[i].setText(datenDownload.getTextRestzeit());
                            break;
                        case DatenDownload.DOWNLOAD_ART:
                            switch (datenDownload.art) {
                                case DatenDownload.ART_DOWNLOAD:
                                    textfeldListe[i].setText(DatenDownload.ART_DOWNLOAD_TXT);
                                    break;
                                case DatenDownload.ART_PROGRAMM:
                                    textfeldListe[i].setText(DatenDownload.ART_PROGRAMM_TXT);
                                    break;
                            }
                            break;
                        case DatenDownload.DOWNLOAD_QUELLE:
                            switch (datenDownload.quelle) {
                                case DatenDownload.QUELLE_ALLE:
                                    textfeldListe[i].setText(DatenDownload.QUELLE_ALLE_TXT);
                                    break;
                                case DatenDownload.QUELLE_ABO:
                                    textfeldListe[i].setText(DatenDownload.QUELLE_ABO_TXT);
                                    break;
                                case DatenDownload.QUELLE_BUTTON:
                                    textfeldListe[i].setText(DatenDownload.QUELLE_BUTTON_TXT);
                                    break;
                                case DatenDownload.QUELLE_DOWNLOAD:
                                    textfeldListe[i].setText(DatenDownload.QUELLE_DOWNLOAD_TXT);
                                    break;
                            }
                            break;
                        default:
                            break;
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
            SysMsg.sysMsg(new String[]{"Datei löschen: ", file.getAbsolutePath()});
            if (!file.delete()) {
                throw new Exception();
            }
        } catch (Exception ex) {
            MVMessageDialog.showMessageDialog(parent, "Konnte die Datei nicht löschen!", "Film löschen", JOptionPane.ERROR_MESSAGE);
            Log.errorLog(812036789, "Fehler beim löschen: " + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        }
        return true;
    }

    private boolean check() {
        mVPanelDownloadZiel.setPfadName_geaendert();
        if ((jRadioButtonResHd.isSelected() && !resolution.equals(DatenFilm.AUFLOESUNG_HD))
                || (jRadioButtonResLo.isSelected() && !resolution.equals(DatenFilm.AUFLOESUNG_KLEIN))
                || (jRadioButtonResHi.isSelected() && !resolution.equals(DatenFilm.AUFLOESUNG_NORMAL))) {
            // dann wurde die Auflösung geändert -> Film kann nicht weitergeführt werden
            ok = downloadDateiLoeschen(datenDownload);
        } else {
            ok = true;
        }
        return ok;
    }

    private void beenden() {
        this.dispose();
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.ButtonGroup buttonGroup1 = new javax.swing.ButtonGroup();
        jScrollPane1 = new javax.swing.JScrollPane();
        jPanelExtra = new javax.swing.JPanel();
        jButtonOk = new javax.swing.JButton();
        jButtonAbbrechen = new javax.swing.JButton();
        jPanelRes = new javax.swing.JPanel();
        javax.swing.JLabel jLabelRes = new javax.swing.JLabel();
        jRadioButtonResHd = new javax.swing.JRadioButton();
        jRadioButtonResHi = new javax.swing.JRadioButton();
        jRadioButtonResLo = new javax.swing.JRadioButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Download editieren");
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

        jButtonOk.setText("Ok");

        jButtonAbbrechen.setText("Abbrechen");

        jPanelRes.setBorder(javax.swing.BorderFactory.createTitledBorder(""));

        jLabelRes.setText("Auflösung ändern:");

        buttonGroup1.add(jRadioButtonResHd);
        jRadioButtonResHd.setText("HD");

        buttonGroup1.add(jRadioButtonResHi);
        jRadioButtonResHi.setText("hoher Auflösung");

        buttonGroup1.add(jRadioButtonResLo);
        jRadioButtonResLo.setText("niedriger Auflösung");

        javax.swing.GroupLayout jPanelResLayout = new javax.swing.GroupLayout(jPanelRes);
        jPanelRes.setLayout(jPanelResLayout);
        jPanelResLayout.setHorizontalGroup(
            jPanelResLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelResLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabelRes)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jRadioButtonResHd)
                .addGap(18, 18, 18)
                .addComponent(jRadioButtonResHi)
                .addGap(18, 18, 18)
                .addComponent(jRadioButtonResLo)
                .addContainerGap(237, Short.MAX_VALUE))
        );
        jPanelResLayout.setVerticalGroup(
            jPanelResLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelResLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelResLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabelRes)
                    .addComponent(jRadioButtonResHd)
                    .addComponent(jRadioButtonResHi)
                    .addComponent(jRadioButtonResLo))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

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
                        .addComponent(jButtonOk, javax.swing.GroupLayout.PREFERRED_SIZE, 93, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jButtonAbbrechen)))
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonAbbrechen, jButtonOk});

        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 577, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanelRes, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonOk)
                    .addComponent(jButtonAbbrechen))
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
    private javax.swing.JScrollPane jScrollPane1;
    // End of variables declaration//GEN-END:variables

    private class BeobachterDocumentTextfeld implements DocumentListener {

        int nr;

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
}
