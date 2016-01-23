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

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import mediathek.controller.Log;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenProg;
import mediathek.file.GetFile;
import mediathek.res.GetIcon;
import mediathek.tool.EscBeenden;
import mediathek.tool.MVMessageDialog;
import msearch.daten.DatenFilm;

public class DialogEditDownload extends javax.swing.JDialog {

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

    public DialogEditDownload(JFrame parent, boolean modal, DatenDownload ddownload, boolean ggestartet) {
        super(parent, modal);
        initComponents();
        this.parent = parent;
        datenDownload = ddownload;
        gestartet = ggestartet;
        jScrollPane1.getVerticalScrollBar().setUnitIncrement(16);

        orgProgArray = datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY_NR];
        mVPanelDownloadZiel = new MVPanelDownloadZiel(parent, datenDownload, false);
        mVPanelDownloadZiel.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(204, 204, 204)));
        jRadioButtonResHd.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                changeRes();
            }
        });
        jRadioButtonResHi.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                changeRes();
            }
        });
        jRadioButtonResLo.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                changeRes();
            }
        });
        jButtonOk.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (check()) {
                    beenden();
                }
            }
        });
        jButtonAbbrechen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                beenden();
            }
        });
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
            jRadioButtonResHi.setSelected(datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR].equals(datenDownload.film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_NORMAL)));
            dateiGroesse_Hoch = datenDownload.film.getDateigroesse(datenDownload.film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_NORMAL));
            if (!dateiGroesse_Hoch.isEmpty()) {
                jRadioButtonResHi.setText(jRadioButtonResHi.getText() + "   [ " + dateiGroesse_Hoch + " MB ]");
            }

            if (!datenDownload.film.arr[DatenFilm.FILM_URL_HD_NR].isEmpty()) {
                jRadioButtonResHd.setEnabled(!gestartet);
                jRadioButtonResHd.setSelected(datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR].equals(datenDownload.film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_HD)));
                dateiGroesse_HD = datenDownload.film.getDateigroesse(datenDownload.film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_HD));
                if (!dateiGroesse_HD.isEmpty()) {
                    jRadioButtonResHd.setText(jRadioButtonResHd.getText() + "   [ " + dateiGroesse_HD + " MB ]");
                }
            }

            if (!datenDownload.film.arr[DatenFilm.FILM_URL_KLEIN_NR].isEmpty()) {
                jRadioButtonResLo.setEnabled(!gestartet);
                jRadioButtonResLo.setSelected(datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR].equals(datenDownload.film.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_KLEIN)));
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
        datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR] = datenDownload.film.getUrlFuerAufloesung(res);
        datenDownload.arr[DatenDownload.DOWNLOAD_URL_RTMP_NR] = datenDownload.film.getUrlRtmpFuerAufloesung(res);
        textfeldListe[DatenDownload.DOWNLOAD_URL_NR].setText(datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR]);
        textfeldListe[DatenDownload.DOWNLOAD_URL_RTMP_NR].setText(datenDownload.arr[DatenDownload.DOWNLOAD_URL_RTMP_NR]);

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
                    datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME_NR],
                    datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR], res);

            datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR] = d.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR];
            datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY_NR] = d.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY_NR];
            textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR].setText(datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR]);
            textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY_NR].setText(datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY_NR]);
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
        if (i == DatenDownload.DOWNLOAD_ZURUECKGESTELLT_NR
                || i == DatenDownload.DOWNLOAD_URL_RTMP_NR
                || i == DatenDownload.DOWNLOAD_BUTTON_DEL_NR || i == DatenDownload.DOWNLOAD_BUTTON_START_NR
                || i == DatenDownload.DOWNLOAD_REF_NR
                || i == DatenDownload.DOWNLOAD_BANDBREITE_NR) {
            // ist eigentlich Unsinn, es anzuzeigen
            return;
        }
        if (datenDownload.art == DatenDownload.ART_DOWNLOAD
                && (i == DatenDownload.DOWNLOAD_ZIEL_DATEINAME_NR
                || i == DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR
                || i == DatenDownload.DOWNLOAD_ZIEL_PFAD_NR)
                && !gestartet) {
            // Downloadpfad anpassen
            if (i == DatenDownload.DOWNLOAD_ZIEL_DATEINAME_NR) {
                c.gridx = 1;
                c.weightx = 10;
                gridbag.setConstraints(mVPanelDownloadZiel, c);
                jPanelExtra.add(mVPanelDownloadZiel);
            }
        } else {
            if (i == DatenDownload.DOWNLOAD_PROGRAMM_RESTART_NR) {
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
            } else if (i == DatenDownload.DOWNLOAD_PROGRAMM_DOWNLOADMANAGER_NR) {
                jCheckBoxDownloadmanager.setSelected(datenDownload.isDownloadManager());
                // jCheckBoxRestart.addActionListener(new BeobCheckbox());
                jCheckBoxDownloadmanager.setEnabled(false);
                gridbag.setConstraints(labelListe[i], c);
                jPanelExtra.add(labelListe[i]);
                c.gridx = 1;
                c.weightx = 10;
                gridbag.setConstraints(jCheckBoxDownloadmanager, c);
                jPanelExtra.add(jCheckBoxDownloadmanager);
            } else if (i == DatenDownload.DOWNLOAD_UNTERBROCHEN_NR) {
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
            } else if (i == DatenDownload.DOWNLOAD_INFODATEI_NR) {
                labelListe[i].setForeground(Color.BLUE);
                jCheckBoxInfodatei.setSelected(Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI_NR]));
                jCheckBoxInfodatei.addActionListener(new BeobCheckbox());
                jCheckBoxInfodatei.setEnabled(!gestartet);
                gridbag.setConstraints(labelListe[i], c);
                jPanelExtra.add(labelListe[i]);
                c.gridx = 1;
                c.weightx = 10;
                gridbag.setConstraints(jCheckBoxInfodatei, c);
                jPanelExtra.add(jCheckBoxInfodatei);
            } else if (i == DatenDownload.DOWNLOAD_SUBTITLE_NR) {
                labelListe[i].setForeground(Color.BLUE);
                jCheckBoxSubtitle.setSelected(Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_SUBTITLE_NR]));
                jCheckBoxSubtitle.addActionListener(new BeobCheckbox());
                jCheckBoxSubtitle.setEnabled(!gestartet);
                gridbag.setConstraints(labelListe[i], c);
                jPanelExtra.add(labelListe[i]);
                c.gridx = 1;
                c.weightx = 10;
                gridbag.setConstraints(jCheckBoxSubtitle, c);
                jPanelExtra.add(jCheckBoxSubtitle);
            } else if (i == DatenDownload.DOWNLOAD_SPOTLIGHT_NR) {
                labelListe[i].setForeground(Color.BLUE);
                jCheckBoxSpotlight.setSelected(Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_SPOTLIGHT_NR]));
                jCheckBoxSpotlight.addActionListener(new BeobCheckbox());
                jCheckBoxSpotlight.setEnabled(!gestartet);
                gridbag.setConstraints(labelListe[i], c);
                jPanelExtra.add(labelListe[i]);
                c.gridx = 1;
                c.weightx = 10;
                gridbag.setConstraints(jCheckBoxSpotlight, c);
                jPanelExtra.add(jCheckBoxSpotlight);
            } else if (i == DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR) {
            } else if (i == DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY_NR) {
                if (datenDownload.art == DatenDownload.ART_PROGRAMM) {
                    // nur bei Downloads über ein Programm
                    if (datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY_NR].isEmpty()) {
                        // Aufruf über Array ist leer -> Win, Mac
                        labelListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY_NR].setForeground(Color.BLUE);
                        textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR].setEditable(!gestartet);// und wenn noch nicht gestartet
                        textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR].getDocument().addDocumentListener(new BeobachterDocumentTextfeld(DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR));
                        gridbag.setConstraints(labelListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY_NR], c);
                        jPanelExtra.add(labelListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY_NR]);
                        c.gridx = 1;
                        c.weightx = 10;
                        gridbag.setConstraints(textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR], c);
                        jPanelExtra.add(textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR]);
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
                        jButtonReset.setIcon(GetIcon.getProgramIcon("view-refresh_16.png"));
                        jButtonReset.addActionListener(new ActionListener() {
                            @Override
                            public void actionPerformed(ActionEvent e) {
                                textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY_NR].setText(orgProgArray);
                            }
                        });
                        JButton jButtonHelp = new JButton("");
                        jButtonHelp.setToolTipText("Hilfe");
                        jButtonHelp.setIcon(GetIcon.getProgramIcon("help_16.png"));
                        jButtonHelp.addActionListener(new ActionListener() {
                            @Override
                            public void actionPerformed(ActionEvent e) {
                                new DialogHilfe(parent, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_EDIT_DOWNLOAD_PROG)).setVisible(true);
                            }
                        });

                        gc.gridy = 0;
                        gc.gridx = 0;
                        gc.weightx = 1;
                        gb.setConstraints(jButtonHelp, gc);
                        jp.add(jButtonHelp);
                        gc.gridx = 1;
                        gc.weightx = 10;
                        gb.setConstraints(textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR], gc);
                        jp.add(textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR]);

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
            } else {
                if (i == DatenDownload.DOWNLOAD_NR_NR) {
                    textfeldListe[i].setText(datenDownload.nr + "");
                } else if (i == DatenDownload.DOWNLOAD_FILM_NR_NR) {
                    if (datenDownload.film != null) {
                        textfeldListe[i].setText(datenDownload.film.nr + "");
                    }
                } else if (i == DatenDownload.DOWNLOAD_URL_NR) {
                    if (datenDownload.art == DatenDownload.ART_DOWNLOAD) {
                        // nur bei direkten Downloads
                        labelListe[i].setForeground(Color.BLUE);
                        textfeldListe[i].setEditable(!gestartet);// und wenn noch nicht gestartet
                        textfeldListe[i].getDocument().addDocumentListener(new BeobachterDocumentTextfeld(i));
                    }
                } else if (i == DatenDownload.DOWNLOAD_GROESSE_NR) {
                    textfeldListe[i].setText(datenDownload.mVFilmSize.toString());
                } else if (i == DatenDownload.DOWNLOAD_PROGRESS_NR) {
                    textfeldListe[i].setText(Start.getTextProgress(datenDownload));
                } else if (i == DatenDownload.DOWNLOAD_RESTZEIT_NR) {
                    textfeldListe[i].setText(datenDownload.getTextRestzeit());
                } else if (i == DatenDownload.DOWNLOAD_ART_NR) {
                    switch (datenDownload.art) {
                        case DatenDownload.ART_DOWNLOAD:
                            textfeldListe[i].setText(DatenDownload.ART_DOWNLOAD_TXT);
                            break;
                        case DatenDownload.ART_PROGRAMM:
                            textfeldListe[i].setText(DatenDownload.ART_PROGRAMM_TXT);
                            break;
                    }
                } else if (i == DatenDownload.DOWNLOAD_QUELLE_NR) {
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
                }
                gridbag.setConstraints(labelListe[i], c);
                jPanelExtra.add(labelListe[i]);
                //Textfeld
                c.gridx = 1;
                c.weightx = 10;
                gridbag.setConstraints(textfeldListe[i], c);
                jPanelExtra.add(textfeldListe[i]);
            }
        }
    }

    private boolean downloadDateiLoeschen(DatenDownload datenDownload) {
        try {
            File file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
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
            Log.systemMeldung(new String[]{"Datei löschen: ", file.getAbsolutePath()});
            if (!file.delete()) {
                throw new Exception();
            }
        } catch (Exception ex) {
            MVMessageDialog.showMessageDialog(parent, "Konnte die Datei nicht löschen!", "Film löschen", JOptionPane.ERROR_MESSAGE);
            Log.fehlerMeldung(812036789, "Fehler beim löschen: " + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
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

        buttonGroup1 = new javax.swing.ButtonGroup();
        jScrollPane1 = new javax.swing.JScrollPane();
        jPanelExtra = new javax.swing.JPanel();
        jButtonOk = new javax.swing.JButton();
        jButtonAbbrechen = new javax.swing.JButton();
        jPanelRes = new javax.swing.JPanel();
        jLabelRes = new javax.swing.JLabel();
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
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JButton jButtonAbbrechen;
    private javax.swing.JButton jButtonOk;
    private javax.swing.JLabel jLabelRes;
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
            if (nr == DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY_NR) {
                datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR] = DatenProg.makeProgAufrufArray(datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY_NR]);
                textfeldListe[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR].setText(datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR]);
            }
        }
    }

    private class BeobCheckbox implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_RESTART_NR] = Boolean.toString(jCheckBoxRestart.isSelected());
            datenDownload.arr[DatenDownload.DOWNLOAD_UNTERBROCHEN_NR] = Boolean.toString(jCheckBoxUnterbrochen.isSelected());
            datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI_NR] = Boolean.toString(jCheckBoxInfodatei.isSelected());
            datenDownload.arr[DatenDownload.DOWNLOAD_SUBTITLE_NR] = Boolean.toString(jCheckBoxSubtitle.isSelected());
            datenDownload.arr[DatenDownload.DOWNLOAD_SPOTLIGHT_NR] = Boolean.toString(jCheckBoxSpotlight.isSelected());
        }
    }
}
