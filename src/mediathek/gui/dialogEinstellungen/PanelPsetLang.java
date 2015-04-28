/*    
 *    MediathekView
 *    Copyright (C) 2012   W. Xaver
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
package mediathek.gui.dialogEinstellungen;

import com.jidesoft.utils.SystemInfo;
import java.awt.Color;
import java.awt.FileDialog;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.net.URISyntaxException;
import java.util.LinkedList;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import mediathek.controller.IoXmlSchreiben;
import mediathek.controller.Log;
import mediathek.daten.Daten;
import mediathek.daten.DatenProg;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.file.GetFile;
import mediathek.gui.PanelVorlage;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.res.GetIcon;
import mediathek.tool.CellRendererProgramme;
import mediathek.tool.CellRendererPset;
import mediathek.tool.FilenameUtils;
import mediathek.tool.GuiFunktionenProgramme;
import mediathek.tool.HinweisKeineAuswahl;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVColor;
import mediathek.tool.MVTable;
import mediathek.tool.TModel;
import mediathek.tool.UrlHyperlinkAction;
import msearch.daten.DatenFilm;

public class PanelPsetLang extends PanelVorlage {

    private int neuZaehler = 0;
    private String exportPfad = "";
    private ListePset listePset;
    private MVTable tabellePset;
    private MVTable tabelleProgramme;
    private boolean modalHilfe = false;

    public PanelPsetLang(Daten d, JFrame parentComponent) {
        super(d, parentComponent);
        initComponents();
        modalHilfe = false;
        tabellePset = new MVTable(MVTable.TABELLE_TAB_PSET);
        jScrollPane3.setViewportView(tabellePset);
        tabelleProgramme = new MVTable(MVTable.TABELLE_TAB_PROG);
        jScrollPane1.setViewportView(tabelleProgramme);
        listePset = Daten.listePset;
        init();
    }

    public PanelPsetLang(Daten d, JFrame parentComponent, ListePset llistePset) {
        super(d, parentComponent);
        initComponents();
        modalHilfe = true;
        tabellePset = new MVTable(MVTable.TABELLE_TAB_PSET);
        jScrollPane3.setViewportView(tabellePset);
        tabelleProgramme = new MVTable(MVTable.TABELLE_TAB_PROG);
        jScrollPane1.setViewportView(tabelleProgramme);
        listePset = llistePset;
        init();
    }

    private void init() {
        jButtonHilfe.setIcon(GetIcon.getProgramIcon("help_16.png"));
        jButtonGruppePfad.setIcon(GetIcon.getProgramIcon("fileopen_16.png"));
        jButtonProgPlus.setIcon(GetIcon.getProgramIcon("add_16.png"));
        jButtonProgMinus.setIcon(GetIcon.getProgramIcon("remove_16.png"));
        jButtonProgAuf.setIcon(GetIcon.getProgramIcon("move_up_16.png"));
        jButtonProgAb.setIcon(GetIcon.getProgramIcon("move_down_16.png"));
        jButtonProgPfad.setIcon(GetIcon.getProgramIcon("fileopen_16.png"));
        jButtonGruppeNeu.setIcon(GetIcon.getProgramIcon("add_16.png"));
        jButtonGruppeLoeschen.setIcon(GetIcon.getProgramIcon("remove_16.png"));
        jButtonGruppeAuf.setIcon(GetIcon.getProgramIcon("move_up_16.png"));
        jButtonGruppeAb.setIcon(GetIcon.getProgramIcon("move_down_16.png"));
        jLabelMeldungAbspielen.setIcon(GetIcon.getProgramIcon("alert_16.png"));
        jLabelMeldungSeichern.setIcon(GetIcon.getProgramIcon("alert_16.png"));
        //Programme
        tabellePset.setAutoResizeMode(MVTable.AUTO_RESIZE_OFF);
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_LISTE_PSET, PanelPsetLang.class.getSimpleName()) {
            @Override
            public void ping() {
                if (!stopBeob) {
                    tabellePset();
                }
            }
        });
        BeobProgDoc beobDoc = new BeobProgDoc();
        jTextFieldProgPfad.getDocument().addDocumentListener(beobDoc);
        jTextFieldProgSchalter.getDocument().addDocumentListener(beobDoc);
        jTextFieldProgName.getDocument().addDocumentListener(beobDoc);
        jTextFieldProgZielDateiName.getDocument().addDocumentListener(beobDoc);
        jTextFieldProgPraefix.getDocument().addDocumentListener(beobDoc);
        jTextFieldProgSuffix.getDocument().addDocumentListener(beobDoc);
        jTextFieldProgPfad.setEnabled(false);
        jTextFieldProgSchalter.setEnabled(false);
        jTextFieldProgName.setEnabled(false);
        jTextFieldProgZielDateiName.setEnabled(false);
        jTextFieldProgPraefix.setEnabled(false);
        jTextFieldProgSuffix.setEnabled(false);
        jButtonProgPfad.addActionListener(new BeobDateiDialogProg());
        jButtonProgPlus.addActionListener(new BeobProgNeueZeile());
        jButtonProgMinus.addActionListener(new BeobProgLoeschen());
        jButtonProgDuplizieren.addActionListener(new BeobProgDuplizieren());
        jButtonProgAuf.addActionListener(new BeobProgAufAb(true));
        jButtonProgAb.addActionListener(new BeobProgAufAb(false));
        jButtonProgPfad.setEnabled(false);
        jCheckBoxRestart.addActionListener(new BeobProgRestart());
        //Pset
        jButtonAbspielen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                jButtonAbspielen.setBackground(MVColor.BUTTON_SET_ABSPIELEN.color);
                DatenPset pset = getPset();
                if (pset != null) {
                    pset.setAbspielen(daten);
                    nurtabellePset();
                    notifyPset();
                }
            }
        });
        jCheckBoxSpeichern.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                DatenPset pset = getPset();
                if (pset != null) {
                    pset.setSpeichern(jCheckBoxSpeichern.isSelected());
                    nurtabellePset();
                    notifyPset();
                }
            }
        });
        jCheckBoxButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                DatenPset pset = getPset();
                if (pset != null) {
                    pset.setButton(jCheckBoxButton.isSelected());
                    nurtabellePset();
                    notifyPset();
                }
            }
        });
        jCheckBoxAbo.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                DatenPset pset = getPset();
                if (pset != null) {
                    pset.setAbo(jCheckBoxAbo.isSelected());
                    nurtabellePset();
                    notifyPset();
                }
            }
        });
        jCheckBoxLaenge.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                DatenPset pset = getPset();
                if (pset != null) {
                    pset.arr[DatenPset.PROGRAMMSET_LAENGE_BESCHRAENKEN_NR] = Boolean.toString(jCheckBoxLaenge.isSelected());
                    nurtabellePset();
                }
            }
        });
        jCheckBoxThema.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                DatenPset pset = getPset();
                if (pset != null) {
                    pset.arr[DatenPset.PROGRAMMSET_THEMA_ANLEGEN_NR] = Boolean.toString(jCheckBoxThema.isSelected());
                    nurtabellePset();
                }
            }
        });
        jSpinnerLaenge.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                DatenPset pset = getPset();
                if (pset != null) {
                    pset.arr[DatenPset.PROGRAMMSET_MAX_LAENGE_NR] = String.valueOf(((Number) jSpinnerLaenge.getModel().getValue()).intValue());
                }
            }
        });
        jCheckBoxInfodatei.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                DatenPset pset = getPset();
                if (pset != null) {
                    pset.arr[DatenPset.PROGRAMMSET_INFODATEI_NR] = Boolean.toString(jCheckBoxInfodatei.isSelected());
                    nurtabellePset();
                }
            }
        });
        jCheckBoxSubtitle.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                DatenPset pset = getPset();
                if (pset != null) {
                    pset.arr[DatenPset.PROGRAMMSET_SUBTITLE_NR] = Boolean.toString(jCheckBoxSubtitle.isSelected());
                    nurtabellePset();
                }
            }
        });
        jCheckBoxSpotlight.setEnabled(SystemInfo.isMacOSX() || Daten.debug);
        jCheckBoxSpotlight.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                DatenPset pset = getPset();
                if (pset != null) {
                    pset.arr[DatenPset.PROGRAMMSET_SPOTLIGHT_NR] = Boolean.toString(jCheckBoxSpotlight.isSelected());
                    nurtabellePset();
                }
            }
        });
        jButtonGruppeNeu.addActionListener(new BeobGruppeNeu());
        jButtonGruppeLoeschen.addActionListener(new BeobGruppeLoeschen());
        jButtonGruppeFarbe.addActionListener(new BeobachterFarbe());
        jButtonGruppeStandardfarbe.addActionListener(new BeobStandardfarbe());
        jButtonGruppeAuf.addActionListener(new BeobGruppeAufAb(true));
        jButtonGruppeAb.addActionListener(new BeobGruppeAufAb(false));
        jButtonGruppeDuplizieren.addActionListener(new BeobGruppeDuplizieren());
        jButtonExport.addActionListener(new BeobGruppeExport());
        jButtonGruppePfad.addActionListener(new BeobDateiDialogPfad());
        jTextFieldSetName.getDocument().addDocumentListener(new BeobDoc(jTextFieldSetName, DatenPset.PROGRAMMSET_NAME_NR));
        jTextAreaSetBeschreibung.getDocument().addDocumentListener(new BeobDoc(jTextAreaSetBeschreibung, DatenPset.PROGRAMMSET_BESCHREIBUNG_NR));
        jTextFieldGruppeDirektSuffix.getDocument().addDocumentListener(
                new BeobDoc(jTextFieldGruppeDirektSuffix, DatenPset.PROGRAMMSET_SUFFIX_DIREKT_NR));
        jTextFieldGruppeDirektPraefix.getDocument().addDocumentListener(
                new BeobDoc(jTextFieldGruppeDirektPraefix, DatenPset.PROGRAMMSET_PRAEFIX_DIREKT_NR));
        jTextFieldGruppeZielName.getDocument().addDocumentListener(new BeobDoc(jTextFieldGruppeZielName,
                DatenPset.PROGRAMMSET_ZIEL_DATEINAME_NR));
        jTextFieldGruppeZielPfad.getDocument().addDocumentListener(
                new BeobDoc(jTextFieldGruppeZielPfad, DatenPset.PROGRAMMSET_ZIEL_PFAD_NR));
        //rest
        jButtonHilfe.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                new DialogHilfe(parentComponent, modalHilfe, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_PRGRAMME)).setVisible(true);
            }
        });
        jRadioButtonAufloesungKlein.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setAufloesung();
            }
        });
        jRadioButtonAufloesungNormal.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setAufloesung();
            }
        });
        jRadioButtonAufloesungHD.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setAufloesung();
            }
        });
        jButtonPruefen.addActionListener(new BeobPuefen());
        tabelleProgramme.getSelectionModel().addListSelectionListener(new BeobTableSelect());
        tabelleProgramme.setDefaultRenderer(Object.class, new CellRendererProgramme(daten));
        tabellePset.setDefaultRenderer(Object.class, new CellRendererPset(daten));
        tabellePset.getSelectionModel().addListSelectionListener(new BeobTableSelectPset());
        tabellePset();
//        spaltenSetzen();
        if (tabellePset.getRowCount() > 0) {
            tabellePset.setRowSelectionInterval(0, 0);
            tabellePset.scrollRectToVisible(tabellePset.getCellRect(0, 0, false));
        }
        jXHyperlinkInfo.setText("");
        try {
            jXHyperlinkInfo.setAction(new UrlHyperlinkAction(parentComponent, ""));
        } catch (URISyntaxException ignored) {
        }
        setHyperLink(""); // zum löschen

    }

    private void setHyperLink(String uurl) {
        if (!uurl.equals("")) {
            jLabelInfo.setVisible(true);
            jXHyperlinkInfo.setVisible(true);
            jXHyperlinkInfo.setText(uurl);
        } else {
            jLabelInfo.setVisible(false);
            jXHyperlinkInfo.setVisible(false);
        }

    }

    private void setAufloesung() {
        if (jRadioButtonAufloesungNormal.isSelected()) {
            DatenPset pset = getPset();
            if (pset != null) {
                pset.arr[DatenPset.PROGRAMMSET_AUFLOESUNG_NR] = DatenFilm.AUFLOESUNG_NORMAL;
            }
        }
        if (jRadioButtonAufloesungHD.isSelected()) {
            DatenPset pset = getPset();
            if (pset != null) {
                pset.arr[DatenPset.PROGRAMMSET_AUFLOESUNG_NR] = DatenFilm.AUFLOESUNG_HD;
            }
        }
        if (jRadioButtonAufloesungKlein.isSelected()) {
            DatenPset pset = getPset();
            if (pset != null) {
                pset.arr[DatenPset.PROGRAMMSET_AUFLOESUNG_NR] = DatenFilm.AUFLOESUNG_KLEIN;
            }
        }
    }

    private void tabellePset() {
        nurtabellePset();
        tabelleProgramme();
    }

    private void nurtabellePset() {
        stopBeob = true;
        tabellePset.getSpalten();
        tabellePset.setModel(listePset.getModel());
        tabellePset.setSpalten();
        spaltenSetzen();
        jLabelMeldungAbspielen.setVisible(listePset.getPsetAbspielen() == null);
        jLabelMeldungSeichern.setVisible(listePset.getListeSpeichern().size() == 0);
        stopBeob = false;
    }

    private void spaltenSetzen() {
        for (int i = 0; i < tabellePset.getColumnCount(); ++i) {
            if (i == DatenPset.PROGRAMMSET_NAME_NR) {
                tabellePset.getColumnModel().getColumn(tabellePset.convertColumnIndexToView(i)).setMinWidth(10);
                tabellePset.getColumnModel().getColumn(tabellePset.convertColumnIndexToView(i)).setPreferredWidth(120);
                tabellePset.getColumnModel().getColumn(tabellePset.convertColumnIndexToView(i)).setMaxWidth(1000);
            } else if (i == DatenPset.PROGRAMMSET_IST_ABSPIELEN_NR
                    || i == DatenPset.PROGRAMMSET_IST_SPEICHERN_NR) {
                tabellePset.getColumnModel().getColumn(tabellePset.convertColumnIndexToView(i)).setMinWidth(10);
                tabellePset.getColumnModel().getColumn(tabellePset.convertColumnIndexToView(i)).setPreferredWidth(80);
                tabellePset.getColumnModel().getColumn(tabellePset.convertColumnIndexToView(i)).setMaxWidth(1000);
            } else {
                tabellePset.getColumnModel().getColumn(tabellePset.convertColumnIndexToView(i)).setMinWidth(0);
                tabellePset.getColumnModel().getColumn(tabellePset.convertColumnIndexToView(i)).setPreferredWidth(0);
                tabellePset.getColumnModel().getColumn(tabellePset.convertColumnIndexToView(i)).setMaxWidth(0);
            }
        }
    }

    private void tabelleProgramme() {
        //Tabelle mit den Programmen füllen
        DatenPset pSet = getPset();
        stopBeob = true;
        jTextFieldSetName.setEnabled(pSet != null);
        jTextFieldGruppeDirektSuffix.setEnabled(pSet != null);
        jTextFieldGruppeDirektPraefix.setEnabled(pSet != null);
        jTextFieldGruppeZielName.setEnabled(pSet != null);
        jTextFieldGruppeZielPfad.setEnabled(pSet != null);
        jTextAreaSetBeschreibung.setEnabled(pSet != null);
        jXHyperlinkInfo.setEnabled(pSet != null);
        jButtonGruppePfad.setEnabled(pSet != null);
        jButtonAbspielen.setEnabled(pSet != null);
        jButtonAbspielen.setBackground(null);
        jCheckBoxSpeichern.setEnabled(pSet != null);
        jCheckBoxButton.setEnabled(pSet != null);
        jCheckBoxAbo.setEnabled(pSet != null);
        jCheckBoxLaenge.setEnabled(pSet != null);
        jCheckBoxThema.setEnabled(pSet != null);
        jSpinnerLaenge.setEnabled(pSet != null);
        jCheckBoxInfodatei.setEnabled(pSet != null);
        jCheckBoxSubtitle.setEnabled(pSet != null);
        jCheckBoxSpotlight.setEnabled(pSet != null && (SystemInfo.isMacOSX() || Daten.debug));
        if (pSet != null) {
            jTabbedPane.setTitleAt(0, "Set Name: " + pSet.arr[DatenPset.PROGRAMMSET_NAME_NR]);
            if (pSet.arr[DatenPset.PROGRAMMSET_MAX_LAENGE_NR].equals("")) {
                jSpinnerLaenge.setValue(Konstanten.LAENGE_DATEINAME);
                pSet.arr[DatenPset.PROGRAMMSET_MAX_LAENGE_NR] = String.valueOf(Konstanten.LAENGE_DATEINAME);
            } else {
                jSpinnerLaenge.setValue(Integer.parseInt(pSet.arr[DatenPset.PROGRAMMSET_MAX_LAENGE_NR]));
            }
            jCheckBoxLaenge.setSelected(Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_LAENGE_BESCHRAENKEN_NR]));
            jCheckBoxThema.setSelected(Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_THEMA_ANLEGEN_NR]));
            jCheckBoxInfodatei.setSelected(Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_INFODATEI_NR]));
            jCheckBoxSubtitle.setSelected(Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_SUBTITLE_NR]));
            jCheckBoxSpotlight.setSelected(Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_SPOTLIGHT_NR]));
            jScrollPane1.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Set Name: " + pSet.arr[DatenPset.PROGRAMMSET_NAME_NR], javax.swing.border.TitledBorder.LEFT, javax.swing.border.TitledBorder.TOP));
            jTextFieldSetName.setText(pSet.arr[DatenPset.PROGRAMMSET_NAME_NR]);
            jTextFieldGruppeDirektSuffix.setText(pSet.arr[DatenPset.PROGRAMMSET_SUFFIX_DIREKT_NR]);
            jTextFieldGruppeDirektPraefix.setText(pSet.arr[DatenPset.PROGRAMMSET_PRAEFIX_DIREKT_NR]);
            jTextFieldGruppeZielName.setText(pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME_NR]);
            jTextFieldGruppeZielPfad.setText(pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR]);
            jTextAreaSetBeschreibung.setText(pSet.arr[DatenPset.PROGRAMMSET_BESCHREIBUNG_NR]);
            setHyperLink(pSet.arr[DatenPset.PROGRAMMSET_INFO_URL_NR]);

            jCheckBoxSpeichern.setSelected(pSet.istSpeichern());
            jCheckBoxButton.setSelected(pSet.istButton());
            jCheckBoxAbo.setSelected(pSet.istAbo());
            jButtonAbspielen.setBackground(pSet.istAbspielen() ? MVColor.BUTTON_SET_ABSPIELEN.color : null);
            switch (pSet.arr[DatenPset.PROGRAMMSET_AUFLOESUNG_NR]) {
                case DatenFilm.AUFLOESUNG_HD:
                    jRadioButtonAufloesungHD.setSelected(true);
                    break;
                case DatenFilm.AUFLOESUNG_KLEIN:
                    jRadioButtonAufloesungKlein.setSelected(true);
                    break;
                default:
                    jRadioButtonAufloesungNormal.setSelected(true);
                    break;
            }
        } else {
            jScrollPane1.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "", javax.swing.border.TitledBorder.LEFT, javax.swing.border.TitledBorder.TOP));
            jTabbedPane.setTitleAt(0, "Sets");
            //jSpinnerLaenge.setValue(GuiKonstanten.MAX_LAENGE_DATEINAME); Exception!
            jCheckBoxLaenge.setSelected(false);
            jCheckBoxThema.setSelected(false);
            jCheckBoxInfodatei.setSelected(false);
            jCheckBoxSubtitle.setSelected(false);
            jCheckBoxSpotlight.setSelected(false);
            jTextFieldSetName.setText("");
            jTextFieldGruppeDirektSuffix.setText("");
            jTextFieldGruppeDirektPraefix.setText("");
            jTextFieldGruppeZielName.setText("");
            jTextFieldGruppeZielPfad.setText("");
            jTextAreaSetBeschreibung.setText("");
            setHyperLink("");
        }
        if (pSet != null) {
            tabelleProgramme.setModel(pSet.getListeProg().getModel());
            if (tabelleProgramme.getRowCount() > 0) {
                spaltenSetzenProgramme();
                tabelleProgramme.setRowSelectionInterval(0, 0);
                tabelleProgramme.scrollRectToVisible(tabelleProgramme.getCellRect(0, 0, true));
            }
        } else {
            tabelleProgramme.setModel(new TModel(new Object[0][DatenProg.MAX_ELEM], DatenProg.COLUMN_NAMES));
        }
        stopBeob = false;
        fillTextProgramme();
    }

    public void spaltenSetzenProgramme() {
        for (int i = 0; i < tabelleProgramme.getColumnCount(); ++i) {
            if (i == DatenProg.PROGRAMM_PRAEFIX_NR
                    || i == DatenProg.PROGRAMM_RESTART_NR
                    || i == DatenProg.PROGRAMM_SUFFIX_NR) {
                tabelleProgramme.getColumnModel().getColumn(tabelleProgramme.convertColumnIndexToView(i)).setMinWidth(10);
                tabelleProgramme.getColumnModel().getColumn(tabelleProgramme.convertColumnIndexToView(i)).setMaxWidth(3000);
                tabelleProgramme.getColumnModel().getColumn(tabelleProgramme.convertColumnIndexToView(i)).setPreferredWidth(75);
            } else {
                tabelleProgramme.getColumnModel().getColumn(tabelleProgramme.convertColumnIndexToView(i)).setMinWidth(10);
                tabelleProgramme.getColumnModel().getColumn(tabelleProgramme.convertColumnIndexToView(i)).setMaxWidth(3000);
                tabelleProgramme.getColumnModel().getColumn(tabelleProgramme.convertColumnIndexToView(i)).setPreferredWidth(150);
            }
        }
    }

    private void notifyPset() {
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_PSET, PanelPsetLang.class.getSimpleName());
    }

    private void fillTextProgramme() {
        //Textfelder mit Programmdaten füllen
        stopBeob = true;
        int row = tabelleProgramme.getSelectedRow();
        boolean letzteZeile = false;
        if (tabelleProgramme.getRowCount() <= 1 || row == tabelleProgramme.getRowCount() - 1) {
            letzteZeile = true;
        }
        jTextFieldProgPfad.setEnabled(row != -1);
        jTextFieldProgSchalter.setEnabled(row != -1);
        jTextFieldProgZielDateiName.setEnabled(row != -1);
        jTextFieldProgName.setEnabled(row != -1);
        jTextFieldProgZielDateiName.setEnabled(row != -1);
        jTextFieldProgPraefix.setEnabled(row != -1);
        jTextFieldProgSuffix.setEnabled(row != -1);
        jButtonProgPfad.setEnabled(row != -1);
        jCheckBoxRestart.setEnabled(row != -1);
        if (row != -1) {
            DatenProg prog = getPset().getProg(tabelleProgramme.convertRowIndexToModel(row));
            jTextFieldProgPfad.setText(prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD_NR]);
            jTextFieldProgSchalter.setText(prog.arr[DatenProg.PROGRAMM_SCHALTER_NR]);
            jTextFieldProgZielDateiName.setText(prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR]);
            jTextFieldProgName.setText(prog.arr[DatenProg.PROGRAMM_NAME_NR]);
            jTextFieldProgZielDateiName.setText(prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR]);
            jTextFieldProgPraefix.setText(prog.arr[DatenProg.PROGRAMM_PRAEFIX_NR]);
            jTextFieldProgSuffix.setText(prog.arr[DatenProg.PROGRAMM_SUFFIX_NR]);
            jCheckBoxRestart.setSelected(prog.isRestart());
        } else {
            jTextFieldProgPfad.setText("");
            jTextFieldProgSchalter.setText("");
            jTextFieldProgZielDateiName.setText("");
            jTextFieldProgName.setText("");
            jTextFieldProgZielDateiName.setText("");
            jTextFieldProgPraefix.setText("");
            jTextFieldProgSuffix.setText("");
        }
        if (letzteZeile) {
            jTextFieldProgPraefix.setEnabled(false);
            jTextFieldProgSuffix.setEnabled(false);
        }
        stopBeob = false;
    }

    //Pset
    private DatenPset getPset() {
        DatenPset ret = null;
        int row = tabellePset.getSelectedRow();
        if (row != -1) {
            ret = listePset.get(tabellePset.convertRowIndexToModel(row));
        }
        return ret;
    }

    private void setNamePruefen() {
        //doppelte Gruppennamen suchen
        int row = tabellePset.getSelectedRow();
        if (row != -1) {
            int foundgruppe = 0;
            for (DatenPset gruppe : listePset) {
                if (jTextFieldSetName.getText().equals(gruppe.arr[DatenPset.PROGRAMMSET_NAME_NR])) {
                    ++foundgruppe;
                }
            }
            if (foundgruppe > 1) {
                jTextFieldSetName.setBackground(Color.ORANGE);
            } else {
                jTextFieldSetName.setBackground(Color.WHITE);
            }
        }
    }

    private void setAufAb(boolean auf) {
        int row = tabellePset.getSelectedRow();
        if (row != -1) {
            int neu = listePset.auf(tabellePset.convertRowIndexToModel(row), auf);
            neu = tabellePset.convertRowIndexToView(neu);
//            tabellePset();
            tabellePset.setRowSelectionInterval(neu, neu);
            tabellePset.scrollRectToVisible(tabellePset.getCellRect(neu, 0, false));
            notifyPset();
        } else {
            new HinweisKeineAuswahl().zeigen(parentComponent);
        }
    }

    private void setNeu() {
        listePset.addPset(new DatenPset("Neu-" + ++neuZaehler));
        tabellePset();
        notifyPset();
    }

    private void setLoeschen() {
        int rows[] = tabellePset.getSelectedRows();
        if (rows.length > 0) {
            DatenPset pSet;
            String text;
            if (rows.length == 1) {
                pSet = listePset.get(tabellePset.convertRowIndexToModel(rows[0]));
                text = pSet.arr[DatenPset.PROGRAMMSET_NAME_NR];
            } else {
                text = rows.length + " Set löschen?";
            }
            int ret = JOptionPane.showConfirmDialog(parentComponent, text, "Löschen?", JOptionPane.YES_NO_OPTION);
            if (ret == JOptionPane.OK_OPTION) {
                for (int i = rows.length - 1; i >= 0; --i) {
                    int delRow = tabellePset.convertRowIndexToModel(rows[i]);
                    ((TModel) tabellePset.getModel()).removeRow(delRow);
                    listePset.remove(delRow);
                }
                tabellePset();
                notifyPset();
            }
        } else {
            new HinweisKeineAuswahl().zeigen(parentComponent);
        }
    }

    private void setExport() {
        LinkedList<DatenPset> liste = new LinkedList<>();
        int rows[] = tabellePset.getSelectedRows();
        if (rows.length > 0) {
            DatenPset pSet;
            for (int row : rows) {
                int delRow = tabellePset.convertRowIndexToModel(row);
                pSet = listePset.get(delRow);
                if (pSet != null) {
                    liste.add(pSet);
                }
            }
            String name = liste.getFirst().arr[DatenPset.PROGRAMMSET_NAME_NR].equals("") ? "Name.xml" : liste.getFirst().arr[DatenPset.PROGRAMMSET_NAME_NR] + ".xml";
            DialogZielExportPset dialogZiel = new DialogZielExportPset(null, daten, true, exportPfad, FilenameUtils.replaceLeerDateiname(name));
            dialogZiel.setVisible(true);
            if (dialogZiel.ok) {
                if (dialogZiel.ziel.contains(File.separator)) {
                    exportPfad = dialogZiel.ziel.substring(0, dialogZiel.ziel.lastIndexOf(File.separator));
                }
                IoXmlSchreiben.exportPset(liste.toArray(new DatenPset[liste.size()]), dialogZiel.ziel);
            }
        } else {
            new HinweisKeineAuswahl().zeigen(parentComponent);
        }
    }

    private void progNeueZeile(DatenProg prog) {
        DatenPset gruppe = getPset();
        if (gruppe != null) {
            gruppe.addProg(prog);
            tabelleProgramme();
        }
    }

    private void progAufAb(boolean auf) {
        int rows = tabelleProgramme.getSelectedRow();
        if (rows != -1) {
            int row = tabelleProgramme.convertRowIndexToModel(rows);
            int neu = getPset().getListeProg().auf(row, auf);
            tabelleProgramme();
            tabelleProgramme.setRowSelectionInterval(neu, neu);
            tabelleProgramme.scrollRectToVisible(tabelleProgramme.getCellRect(neu, 0, true));
        } else {
            new HinweisKeineAuswahl().zeigen(parentComponent);
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
        jButtonHilfe = new javax.swing.JButton();
        jButtonPruefen = new javax.swing.JButton();
        javax.swing.JSplitPane jSplitPane1 = new javax.swing.JSplitPane();
        jTabbedPane = new javax.swing.JTabbedPane();
        javax.swing.JPanel jPanelDetails = new javax.swing.JPanel();
        javax.swing.JPanel jPanel7 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel6 = new javax.swing.JLabel();
        jTextFieldSetName = new javax.swing.JTextField();
        javax.swing.JPanel jPanel4 = new javax.swing.JPanel();
        javax.swing.JScrollPane jScrollPane2 = new javax.swing.JScrollPane();
        jTextAreaSetBeschreibung = new javax.swing.JTextArea();
        jLabelInfo = new javax.swing.JLabel();
        jXHyperlinkInfo = new org.jdesktop.swingx.JXHyperlink();
        javax.swing.JPanel jPanel6 = new javax.swing.JPanel();
        jCheckBoxSpeichern = new javax.swing.JCheckBox();
        jCheckBoxButton = new javax.swing.JCheckBox();
        jCheckBoxAbo = new javax.swing.JCheckBox();
        jButtonAbspielen = new javax.swing.JButton();
        jLabelMeldungAbspielen = new javax.swing.JLabel();
        jLabelMeldungSeichern = new javax.swing.JLabel();
        javax.swing.JPanel jPanel10 = new javax.swing.JPanel();
        javax.swing.JPanel jPanel5 = new javax.swing.JPanel();
        jButtonGruppeFarbe = new javax.swing.JButton();
        jButtonGruppeStandardfarbe = new javax.swing.JButton();
        javax.swing.JLabel jLabel11 = new javax.swing.JLabel();
        javax.swing.JLabel jLabel13 = new javax.swing.JLabel();
        javax.swing.JPanel jPanel9 = new javax.swing.JPanel();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel7 = new javax.swing.JLabel();
        jTextFieldGruppeZielPfad = new javax.swing.JTextField();
        jButtonGruppePfad = new javax.swing.JButton();
        jCheckBoxThema = new javax.swing.JCheckBox();
        javax.swing.JLabel jLabel8 = new javax.swing.JLabel();
        jTextFieldGruppeZielName = new javax.swing.JTextField();
        jCheckBoxLaenge = new javax.swing.JCheckBox();
        jSpinnerLaenge = new javax.swing.JSpinner();
        javax.swing.JLabel jLabel12 = new javax.swing.JLabel();
        jLabel15 = new javax.swing.JLabel();
        javax.swing.JPanel jPanel11 = new javax.swing.JPanel();
        javax.swing.JPanel jPanel8 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel10 = new javax.swing.JLabel();
        jTextFieldGruppeDirektPraefix = new javax.swing.JTextField();
        javax.swing.JLabel jLabel5 = new javax.swing.JLabel();
        jTextFieldGruppeDirektSuffix = new javax.swing.JTextField();
        javax.swing.JTextArea jTextArea1 = new javax.swing.JTextArea();
        javax.swing.JPanel jPanel12 = new javax.swing.JPanel();
        jRadioButtonAufloesungNormal = new javax.swing.JRadioButton();
        jRadioButtonAufloesungKlein = new javax.swing.JRadioButton();
        jRadioButtonAufloesungHD = new javax.swing.JRadioButton();
        javax.swing.JLabel jLabel14 = new javax.swing.JLabel();
        javax.swing.JPanel jPanel13 = new javax.swing.JPanel();
        jCheckBoxInfodatei = new javax.swing.JCheckBox();
        jCheckBoxSpotlight = new javax.swing.JCheckBox();
        jCheckBoxSubtitle = new javax.swing.JCheckBox();
        javax.swing.JPanel jPanelProgramme = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        javax.swing.JTable jTableProgramme = new javax.swing.JTable();
        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        jButtonProgPlus = new javax.swing.JButton();
        jButtonProgMinus = new javax.swing.JButton();
        jButtonProgDuplizieren = new javax.swing.JButton();
        jButtonProgAuf = new javax.swing.JButton();
        jButtonProgAb = new javax.swing.JButton();
        javax.swing.JPanel jPanelProgrammDetails = new javax.swing.JPanel();
        javax.swing.JLabel jLabel = new javax.swing.JLabel();
        jTextFieldProgPfad = new javax.swing.JTextField();
        jButtonProgPfad = new javax.swing.JButton();
        jTextFieldProgSchalter = new javax.swing.JTextField();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        javax.swing.JLabel jLabel2 = new javax.swing.JLabel();
        jTextFieldProgName = new javax.swing.JTextField();
        javax.swing.JLabel jLabel3 = new javax.swing.JLabel();
        jTextFieldProgPraefix = new javax.swing.JTextField();
        javax.swing.JLabel jLabel4 = new javax.swing.JLabel();
        jTextFieldProgSuffix = new javax.swing.JTextField();
        jCheckBoxRestart = new javax.swing.JCheckBox();
        javax.swing.JLabel jLabel9 = new javax.swing.JLabel();
        jTextFieldProgZielDateiName = new javax.swing.JTextField();
        javax.swing.JPanel jPanel3 = new javax.swing.JPanel();
        jScrollPane3 = new javax.swing.JScrollPane();
        javax.swing.JTable jTablePset = new javax.swing.JTable();
        jButtonGruppeDuplizieren = new javax.swing.JButton();
        jButtonExport = new javax.swing.JButton();
        jButtonGruppeNeu = new javax.swing.JButton();
        jButtonGruppeLoeschen = new javax.swing.JButton();
        jButtonGruppeAuf = new javax.swing.JButton();
        jButtonGruppeAb = new javax.swing.JButton();

        jButtonHilfe.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/help_16.png"))); // NOI18N
        jButtonHilfe.setToolTipText("Hilfedialog anzeigen");

        jButtonPruefen.setText("Prüfen");
        jButtonPruefen.setToolTipText("Programmpfade prüfen");

        jSplitPane1.setDividerLocation(200);

        jPanelDetails.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));

        jPanel7.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jLabel6.setText("Set Name:");

        jPanel4.setBorder(javax.swing.BorderFactory.createTitledBorder("Beschreibung"));

        jTextAreaSetBeschreibung.setColumns(20);
        jTextAreaSetBeschreibung.setRows(8);
        jScrollPane2.setViewportView(jTextAreaSetBeschreibung);

        jLabelInfo.setText("Infos:");

        jXHyperlinkInfo.setText("jXHyperlink1");

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 553, Short.MAX_VALUE)
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addComponent(jLabelInfo)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jXHyperlinkInfo, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 194, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabelInfo)
                    .addComponent(jXHyperlinkInfo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );

        jPanel6.setBorder(javax.swing.BorderFactory.createTitledBorder("Funktion"));

        jCheckBoxSpeichern.setText("Speichern");

        jCheckBoxButton.setText("Button");

        jCheckBoxAbo.setText("Abo");

        jButtonAbspielen.setText("Abspielen via grünem Play-Button");

        javax.swing.GroupLayout jPanel6Layout = new javax.swing.GroupLayout(jPanel6);
        jPanel6.setLayout(jPanel6Layout);
        jPanel6Layout.setHorizontalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jButtonAbspielen)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jCheckBoxSpeichern)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jCheckBoxButton)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jCheckBoxAbo)
                .addContainerGap(55, Short.MAX_VALUE))
        );
        jPanel6Layout.setVerticalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jCheckBoxSpeichern)
                    .addComponent(jCheckBoxButton)
                    .addComponent(jCheckBoxAbo)
                    .addComponent(jButtonAbspielen))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jLabelMeldungAbspielen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/alert_16.png"))); // NOI18N
        jLabelMeldungAbspielen.setText("kein Set zum Abspielen ausgewählt!");

        jLabelMeldungSeichern.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/alert_16.png"))); // NOI18N
        jLabelMeldungSeichern.setText("kein Set zum Speichern ausgewählt!");

        javax.swing.GroupLayout jPanel7Layout = new javax.swing.GroupLayout(jPanel7);
        jPanel7.setLayout(jPanel7Layout);
        jPanel7Layout.setHorizontalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel7Layout.createSequentialGroup()
                        .addComponent(jLabel6)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldSetName))
                    .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel6, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(jPanel7Layout.createSequentialGroup()
                        .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabelMeldungAbspielen)
                            .addComponent(jLabelMeldungSeichern))
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanel7Layout.setVerticalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jTextFieldSetName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel6))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGap(18, 18, 18)
                .addComponent(jPanel6, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jLabelMeldungAbspielen)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jLabelMeldungSeichern)
                .addGap(38, 38, 38))
        );

        javax.swing.GroupLayout jPanelDetailsLayout = new javax.swing.GroupLayout(jPanelDetails);
        jPanelDetails.setLayout(jPanelDetailsLayout);
        jPanelDetailsLayout.setHorizontalGroup(
            jPanelDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelDetailsLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel7, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        jPanelDetailsLayout.setVerticalGroup(
            jPanelDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelDetailsLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel7, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGap(21, 21, 21))
        );

        jTabbedPane.addTab("Einstellungen", jPanelDetails);

        jPanel5.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jButtonGruppeFarbe.setText("Farbe");
        jButtonGruppeFarbe.setToolTipText("Farbauswahldialog anzeigen");

        jButtonGruppeStandardfarbe.setText("Standardfarbe");
        jButtonGruppeStandardfarbe.setToolTipText("Farbe zurücksetzen");

        jLabel11.setText("Wird das Set als Button verwendet,");

        jLabel13.setText("kann damit die Schriftfarbe verändert werden.");

        javax.swing.GroupLayout jPanel5Layout = new javax.swing.GroupLayout(jPanel5);
        jPanel5.setLayout(jPanel5Layout);
        jPanel5Layout.setHorizontalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel5Layout.createSequentialGroup()
                        .addComponent(jButtonGruppeFarbe)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonGruppeStandardfarbe))
                    .addComponent(jLabel11)
                    .addComponent(jLabel13))
                .addContainerGap(280, Short.MAX_VALUE))
        );

        jPanel5Layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonGruppeFarbe, jButtonGruppeStandardfarbe});

        jPanel5Layout.setVerticalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel11)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel13)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonGruppeFarbe)
                    .addComponent(jButtonGruppeStandardfarbe))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout jPanel10Layout = new javax.swing.GroupLayout(jPanel10);
        jPanel10.setLayout(jPanel10Layout);
        jPanel10Layout.setHorizontalGroup(
            jPanel10Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel10Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel5, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        jPanel10Layout.setVerticalGroup(
            jPanel10Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel10Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel5, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(405, Short.MAX_VALUE))
        );

        jTabbedPane.addTab("Aussehen", jPanel10);

        jPanel1.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jLabel7.setText("Zielpfad:");

        jButtonGruppePfad.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/fileopen_16.png"))); // NOI18N
        jButtonGruppePfad.setToolTipText("Pfad auswählen");

        jCheckBoxThema.setText("einen Unterordner mit dem Thema / Abo-Zielpfad anlegen");
        jCheckBoxThema.setToolTipText("im Zielverzeichnis wird ein Unterordner mit dem Namen des Themas zum Speichern der Filme angelegt");

        jLabel8.setText("Zieldateiname:");

        jCheckBoxLaenge.setText("Dateiname beschränken auf:");
        jCheckBoxLaenge.setToolTipText("die Länge des Dateinamens wird auf die Anzahl Zeichen beschränkt");

        jSpinnerLaenge.setModel(new javax.swing.SpinnerNumberModel(25, 12, 200, 1));

        jLabel12.setText("Zeichen");

        jLabel15.setText("(ist der Abo-Zielpfad leer, wird das Thema verwendet)");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addGap(21, 21, 21)
                        .addComponent(jLabel15)
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addComponent(jLabel7)
                                .addGap(56, 56, 56)
                                .addComponent(jTextFieldGruppeZielPfad)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonGruppePfad))
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addComponent(jCheckBoxThema)
                                .addGap(0, 156, Short.MAX_VALUE)))
                        .addGap(16, 16, 16))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                        .addComponent(jLabel8)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jTextFieldGruppeZielName)
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addComponent(jCheckBoxLaenge)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jSpinnerLaenge, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabel12)
                                .addGap(0, 0, Short.MAX_VALUE)))
                        .addContainerGap())))
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jCheckBoxThema)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel15)
                .addGap(18, 18, 18)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel7)
                    .addComponent(jTextFieldGruppeZielPfad, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonGruppePfad))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel8)
                    .addComponent(jTextFieldGruppeZielName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jCheckBoxLaenge)
                    .addComponent(jSpinnerLaenge, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel12))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel1Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonGruppePfad, jTextFieldGruppeZielName, jTextFieldGruppeZielPfad});

        javax.swing.GroupLayout jPanel9Layout = new javax.swing.GroupLayout(jPanel9);
        jPanel9.setLayout(jPanel9Layout);
        jPanel9Layout.setHorizontalGroup(
            jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel9Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        jPanel9Layout.setVerticalGroup(
            jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel9Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(328, Short.MAX_VALUE))
        );

        jTabbedPane.addTab("Speicherziel", jPanel9);

        jPanel8.setBorder(javax.swing.BorderFactory.createTitledBorder("direkt speichern"));

        jLabel10.setText("direkter Download, Präfix ( z.B. http ):");

        jLabel5.setText("Suffix ( z.B. mp4,mp3):");

        javax.swing.GroupLayout jPanel8Layout = new javax.swing.GroupLayout(jPanel8);
        jPanel8.setLayout(jPanel8Layout);
        jPanel8Layout.setHorizontalGroup(
            jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel8Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel10)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jTextFieldGruppeDirektPraefix)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel5)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jTextFieldGruppeDirektSuffix)
                .addContainerGap())
        );
        jPanel8Layout.setVerticalGroup(
            jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel8Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel10)
                    .addComponent(jTextFieldGruppeDirektPraefix, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel5)
                    .addComponent(jTextFieldGruppeDirektSuffix, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(34, Short.MAX_VALUE))
        );

        jTextArea1.setEditable(false);
        jTextArea1.setBackground(new java.awt.Color(238, 238, 238));
        jTextArea1.setColumns(20);
        jTextArea1.setRows(4);
        jTextArea1.setText("Filme, deren URL mit \"Präfix\" beginnt und mit \"Suffix\" endet, werden nicht\nmit einem Hilfsprogramm gespeichert, sondern direkt geladen.\n\nEine geringere Auflösung ist nicht bei jedem Sender möglich, es wird dann in der gleichen\nAuflösung geladen.");
        jTextArea1.setBorder(null);

        jPanel12.setBorder(javax.swing.BorderFactory.createTitledBorder("Auflösung"));

        buttonGroup1.add(jRadioButtonAufloesungNormal);
        jRadioButtonAufloesungNormal.setSelected(true);
        jRadioButtonAufloesungNormal.setText("Film in hoher Auflösung laden");

        buttonGroup1.add(jRadioButtonAufloesungKlein);
        jRadioButtonAufloesungKlein.setText("Film in niedriger Auflösung laden");

        buttonGroup1.add(jRadioButtonAufloesungHD);
        jRadioButtonAufloesungHD.setText("Film in HD laden");

        jLabel14.setText("Wenn es die Auflösung nicht gibt, wird die nächstkleinere genommen.");

        javax.swing.GroupLayout jPanel12Layout = new javax.swing.GroupLayout(jPanel12);
        jPanel12.setLayout(jPanel12Layout);
        jPanel12Layout.setHorizontalGroup(
            jPanel12Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel12Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel12Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel14)
                    .addComponent(jRadioButtonAufloesungNormal)
                    .addComponent(jRadioButtonAufloesungKlein)
                    .addComponent(jRadioButtonAufloesungHD))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel12Layout.setVerticalGroup(
            jPanel12Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel12Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jRadioButtonAufloesungHD)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jRadioButtonAufloesungNormal)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jRadioButtonAufloesungKlein)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jLabel14)
                .addContainerGap(30, Short.MAX_VALUE))
        );

        jPanel13.setBorder(javax.swing.BorderFactory.createTitledBorder("nach dem Speichern"));

        jCheckBoxInfodatei.setText("Infodatei anlegen: \"Filmname.txt\"");

        jCheckBoxSpotlight.setText("Filmbeschreibung als Finder-Kommentar für Spotlight speichern (nur OS X)");

        jCheckBoxSubtitle.setText("Untertitel speichern: \"Filmname.xxx\"");

        javax.swing.GroupLayout jPanel13Layout = new javax.swing.GroupLayout(jPanel13);
        jPanel13.setLayout(jPanel13Layout);
        jPanel13Layout.setHorizontalGroup(
            jPanel13Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel13Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel13Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jCheckBoxSpotlight)
                    .addComponent(jCheckBoxInfodatei)
                    .addComponent(jCheckBoxSubtitle))
                .addContainerGap(51, Short.MAX_VALUE))
        );
        jPanel13Layout.setVerticalGroup(
            jPanel13Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel13Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jCheckBoxInfodatei)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jCheckBoxSubtitle)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(jCheckBoxSpotlight))
        );

        javax.swing.GroupLayout jPanel11Layout = new javax.swing.GroupLayout(jPanel11);
        jPanel11.setLayout(jPanel11Layout);
        jPanel11Layout.setHorizontalGroup(
            jPanel11Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel11Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel11Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jTextArea1)
                    .addComponent(jPanel8, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel12, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel13, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel11Layout.setVerticalGroup(
            jPanel11Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel11Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTextArea1, javax.swing.GroupLayout.PREFERRED_SIZE, 91, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel8, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel12, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel13, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jTabbedPane.addTab("Download", jPanel11);

        jScrollPane1.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Titel", javax.swing.border.TitledBorder.LEFT, javax.swing.border.TitledBorder.TOP));

        jTableProgramme.setModel(new TModel());
        jTableProgramme.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        jScrollPane1.setViewportView(jTableProgramme);

        jPanel2.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jButtonProgPlus.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/add_16.png"))); // NOI18N
        jButtonProgPlus.setToolTipText("neues Programm anlegen");

        jButtonProgMinus.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/remove_16.png"))); // NOI18N
        jButtonProgMinus.setToolTipText("markiertes Programm löschen");

        jButtonProgDuplizieren.setText("Duplizieren");
        jButtonProgDuplizieren.setToolTipText("markierte Zeile duplizieren");

        jButtonProgAuf.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/move_up_16.png"))); // NOI18N
        jButtonProgAuf.setToolTipText("markierte Zeile eins nach oben");

        jButtonProgAb.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/move_down_16.png"))); // NOI18N
        jButtonProgAb.setToolTipText("markierte Zeile eins nach unten");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jButtonProgPlus)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonProgMinus)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonProgAuf)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonProgAb)
                .addGap(18, 18, 18)
                .addComponent(jButtonProgDuplizieren)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jButtonProgPlus)
                        .addComponent(jButtonProgMinus)
                        .addComponent(jButtonProgAuf)
                        .addComponent(jButtonProgAb))
                    .addComponent(jButtonProgDuplizieren, javax.swing.GroupLayout.Alignment.TRAILING))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanelProgrammDetails.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jLabel.setText("Programm:");

        jButtonProgPfad.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/fileopen_16.png"))); // NOI18N
        jButtonProgPfad.setToolTipText("Pfad auswählen");

        jLabel1.setText("Schalter:");

        jLabel2.setText("Beschreibung:");

        jLabel3.setText("Präfix (z.B. http):");

        jLabel4.setText("Suffix ( z.B. mp4,mp3):");

        jCheckBoxRestart.setText("fehlgeschlagene Downloads wieder starten");

        jLabel9.setText("Zieldateiname:");

        javax.swing.GroupLayout jPanelProgrammDetailsLayout = new javax.swing.GroupLayout(jPanelProgrammDetails);
        jPanelProgrammDetails.setLayout(jPanelProgrammDetailsLayout);
        jPanelProgrammDetailsLayout.setHorizontalGroup(
            jPanelProgrammDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelProgrammDetailsLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelProgrammDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanelProgrammDetailsLayout.createSequentialGroup()
                        .addGroup(jPanelProgrammDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel3)
                            .addComponent(jLabel1)
                            .addComponent(jLabel))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanelProgrammDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jCheckBoxRestart)
                            .addComponent(jTextFieldProgSchalter)
                            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelProgrammDetailsLayout.createSequentialGroup()
                                .addComponent(jTextFieldProgPfad)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonProgPfad))
                            .addGroup(jPanelProgrammDetailsLayout.createSequentialGroup()
                                .addComponent(jTextFieldProgPraefix, javax.swing.GroupLayout.PREFERRED_SIZE, 146, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabel4)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jTextFieldProgSuffix, javax.swing.GroupLayout.DEFAULT_SIZE, 140, Short.MAX_VALUE))))
                    .addGroup(jPanelProgrammDetailsLayout.createSequentialGroup()
                        .addGroup(jPanelProgrammDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel2)
                            .addComponent(jLabel9))
                        .addGap(24, 24, 24)
                        .addGroup(jPanelProgrammDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jTextFieldProgZielDateiName)
                            .addComponent(jTextFieldProgName))))
                .addContainerGap())
        );
        jPanelProgrammDetailsLayout.setVerticalGroup(
            jPanelProgrammDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelProgrammDetailsLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelProgrammDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel2)
                    .addComponent(jTextFieldProgName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanelProgrammDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel9)
                    .addComponent(jTextFieldProgZielDateiName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanelProgrammDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel)
                    .addComponent(jTextFieldProgPfad, javax.swing.GroupLayout.PREFERRED_SIZE, 12, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonProgPfad))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanelProgrammDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jTextFieldProgSchalter, javax.swing.GroupLayout.PREFERRED_SIZE, 15, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanelProgrammDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE, false)
                    .addComponent(jTextFieldProgPraefix, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel3)
                    .addComponent(jTextFieldProgSuffix, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel4))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jCheckBoxRestart)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanelProgrammDetailsLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonProgPfad, jTextFieldProgName, jTextFieldProgPfad, jTextFieldProgPraefix, jTextFieldProgSchalter, jTextFieldProgSuffix, jTextFieldProgZielDateiName});

        javax.swing.GroupLayout jPanelProgrammeLayout = new javax.swing.GroupLayout(jPanelProgramme);
        jPanelProgramme.setLayout(jPanelProgrammeLayout);
        jPanelProgrammeLayout.setHorizontalGroup(
            jPanelProgrammeLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelProgrammeLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelProgrammeLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanelProgrammDetails, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jScrollPane1)
                    .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanelProgrammeLayout.setVerticalGroup(
            jPanelProgrammeLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelProgrammeLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 219, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanelProgrammDetails, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        jTabbedPane.addTab("Hilfsprogramme", jPanelProgramme);

        jSplitPane1.setRightComponent(jTabbedPane);

        jTablePset.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null}
            },
            new String [] {
                "Title 1", "Title 2", "Title 3"
            }
        ));
        jTablePset.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        jScrollPane3.setViewportView(jTablePset);

        jButtonGruppeDuplizieren.setText("Duplizieren");
        jButtonGruppeDuplizieren.setToolTipText("Programmgruppe kopieren");

        jButtonExport.setText("Export");

        jButtonGruppeNeu.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/add_16.png"))); // NOI18N
        jButtonGruppeNeu.setToolTipText("neue Programmgruppe anlegen");

        jButtonGruppeLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/remove_16.png"))); // NOI18N
        jButtonGruppeLoeschen.setToolTipText("Programmgruppe löschen");

        jButtonGruppeAuf.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/move_up_16.png"))); // NOI18N
        jButtonGruppeAuf.setToolTipText("Programmgruppe nach oben schieben");

        jButtonGruppeAb.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/move_down_16.png"))); // NOI18N
        jButtonGruppeAb.setToolTipText("Programmgruppe nach unten schieben");

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jScrollPane3, javax.swing.GroupLayout.PREFERRED_SIZE, 0, Short.MAX_VALUE)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jButtonExport, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jButtonGruppeDuplizieren, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addComponent(jButtonGruppeNeu)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jButtonGruppeLoeschen))
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addComponent(jButtonGruppeAuf)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jButtonGruppeAb)))
                .addContainerGap())
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addComponent(jScrollPane3, javax.swing.GroupLayout.DEFAULT_SIZE, 389, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jButtonGruppeAuf)
                    .addComponent(jButtonGruppeAb))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jButtonGruppeLoeschen)
                    .addComponent(jButtonGruppeNeu))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonGruppeDuplizieren)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonExport)
                .addContainerGap())
        );

        jSplitPane1.setLeftComponent(jPanel3);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jSplitPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 861, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jButtonPruefen, javax.swing.GroupLayout.PREFERRED_SIZE, 84, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonHilfe)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jSplitPane1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jButtonPruefen)
                    .addComponent(jButtonHilfe))
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAbspielen;
    private javax.swing.JButton jButtonExport;
    private javax.swing.JButton jButtonGruppeAb;
    private javax.swing.JButton jButtonGruppeAuf;
    private javax.swing.JButton jButtonGruppeDuplizieren;
    private javax.swing.JButton jButtonGruppeFarbe;
    private javax.swing.JButton jButtonGruppeLoeschen;
    private javax.swing.JButton jButtonGruppeNeu;
    private javax.swing.JButton jButtonGruppePfad;
    private javax.swing.JButton jButtonGruppeStandardfarbe;
    private javax.swing.JButton jButtonHilfe;
    private javax.swing.JButton jButtonProgAb;
    private javax.swing.JButton jButtonProgAuf;
    private javax.swing.JButton jButtonProgDuplizieren;
    private javax.swing.JButton jButtonProgMinus;
    private javax.swing.JButton jButtonProgPfad;
    private javax.swing.JButton jButtonProgPlus;
    private javax.swing.JButton jButtonPruefen;
    private javax.swing.JCheckBox jCheckBoxAbo;
    private javax.swing.JCheckBox jCheckBoxButton;
    private javax.swing.JCheckBox jCheckBoxInfodatei;
    private javax.swing.JCheckBox jCheckBoxLaenge;
    private javax.swing.JCheckBox jCheckBoxRestart;
    private javax.swing.JCheckBox jCheckBoxSpeichern;
    private javax.swing.JCheckBox jCheckBoxSpotlight;
    private javax.swing.JCheckBox jCheckBoxSubtitle;
    private javax.swing.JCheckBox jCheckBoxThema;
    private javax.swing.JLabel jLabel15;
    private javax.swing.JLabel jLabelInfo;
    private javax.swing.JLabel jLabelMeldungAbspielen;
    private javax.swing.JLabel jLabelMeldungSeichern;
    private javax.swing.JRadioButton jRadioButtonAufloesungHD;
    private javax.swing.JRadioButton jRadioButtonAufloesungKlein;
    private javax.swing.JRadioButton jRadioButtonAufloesungNormal;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JSpinner jSpinnerLaenge;
    private javax.swing.JTabbedPane jTabbedPane;
    private javax.swing.JTextArea jTextAreaSetBeschreibung;
    private javax.swing.JTextField jTextFieldGruppeDirektPraefix;
    private javax.swing.JTextField jTextFieldGruppeDirektSuffix;
    private javax.swing.JTextField jTextFieldGruppeZielName;
    private javax.swing.JTextField jTextFieldGruppeZielPfad;
    private javax.swing.JTextField jTextFieldProgName;
    private javax.swing.JTextField jTextFieldProgPfad;
    private javax.swing.JTextField jTextFieldProgPraefix;
    private javax.swing.JTextField jTextFieldProgSchalter;
    private javax.swing.JTextField jTextFieldProgSuffix;
    private javax.swing.JTextField jTextFieldProgZielDateiName;
    private javax.swing.JTextField jTextFieldSetName;
    private org.jdesktop.swingx.JXHyperlink jXHyperlinkInfo;
    // End of variables declaration//GEN-END:variables

    private class BeobProgRestart implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            if (!stopBeob) {
                int rows = tabelleProgramme.getSelectedRow();
                if (rows != -1) {
                    int row = tabelleProgramme.convertRowIndexToModel(rows);
                    DatenProg prog = getPset().getListeProg().get(row);
                    prog.arr[DatenProg.PROGRAMM_RESTART_NR] = Boolean.toString(jCheckBoxRestart.isSelected());
                    tabelleProgramme.getModel().setValueAt(Boolean.toString(jCheckBoxRestart.isSelected()), row, DatenProg.PROGRAMM_RESTART_NR);
                }
            }

        }
    }

    private class BeobProgDoc implements DocumentListener {

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
            if (!stopBeob) {
                int rows = tabelleProgramme.getSelectedRow();
                if (rows != -1) {
                    int row = tabelleProgramme.convertRowIndexToModel(rows);
                    DatenProg prog = getPset().getListeProg().get(row);
                    prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD_NR] = jTextFieldProgPfad.getText();
                    prog.arr[DatenProg.PROGRAMM_SCHALTER_NR] = jTextFieldProgSchalter.getText();
                    prog.arr[DatenProg.PROGRAMM_NAME_NR] = jTextFieldProgName.getText();
                    prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR] = jTextFieldProgZielDateiName.getText();
                    prog.arr[DatenProg.PROGRAMM_SUFFIX_NR] = jTextFieldProgSuffix.getText();
                    prog.arr[DatenProg.PROGRAMM_PRAEFIX_NR] = jTextFieldProgPraefix.getText();
                    tabelleProgramme.getModel().setValueAt(jTextFieldProgPfad.getText(), row, DatenProg.PROGRAMM_PROGRAMMPFAD_NR);
                    tabelleProgramme.getModel().setValueAt(jTextFieldProgSchalter.getText(), row, DatenProg.PROGRAMM_SCHALTER_NR);
                    tabelleProgramme.getModel().setValueAt(jTextFieldProgName.getText(), row, DatenProg.PROGRAMM_NAME_NR);
                    tabelleProgramme.getModel().setValueAt(jTextFieldProgZielDateiName.getText(), row, DatenProg.PROGRAMM_ZIEL_DATEINAME_NR);
                    tabelleProgramme.getModel().setValueAt(jTextFieldProgSuffix.getText(), row, DatenProg.PROGRAMM_SUFFIX_NR);
                    tabelleProgramme.getModel().setValueAt(jTextFieldProgPraefix.getText(), row, DatenProg.PROGRAMM_PRAEFIX_NR);
//                    progNamePruefen();
                }
            }
        }
    }

    private class BeobTableSelectPset implements ListSelectionListener {

        @Override
        public void valueChanged(ListSelectionEvent event) {
            if (!stopBeob) {
                if (!event.getValueIsAdjusting()) {
                    tabelleProgramme();
                    DatenPset datenPset;
                    int row = tabellePset.getSelectedRow();
                    if (row != -1) {
                        datenPset = listePset.get(tabellePset.convertRowIndexToModel(row));
                        tabellePset.getModel().setValueAt(jTextFieldSetName.getText(), tabellePset.convertRowIndexToModel(row), DatenPset.PROGRAMMSET_NAME_NR);
                        jTabbedPane.setTitleAt(0, "Set Name: " + datenPset.arr[DatenPset.PROGRAMMSET_NAME_NR]);
                    }
                }
            }
        }
    }

    private class BeobDateiDialogProg implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            //we can use native chooser on Mac...
            if (SystemInfo.isMacOSX()) {
                FileDialog chooser = new FileDialog(daten.mediathekGui, "Programm auswählen");
                chooser.setMode(FileDialog.LOAD);
                chooser.setVisible(true);
                if (chooser.getFile() != null) {
                    try {
                        jTextFieldProgPfad.setText(new File(chooser.getDirectory() + chooser.getFile()).getAbsolutePath());
                    } catch (Exception ex) {
                        Log.fehlerMeldung(369047894, ex);
                    }
                }
            } else {
                int returnVal;
                JFileChooser chooser = new JFileChooser();
                chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                if (!jTextFieldProgPfad.getText().equals("")) {
                    chooser.setCurrentDirectory(new File(jTextFieldProgPfad.getText()));
                }
                returnVal = chooser.showOpenDialog(null);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        String str = chooser.getSelectedFile().getPath();
                        jTextFieldProgPfad.setText(str);
                    } catch (Exception ex) {
                        Log.fehlerMeldung(825630443, ex);
                    }
                }
            }
        }
    }

    private class BeobDateiDialogPfad implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            //we can use native directory chooser on Mac...
            if (SystemInfo.isMacOSX()) {
                //we want to select a directory only, so temporarily change properties
                System.setProperty("apple.awt.fileDialogForDirectories", "true");
                FileDialog chooser = new FileDialog(daten.mediathekGui, "Film speichern");
                chooser.setVisible(true);
                if (chooser.getFile() != null) {
                    //A directory was selected, that means Cancel was not pressed
                    try {
                        jTextFieldGruppeZielPfad.setText(new File(chooser.getDirectory() + chooser.getFile()).getAbsolutePath());
                    } catch (Exception ex) {
                        Log.fehlerMeldung(392847589, ex);
                    }
                }
                System.setProperty("apple.awt.fileDialogForDirectories", "false");
            } else {
                //use the cross-platform swing chooser
                int returnVal;
                JFileChooser chooser = new JFileChooser();
                chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                if (!jTextFieldGruppeZielPfad.getText().equals("")) {
                    chooser.setCurrentDirectory(new File(jTextFieldGruppeZielPfad.getText()));
                }
                returnVal = chooser.showOpenDialog(null);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        jTextFieldGruppeZielPfad.setText(chooser.getSelectedFile().getPath());
                    } catch (Exception ex) {
                        Log.fehlerMeldung(319860075, ex);
                    }
                }
            }
        }
    }

    private class BeobProgNeueZeile implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            DatenProg prog = new DatenProg();
            progNeueZeile(prog);
        }
    }

    private class BeobProgDuplizieren implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            int rows = tabelleProgramme.getSelectedRow();
            if (rows != -1) {
                int row = tabelleProgramme.convertRowIndexToModel(rows);
                DatenProg prog = getPset().getListeProg().get(row);
                progNeueZeile(prog.copy());
            } else {
                new HinweisKeineAuswahl().zeigen(parentComponent);
            }
        }
    }

    private class BeobDoc implements DocumentListener {

        JTextField textfeld = null;
        JTextArea textArea = null;
        int nr;

        public BeobDoc(JTextField ttextfeld, int nnr) {
            textfeld = ttextfeld;
            nr = nnr;
        }

        public BeobDoc(JTextArea tt, int nnr) {
            textArea = tt;
            nr = nnr;
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
            if (!stopBeob) {
                DatenPset datenPset;
                int row = tabellePset.getSelectedRow();
                if (row != -1) {
                    datenPset = listePset.get(tabellePset.convertRowIndexToModel(row));
                    stopBeob = true;
                    if (textfeld != null) {
                        datenPset.arr[nr] = textfeld.getText();
                    } else {
                        datenPset.arr[nr] = textArea.getText();
                    }
                    if (nr == DatenPset.PROGRAMMSET_NAME_NR) {
                        tabellePset.getModel().setValueAt(jTextFieldSetName.getText(), tabellePset.convertRowIndexToModel(row), DatenPset.PROGRAMMSET_NAME_NR);
                        jTabbedPane.setTitleAt(0, "Set Name: " + datenPset.arr[DatenPset.PROGRAMMSET_NAME_NR]);
                    }
                    notifyPset();
                    stopBeob = false;
                } else {
                    new HinweisKeineAuswahl().zeigen(parentComponent);
                }
            }
            setNamePruefen();
        }
    }

    private class BeobGruppeDuplizieren implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            DatenPset gruppe;
            int row = tabellePset.getSelectedRow();
            if (row != -1) {
                gruppe = listePset.get(tabellePset.convertRowIndexToModel(row));
                listePset.addPset(gruppe.copy());
                tabellePset();
                notifyPset();
            } else {
                new HinweisKeineAuswahl().zeigen(parentComponent);
            }
        }
    }

    private class BeobProgLoeschen implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            int rows[] = tabelleProgramme.getSelectedRows();
            if (rows.length > 0) {
                DatenPset pSet = getPset();
                String text;
                if (rows.length == 1) {
                    int delRow = tabelleProgramme.convertRowIndexToModel(rows[0]);
                    text = pSet.getProg(delRow).arr[DatenProg.PROGRAMM_NAME_NR];
                } else {
                    text = rows.length + " Programme löschen?";
                }
                int ret = JOptionPane.showConfirmDialog(parentComponent, text, "Löschen?", JOptionPane.YES_NO_OPTION);
                if (ret == JOptionPane.OK_OPTION) {
                    for (int i = rows.length - 1; i >= 0; --i) {
                        int delRow = tabelleProgramme.convertRowIndexToModel(rows[i]);
                        pSet.getListeProg().remove(delRow);
                    }
                    tabelleProgramme();
                }
            } else {
                new HinweisKeineAuswahl().zeigen(parentComponent);
            }
        }
    }

    private class BeobPuefen implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            GuiFunktionenProgramme.programmePruefen(parentComponent, daten);
        }
    }

    private class BeobProgAufAb implements ActionListener {

        boolean auf;

        public BeobProgAufAb(boolean a) {
            auf = a;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            progAufAb(auf);
        }
    }

    private class BeobGruppeAufAb implements ActionListener {

        boolean auf;

        public BeobGruppeAufAb(boolean a) {
            auf = a;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            setAufAb(auf);
        }
    }

    private class BeobGruppeNeu implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            setNeu();
        }
    }

    private class BeobGruppeLoeschen implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            setLoeschen();
        }
    }

    private class BeobGruppeExport implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            setExport();
        }
    }

    private class BeobachterFarbe implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            DatenPset pSet = getPset();
            if (pSet != null) {
                DialogFarbe dialog = new DialogFarbe(parentComponent, true, pSet.getFarbe());
                dialog.setVisible(true);
                if (dialog.farbe != null) {
                    pSet.setFarbe(dialog.farbe);
                    tabellePset();
                    notifyPset();
                }
            }

        }
    }

    private class BeobStandardfarbe implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            DatenPset pSet = getPset();
            if (pSet != null) {
                pSet.arr[DatenPset.PROGRAMMSET_FARBE_NR] = "";
                tabellePset();
                notifyPset();
            }

        }
    }

    public class BeobTableSelect implements ListSelectionListener {

        @Override
        public void valueChanged(ListSelectionEvent event) {
            if (!event.getValueIsAdjusting()) {
                if (!stopBeob) {
                    fillTextProgramme();
                }
            }

        }
    }
}
