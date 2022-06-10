package mediathek.gui.dialogEinstellungen;

import javafx.scene.control.Alert;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVColor;
import mediathek.config.MVConfig;
import mediathek.controller.IoXmlSchreiben;
import mediathek.controller.starter.RuntimeExec;
import mediathek.daten.DatenProg;
import mediathek.daten.DatenPset;
import mediathek.daten.FilmResolution;
import mediathek.daten.ListePset;
import mediathek.file.GetFile;
import mediathek.gui.PanelVorlage;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.gui.messages.ProgramSetChangedEvent;
import mediathek.javafx.tool.JFXHiddenApplication;
import mediathek.javafx.tool.JavaFxUtils;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.*;
import mediathek.tool.cellrenderer.CellRendererProgramme;
import mediathek.tool.cellrenderer.CellRendererPset;
import mediathek.tool.models.TModel;
import mediathek.tool.table.MVProgTable;
import mediathek.tool.table.MVPsetTable;
import mediathek.tool.table.MVTable;
import net.engio.mbassy.listener.Handler;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.lang3.SystemUtils;

import javax.swing.*;
import javax.swing.border.EtchedBorder;
import javax.swing.border.SoftBevelBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;

public class PanelPsetLang extends PanelVorlage {
    private int neuZaehler;
    private final ListePset listePset;
    private final MVTable tabellePset;
    private final MVTable tabelleProgramme;
    private final boolean modalHilfe;

    public PanelPsetLang(Daten d, JFrame parentComponent, ListePset llistePset) {
        super(d, parentComponent);
        initComponents();
        modalHilfe = true;
        tabellePset = new MVPsetTable();
        jScrollPane3.setViewportView(tabellePset);
        tabelleProgramme = new MVProgTable();
        jScrollPane1.setViewportView(tabelleProgramme);
        listePset = llistePset;
        init();
    }

    @Handler
    private void handleProgramSetChanged(ProgramSetChangedEvent e) {
        SwingUtilities.invokeLater(() -> {
            if (!stopBeob) {
                tabellePset();
            }
        });
    }

    private void init() {
        jButtonHilfe.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg"));
        jButtonGruppePfad.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg"));
        jButtonProgPlus.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/plus.svg"));
        jButtonProgMinus.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/minus.svg"));
        jButtonProgAuf.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/arrow-up.svg"));
        jButtonProgAb.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/arrow-down.svg"));
        jButtonProgPfad.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg"));
        jButtonGruppeNeu.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/plus.svg"));
        jButtonGruppeLoeschen.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/minus.svg"));
        jButtonGruppeAuf.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/arrow-up.svg"));
        jButtonGruppeAb.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/arrow-down.svg"));

        var exclamationIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/triangle-exclamation.svg");
        jLabelMeldungAbspielen.setIcon(exclamationIcon);
        jLabelMeldungSeichern.setIcon(exclamationIcon);

        MessageBus.getMessageBus().subscribe(this);

        //Programme
        tabellePset.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);

        BeobProgDoc beobDoc = new BeobProgDoc();
        jTextFieldProgPfad.getDocument().addDocumentListener(beobDoc);
        jTextFieldProgSchalter.getDocument().addDocumentListener(beobDoc);
        jTextFieldProgName.getDocument().addDocumentListener(beobDoc);
        jTextFieldProgZielDateiName.getDocument().addDocumentListener(beobDoc);
        jTextFieldProgPraefix.getDocument().addDocumentListener(beobDoc);
        jTextFieldProgSuffix.getDocument().addDocumentListener(beobDoc);

        var handler = new TextCopyPasteHandler<>(jTextFieldProgPfad);
        jTextFieldProgPfad.setComponentPopupMenu(handler.getPopupMenu());

        handler = new TextCopyPasteHandler<>(jTextFieldProgSchalter);
        jTextFieldProgSchalter.setComponentPopupMenu(handler.getPopupMenu());

        handler = new TextCopyPasteHandler<>(jTextFieldProgName);
        jTextFieldProgName.setComponentPopupMenu(handler.getPopupMenu());

        handler = new TextCopyPasteHandler<>(jTextFieldProgZielDateiName);
        jTextFieldProgZielDateiName.setComponentPopupMenu(handler.getPopupMenu());

        handler = new TextCopyPasteHandler<>(jTextFieldProgPraefix);
        jTextFieldProgPraefix.setComponentPopupMenu(handler.getPopupMenu());

        handler = new TextCopyPasteHandler<>(jTextFieldProgSuffix);
        jTextFieldProgSuffix.setComponentPopupMenu(handler.getPopupMenu());

        jTextFieldProgPfad.setEnabled(false);
        jTextFieldProgSchalter.setEnabled(false);
        jTextFieldProgName.setEnabled(false);
        jTextFieldProgZielDateiName.setEnabled(false);
        jTextFieldProgPraefix.setEnabled(false);
        jTextFieldProgSuffix.setEnabled(false);

        jButtonProgPfad.addActionListener(l -> {
            String initialFile = "";
            if (!jTextFieldProgPfad.getText().isEmpty()) {
                initialFile = jTextFieldProgPfad.getText();
            }
            var destFile = FileDialogs.chooseLoadFileLocation(MediathekGui.ui(),"Programm auswählen", initialFile);
            if (destFile != null) {
                jTextFieldProgPfad.setText(destFile.getAbsolutePath());
            }
        });

        jButtonProgPlus.addActionListener(l -> {
            DatenProg prog = new DatenProg();
            progNeueZeile(prog);
        });

        jButtonProgMinus.addActionListener(e -> {
            final int[] rows = tabelleProgramme.getSelectedRows();
            if (rows.length > 0) {
                DatenPset pSet = getPset();
                String text;
                if (rows.length == 1) {
                    final int delRow = tabelleProgramme.convertRowIndexToModel(rows[0]);
                    text = pSet.getProg(delRow).arr[DatenProg.PROGRAMM_NAME];
                } else {
                    text = rows.length + " Programme löschen?";
                }
                int ret = JOptionPane.showConfirmDialog(parentComponent, text, "Löschen?", JOptionPane.YES_NO_OPTION);
                if (ret == JOptionPane.OK_OPTION) {
                    for (int i = rows.length - 1; i >= 0; --i) {
                        final int delRow = tabelleProgramme.convertRowIndexToModel(rows[i]);
                        pSet.getListeProg().remove(delRow);
                    }
                    tabelleProgramme();
                }
            } else {
                NoSelectionErrorDialog.show();
            }
        });

        jButtonProgDuplizieren.addActionListener(e -> {
            final int rows = tabelleProgramme.getSelectedRow();
            if (rows != -1) {
                int row = tabelleProgramme.convertRowIndexToModel(rows);
                DatenProg prog = getPset().getListeProg().get(row);
                progNeueZeile(prog.copy());
            } else {
                NoSelectionErrorDialog.show();
            }
        });

        jButtonProgAuf.addActionListener(e -> progAufAb(true));
        jButtonProgAb.addActionListener(e -> progAufAb(false));

        jButtonProgPfad.setEnabled(false);
        jCheckBoxRestart.addActionListener(e -> {
            if (!stopBeob) {
                int rows = tabelleProgramme.getSelectedRow();
                if (rows != -1) {
                    int row = tabelleProgramme.convertRowIndexToModel(rows);
                    DatenProg prog = getPset().getListeProg().get(row);
                    prog.arr[DatenProg.PROGRAMM_RESTART] = Boolean.toString(jCheckBoxRestart.isSelected());
                    tabelleProgramme.getModel().setValueAt(Boolean.toString(jCheckBoxRestart.isSelected()), row, DatenProg.PROGRAMM_RESTART);
                }
            }
        });
        jCheckBoxRemoteDownload.addActionListener(e -> {
            if (!stopBeob) {
                final int rows = tabelleProgramme.getSelectedRow();
                if (rows != -1) {
                    final int modelIndex = tabelleProgramme.convertRowIndexToModel(rows);
                    DatenProg prog = getPset().getListeProg().get(modelIndex);
                    prog.arr[DatenProg.PROGRAMM_DOWNLOADMANAGER] = Boolean.toString(jCheckBoxRemoteDownload.isSelected());
                    tabelleProgramme.getModel().setValueAt(Boolean.toString(jCheckBoxRemoteDownload.isSelected()), modelIndex, DatenProg.PROGRAMM_DOWNLOADMANAGER);
                }
            }
        });

        //Pset
        jButtonAbspielen.addActionListener(e -> {
            jButtonAbspielen.setBackground(MVColor.BUTTON_SET_ABSPIELEN.color);
            DatenPset pset = getPset();
            if (pset != null) {
                pset.setAbspielen();
                nurtabellePset();
                notifyProgramSetChanged();
            }
        });
        jCheckBoxSpeichern.addActionListener(e -> {
            DatenPset pset = getPset();
            if (pset != null) {
                pset.setSpeichern(jCheckBoxSpeichern.isSelected());
                nurtabellePset();
                notifyProgramSetChanged();
            }
        });
        jCheckBoxButton.addActionListener(e -> {
            DatenPset pset = getPset();
            if (pset != null) {
                pset.setButton(jCheckBoxButton.isSelected());
                nurtabellePset();
                notifyProgramSetChanged();
            }
        });
        jCheckBoxAbo.addActionListener(e -> {
            DatenPset pset = getPset();
            if (pset != null) {
                pset.setAbo(jCheckBoxAbo.isSelected());
                nurtabellePset();
                notifyProgramSetChanged();
            }
        });
        jCheckBoxLaenge.addActionListener(e -> {
            DatenPset pset = getPset();
            if (pset != null) {
                pset.arr[DatenPset.PROGRAMMSET_LAENGE_BESCHRAENKEN] = Boolean.toString(jCheckBoxLaenge.isSelected());
                nurtabellePset();
            }
        });
        jCheckBoxField.addActionListener(e -> {
            DatenPset pset = getPset();
            if (pset != null) {
                pset.arr[DatenPset.PROGRAMMSET_LAENGE_FIELD_BESCHRAENKEN] = Boolean.toString(jCheckBoxField.isSelected());
                nurtabellePset();
            }
        });
        jCheckBoxThema.addActionListener(e -> {
            DatenPset pset = getPset();
            if (pset != null) {
                pset.arr[DatenPset.PROGRAMMSET_THEMA_ANLEGEN] = Boolean.toString(jCheckBoxThema.isSelected());
                nurtabellePset();
            }
        });
        jSpinnerLaenge.addChangeListener(e -> {
            DatenPset pset = getPset();
            if (pset != null) {
                pset.arr[DatenPset.PROGRAMMSET_MAX_LAENGE] = String.valueOf(((Number) jSpinnerLaenge.getModel().getValue()).intValue());
            }
        });
        jSpinnerField.addChangeListener(e -> {
            DatenPset pset = getPset();
            if (pset != null) {
                pset.arr[DatenPset.PROGRAMMSET_MAX_LAENGE_FIELD] = String.valueOf(((Number) jSpinnerField.getModel().getValue()).intValue());
            }
        });
        jCheckBoxInfodatei.addActionListener(e -> {
            DatenPset pset = getPset();
            if (pset != null) {
                pset.arr[DatenPset.PROGRAMMSET_INFODATEI] = Boolean.toString(jCheckBoxInfodatei.isSelected());
                nurtabellePset();
            }
        });
        jCheckBoxSubtitle.addActionListener(e -> {
            DatenPset pset = getPset();
            if (pset != null) {
                pset.arr[DatenPset.PROGRAMMSET_SUBTITLE] = Boolean.toString(jCheckBoxSubtitle.isSelected());
                nurtabellePset();
            }
        });

        jCheckBoxSpotlight.setEnabled(SystemUtils.IS_OS_MAC_OSX);
        jCheckBoxSpotlight.addActionListener(e -> {
            DatenPset pset = getPset();
            if (pset != null) {
                pset.arr[DatenPset.PROGRAMMSET_SPOTLIGHT] = Boolean.toString(jCheckBoxSpotlight.isSelected());
                nurtabellePset();
            }
        });

        jButtonGruppeNeu.addActionListener(e -> setNeu());
        jButtonGruppeLoeschen.addActionListener(e -> setLoeschen());
        jButtonGruppeFarbe.addActionListener(e -> {
            DatenPset pSet = getPset();
            if (pSet != null) {
                var selectedColor = JColorChooser.showDialog(PanelPsetLang.this, "Farbe auswählen", pSet.getFarbe());
                if (selectedColor != null) {
                    pSet.setFarbe(selectedColor);
                    tabellePset();
                    notifyProgramSetChanged();
                }
            }
        });
        jButtonGruppeStandardfarbe.addActionListener(l -> {
            DatenPset pSet = getPset();
            if (pSet != null) {
                pSet.arr[DatenPset.PROGRAMMSET_FARBE] = "";
                tabellePset();
                notifyProgramSetChanged();
            }
        });

        jButtonGruppeAuf.addActionListener(e -> setAufAb(true));
        jButtonGruppeAb.addActionListener(e -> setAufAb(false));

        jButtonGruppeDuplizieren.addActionListener(e -> {
            final int row = tabellePset.getSelectedRow();
            if (row != -1) {
                var gruppe = listePset.get(tabellePset.convertRowIndexToModel(row));
                listePset.addPset(gruppe.copy());
                tabellePset();
                notifyProgramSetChanged();
            } else {
                NoSelectionErrorDialog.show();
            }
        });

        jButtonExport.addActionListener(e -> setExport());

        jButtonGruppePfad.addActionListener(l -> {
            var initialFile = "";
            if (!tfGruppeZielPfad.getText().isEmpty()) {
                initialFile = tfGruppeZielPfad.getText();
            }
            var destDirectory = FileDialogs.chooseDirectoryLocation(MediathekGui.ui(), "Filme speichern unter", initialFile);
            if (destDirectory != null) {
                tfGruppeZielPfad.setText(destDirectory.getAbsolutePath());
            }
        });

        jTextAreaSetBeschreibung.getDocument().addDocumentListener(new BeobDoc(jTextAreaSetBeschreibung, DatenPset.PROGRAMMSET_BESCHREIBUNG));
        var handler2 = new TextCopyPasteHandler<>(jTextAreaSetBeschreibung);
        jTextAreaSetBeschreibung.setComponentPopupMenu(handler2.getPopupMenu());

        tfGruppeDirektSuffix.getDocument().addDocumentListener(
                new BeobDoc(tfGruppeDirektSuffix, DatenPset.PROGRAMMSET_SUFFIX_DIREKT, false));
        tfGruppeDirektPraefix.getDocument().addDocumentListener(
                new BeobDoc(tfGruppeDirektPraefix, DatenPset.PROGRAMMSET_PRAEFIX_DIREKT, false));

        tfGruppeZielName.getDocument().addDocumentListener(new BeobDoc(tfGruppeZielName,
                DatenPset.PROGRAMMSET_ZIEL_DATEINAME, false));

        tfGruppeZielPfad.getDocument().addDocumentListener(
                new BeobDoc(tfGruppeZielPfad, DatenPset.PROGRAMMSET_ZIEL_PFAD, false));

        jTextFieldSetName.getDocument().addDocumentListener(new BeobDoc(jTextFieldSetName, DatenPset.PROGRAMMSET_NAME));
        handler = new TextCopyPasteHandler<>(jTextFieldSetName);
        jTextFieldSetName.setComponentPopupMenu(handler.getPopupMenu());

        handler = new TextCopyPasteHandler<>(tfGruppeDirektSuffix);
        tfGruppeDirektSuffix.setComponentPopupMenu(handler.getPopupMenu());

        handler = new TextCopyPasteHandler<>(tfGruppeDirektPraefix);
        tfGruppeDirektPraefix.setComponentPopupMenu(handler.getPopupMenu());

        handler = new TextCopyPasteHandler<>(tfGruppeZielName);
        tfGruppeZielName.setComponentPopupMenu(handler.getPopupMenu());

        handler = new TextCopyPasteHandler<>(tfGruppeZielPfad);
        tfGruppeZielPfad.setComponentPopupMenu(handler.getPopupMenu());

        jButtonHilfe.addActionListener(e -> new DialogHilfe(parentComponent, modalHilfe, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_PRGRAMME)).setVisible(true));
        jRadioButtonAufloesungKlein.addActionListener(e -> setAufloesung());
        jRadioButtonAufloesungNormal.addActionListener(e -> setAufloesung());
        jRadioButtonAufloesungHD.addActionListener(e -> setAufloesung());
        jButtonPruefen.addActionListener(l -> programmePruefen());


        tabelleProgramme.getSelectionModel().addListSelectionListener(e -> {
            if (!e.getValueIsAdjusting()) {
                if (!stopBeob) {
                    fillTextProgramme();
                }
            }
        });
        tabelleProgramme.setDefaultRenderer(Object.class, new CellRendererProgramme());

        tabellePset.setDefaultRenderer(Object.class, new CellRendererPset());
        tabellePset.getSelectionModel().addListSelectionListener(event -> {
            if (!stopBeob) {
                if (!event.getValueIsAdjusting()) {
                    tabelleProgramme();
                    DatenPset datenPset;
                    int row = tabellePset.getSelectedRow();
                    if (row != -1) {
                        datenPset = listePset.get(tabellePset.convertRowIndexToModel(row));
                        tabellePset.getModel().setValueAt(jTextFieldSetName.getText(), tabellePset.convertRowIndexToModel(row), DatenPset.PROGRAMMSET_NAME);
                        jTabbedPane.setTitleAt(0, "Set Name: " + datenPset.arr[DatenPset.PROGRAMMSET_NAME]);
                    }
                }
            }
        });
        tabellePset();

        if (tabellePset.getRowCount() > 0) {
            tabellePset.setRowSelectionInterval(0, 0);
            tabellePset.scrollRectToVisible(tabellePset.getCellRect(0, 0, false));
        }
    }

    /**
     * Prüfen ob die eingestellten Programmsets passen
     */
    public void programmePruefen() {
        final String PIPE = "| ";
        final String LEER = "      ";
        final String PFEIL = " -> ";
        boolean ret;
        StringBuilder text = new StringBuilder();

        for (DatenPset datenPset : Daten.listePset) {
            ret = true;
            if (!datenPset.isFreeLine() && !datenPset.isLabel()) {
                // nur wenn kein Label oder freeline
                text.append("++++++++++++++++++++++++++++++++++++++++++++" + '\n');
                text.append(PIPE + "Programmgruppe: ").append(datenPset.arr[DatenPset.PROGRAMMSET_NAME]).append('\n');
                String zielPfad = datenPset.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD];
                if (datenPset.progsContainPath()) {
                    // beim nur Abspielen wird er nicht gebraucht
                    if (zielPfad.isEmpty()) {
                        ret = false;
                        text.append(PIPE + LEER + "Zielpfad fehlt!\n");
                    } else // Pfad beschreibbar?
                        if (!GuiFunktionenProgramme.checkPathWriteable(zielPfad)) {
                            //da Pfad-leer und "kein" Pfad schon abgeprüft
                            ret = false;
                            text.append(PIPE + LEER + "Falscher Zielpfad!\n");
                            text.append(PIPE + LEER + PFEIL + "Zielpfad \"").append(zielPfad).append("\" nicht beschreibbar!").append('\n');
                        }
                }

                for (DatenProg datenProg : datenPset.getListeProg()) {
                    // Programmpfad prüfen
                    final var progPfad = datenProg.arr[DatenProg.PROGRAMM_PROGRAMMPFAD];
                    final var progName = datenProg.arr[DatenProg.PROGRAMM_NAME];
                    if (progPfad.isEmpty()) {
                        ret = false;
                        text.append(PIPE + LEER + "Kein Programm angegeben!\n");
                        text.append(PIPE + LEER + PFEIL + "Programmname: ").append(progName).append('\n');
                        text.append(PIPE + LEER + LEER + "Pfad: ").append(progPfad).append('\n');
                    } else if (!Files.isExecutable(Paths.get(progPfad))) {
                        // dann noch mit RuntimeExec versuchen
                        RuntimeExec r = new RuntimeExec(progPfad);
                        Process pr = r.exec(false);
                        if (pr == null) {
                            // läßt sich nicht starten
                            ret = false;
                            text.append(PIPE + LEER + "Falscher Programmpfad!\n");
                            text.append(PIPE + LEER + PFEIL + "Programmname: ").append(progName).append('\n');
                            text.append(PIPE + LEER + LEER + "Pfad: ").append(progPfad).append('\n');
                            if (!progPfad.contains(File.separator)) {
                                text.append(PIPE + LEER + PFEIL + "Wenn das Programm nicht im Systempfad liegt, " + '\n');
                                text.append(PIPE + LEER + LEER + "wird der Start nicht klappen!" + '\n');
                            }
                        }
                        else
                            pr.destroy();
                    }
                }
                if (ret) {
                    //sollte alles passen
                    text.append(PIPE + PFEIL + "Ok!" + '\n');
                }
                text.append("""
                        ++++++++++++++++++++++++++++++++++++++++++++


                        """);
            }
        }

        var dlg = new DialogHilfe(parentComponent, true, text.toString());
        dlg.setVisible(true);
    }

    private void setAufloesung() {
        if (jRadioButtonAufloesungNormal.isSelected()) {
            DatenPset pset = getPset();
            if (pset != null) {
                pset.arr[DatenPset.PROGRAMMSET_AUFLOESUNG] = FilmResolution.Enum.NORMAL.toString();
            }
        }
        if (jRadioButtonAufloesungHD.isSelected()) {
            DatenPset pset = getPset();
            if (pset != null) {
                pset.arr[DatenPset.PROGRAMMSET_AUFLOESUNG] = FilmResolution.Enum.HIGH_QUALITY.toString();
            }
        }
        if (jRadioButtonAufloesungKlein.isSelected()) {
            DatenPset pset = getPset();
            if (pset != null) {
                pset.arr[DatenPset.PROGRAMMSET_AUFLOESUNG] = FilmResolution.Enum.LOW.toString();
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
        jLabelMeldungSeichern.setVisible(listePset.getListeSpeichern().isEmpty());
        stopBeob = false;
    }

    private void spaltenSetzen() {
        for (int i = 0; i < tabellePset.getColumnCount(); ++i) {
            if (i == DatenPset.PROGRAMMSET_NAME) {
                tabellePset.getColumnModel().getColumn(tabellePset.convertColumnIndexToView(i)).setMinWidth(10);
                tabellePset.getColumnModel().getColumn(tabellePset.convertColumnIndexToView(i)).setPreferredWidth(120);
                tabellePset.getColumnModel().getColumn(tabellePset.convertColumnIndexToView(i)).setMaxWidth(1000);
            } else if (i == DatenPset.PROGRAMMSET_IST_ABSPIELEN
                    || i == DatenPset.PROGRAMMSET_IST_SPEICHERN) {
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

        GuiFunktionen.enableComponents(jTabbedPane, pSet != null);
        jButtonAbspielen.setBackground(null);
        if (pSet != null) {
            jTabbedPane.setTitleAt(0, "Set Name: " + pSet.arr[DatenPset.PROGRAMMSET_NAME]);
            if (pSet.arr[DatenPset.PROGRAMMSET_MAX_LAENGE].equals("")) {
                jSpinnerLaenge.setValue(Konstanten.LAENGE_DATEINAME);
                pSet.arr[DatenPset.PROGRAMMSET_MAX_LAENGE] = String.valueOf(Konstanten.LAENGE_DATEINAME);
            } else {
                jSpinnerLaenge.setValue(Integer.parseInt(pSet.arr[DatenPset.PROGRAMMSET_MAX_LAENGE]));
            }
            if (pSet.arr[DatenPset.PROGRAMMSET_MAX_LAENGE_FIELD].equals("")) {
                jSpinnerField.setValue(Konstanten.LAENGE_FELD);
                pSet.arr[DatenPset.PROGRAMMSET_MAX_LAENGE_FIELD] = String.valueOf(Konstanten.LAENGE_FELD);
            } else {
                jSpinnerField.setValue(Integer.parseInt(pSet.arr[DatenPset.PROGRAMMSET_MAX_LAENGE_FIELD]));
            }
            jCheckBoxLaenge.setSelected(Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_LAENGE_BESCHRAENKEN]));
            jCheckBoxField.setSelected(Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_LAENGE_FIELD_BESCHRAENKEN]));
            jCheckBoxThema.setSelected(Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_THEMA_ANLEGEN]));
            jCheckBoxInfodatei.setSelected(Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_INFODATEI]));
            jCheckBoxSubtitle.setSelected(Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_SUBTITLE]));
            jCheckBoxSpotlight.setEnabled(SystemUtils.IS_OS_MAC_OSX);
            jCheckBoxSpotlight.setSelected(Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_SPOTLIGHT]));
            jScrollPane1.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Set Name: " + pSet.arr[DatenPset.PROGRAMMSET_NAME], javax.swing.border.TitledBorder.LEFT, javax.swing.border.TitledBorder.TOP));
            jTextFieldSetName.setText(pSet.arr[DatenPset.PROGRAMMSET_NAME]);
            tfGruppeDirektSuffix.setText(pSet.arr[DatenPset.PROGRAMMSET_SUFFIX_DIREKT]);
            tfGruppeDirektPraefix.setText(pSet.arr[DatenPset.PROGRAMMSET_PRAEFIX_DIREKT]);
            tfGruppeZielName.setText(pSet.arr[DatenPset.PROGRAMMSET_ZIEL_DATEINAME]);
            tfGruppeZielPfad.setText(pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD]);
            jTextAreaSetBeschreibung.setText(pSet.arr[DatenPset.PROGRAMMSET_BESCHREIBUNG]);

            jCheckBoxSpeichern.setSelected(pSet.istSpeichern());
            jCheckBoxButton.setSelected(pSet.istButton());
            jCheckBoxAbo.setSelected(pSet.istAbo());
            jButtonAbspielen.setBackground(pSet.istAbspielen() ? MVColor.BUTTON_SET_ABSPIELEN.color : null);
            switch (pSet.arr[DatenPset.PROGRAMMSET_AUFLOESUNG]) {
                case FilmResolution.HIGH_QUALITY -> jRadioButtonAufloesungHD.setSelected(true);
                case FilmResolution.LOW -> jRadioButtonAufloesungKlein.setSelected(true);
                default -> jRadioButtonAufloesungNormal.setSelected(true);
            }
            tabelleProgramme.setModel(pSet.getListeProg().getModel());
            if (tabelleProgramme.getRowCount() > 0) {
                spaltenSetzenProgramme();
                tabelleProgramme.setRowSelectionInterval(0, 0);
                tabelleProgramme.scrollRectToVisible(tabelleProgramme.getCellRect(0, 0, true));
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
            tfGruppeDirektSuffix.setText("");
            tfGruppeDirektPraefix.setText("");
            tfGruppeZielName.setText("");
            tfGruppeZielPfad.setText("");
            jTextAreaSetBeschreibung.setText("");
            tabelleProgramme.setModel(new TModel(new Object[0][DatenProg.MAX_ELEM], DatenProg.COLUMN_NAMES));
        }
        stopBeob = false;
        fillTextProgramme();
    }

    public void spaltenSetzenProgramme() {
        for (int i = 0; i < tabelleProgramme.getColumnCount(); ++i) {
            if (i == DatenProg.PROGRAMM_PRAEFIX
                    || i == DatenProg.PROGRAMM_RESTART
                    || i == DatenProg.PROGRAMM_DOWNLOADMANAGER
                    || i == DatenProg.PROGRAMM_SUFFIX) {
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

    /**
     * Send message that changes to the Pset were performed.
     */
    private void notifyProgramSetChanged() {
        MessageBus.getMessageBus().publishAsync(new ProgramSetChangedEvent());
    }

    private void fillTextProgramme() {
        //Textfelder mit Programmdaten füllen
        stopBeob = true;
        final int row = tabelleProgramme.getSelectedRow();
        final boolean letzteZeile = tabelleProgramme.getRowCount() <= 1 || row == tabelleProgramme.getRowCount() - 1;

        jTextFieldProgPfad.setEnabled(row != -1);
        jTextFieldProgSchalter.setEnabled(row != -1);
        jTextFieldProgZielDateiName.setEnabled(row != -1);
        jTextFieldProgName.setEnabled(row != -1);
        jTextFieldProgZielDateiName.setEnabled(row != -1);
        jTextFieldProgPraefix.setEnabled(row != -1);
        jTextFieldProgSuffix.setEnabled(row != -1);
        jButtonProgPfad.setEnabled(row != -1);
        jCheckBoxRestart.setEnabled(row != -1);
        jCheckBoxRemoteDownload.setEnabled(row != -1);
        if (row != -1) {
            DatenProg prog = getPset().getProg(tabelleProgramme.convertRowIndexToModel(row));
            jTextFieldProgPfad.setText(prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD]);
            jTextFieldProgSchalter.setText(prog.arr[DatenProg.PROGRAMM_SCHALTER]);
            jTextFieldProgZielDateiName.setText(prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME]);
            jTextFieldProgName.setText(prog.arr[DatenProg.PROGRAMM_NAME]);
            jTextFieldProgZielDateiName.setText(prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME]);
            jTextFieldProgPraefix.setText(prog.arr[DatenProg.PROGRAMM_PRAEFIX]);
            jTextFieldProgSuffix.setText(prog.arr[DatenProg.PROGRAMM_SUFFIX]);
            jCheckBoxRestart.setSelected(prog.isRestart());
            jCheckBoxRemoteDownload.setSelected(prog.isDownloadManager());
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

    private void setAufAb(boolean auf) {
        int row = tabellePset.getSelectedRow();
        if (row != -1) {
            int neu = listePset.auf(tabellePset.convertRowIndexToModel(row), auf);
            neu = tabellePset.convertRowIndexToView(neu);
            tabellePset.setRowSelectionInterval(neu, neu);
            tabellePset.scrollRectToVisible(tabellePset.getCellRect(neu, 0, false));
            notifyProgramSetChanged();
        } else {
            NoSelectionErrorDialog.show();
        }
    }

    private void setNeu() {
        listePset.addPset(new DatenPset("Neu-" + ++neuZaehler));
        tabellePset();
        notifyProgramSetChanged();
    }

    private void setLoeschen() {
        int[] rows = tabellePset.getSelectedRows();
        if (rows.length > 0) {
            DatenPset pSet;
            String text;
            if (rows.length == 1) {
                pSet = listePset.get(tabellePset.convertRowIndexToModel(rows[0]));
                text = pSet.arr[DatenPset.PROGRAMMSET_NAME];
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
                notifyProgramSetChanged();
            }
        } else {
            NoSelectionErrorDialog.show();
        }
    }

    private void setExport() {
        ArrayList<DatenPset> liste = new ArrayList<>();
        int[] rows = tabellePset.getSelectedRows();
        if (rows.length > 0) {
            DatenPset pSet;
            for (int row : rows) {
                int delRow = tabellePset.convertRowIndexToModel(row);
                pSet = listePset.get(delRow);
                if (pSet != null) {
                    liste.add(pSet);
                }
            }

            final var entryName = liste.get(0).arr[DatenPset.PROGRAMMSET_NAME];
            String name = entryName.isEmpty() ? "Name.xml" : entryName + ".xml";
            var fileName = FilenameUtils.replaceLeerDateiname(name, false,
                    Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_REPLACETABLE)),
                    Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ONLY_ASCII)));
            var resultFile = FileDialogs.chooseSaveFileLocation(parentComponent,"PSet exportieren", fileName);
            if (resultFile != null) {
                var ziel = resultFile.getAbsolutePath();

                IoXmlSchreiben configWriter = new IoXmlSchreiben();
                configWriter.exportPset(liste.toArray(new DatenPset[0]), ziel);
                JavaFxUtils.invokeInFxThreadAndWait(() -> {
                    Alert alert = new Alert(Alert.AlertType.INFORMATION);
                    alert.setHeaderText("Programmset exportieren");
                    alert.setContentText("Das Programmset wurde erfolgreich exportiert.");
                    JFXHiddenApplication.showAlert(alert, MediathekGui.ui());
                });
            }
        } else {
            NoSelectionErrorDialog.show();
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
            NoSelectionErrorDialog.show();
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
                    prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD] = jTextFieldProgPfad.getText();
                    prog.arr[DatenProg.PROGRAMM_SCHALTER] = jTextFieldProgSchalter.getText();
                    prog.arr[DatenProg.PROGRAMM_NAME] = jTextFieldProgName.getText();
                    prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME] = jTextFieldProgZielDateiName.getText();
                    prog.arr[DatenProg.PROGRAMM_SUFFIX] = jTextFieldProgSuffix.getText();
                    prog.arr[DatenProg.PROGRAMM_PRAEFIX] = jTextFieldProgPraefix.getText();
                    tabelleProgramme.getModel().setValueAt(jTextFieldProgPfad.getText(), row, DatenProg.PROGRAMM_PROGRAMMPFAD);
                    tabelleProgramme.getModel().setValueAt(jTextFieldProgSchalter.getText(), row, DatenProg.PROGRAMM_SCHALTER);
                    tabelleProgramme.getModel().setValueAt(jTextFieldProgName.getText(), row, DatenProg.PROGRAMM_NAME);
                    tabelleProgramme.getModel().setValueAt(jTextFieldProgZielDateiName.getText(), row, DatenProg.PROGRAMM_ZIEL_DATEINAME);
                    tabelleProgramme.getModel().setValueAt(jTextFieldProgSuffix.getText(), row, DatenProg.PROGRAMM_SUFFIX);
                    tabelleProgramme.getModel().setValueAt(jTextFieldProgPraefix.getText(), row, DatenProg.PROGRAMM_PRAEFIX);
//                    progNamePruefen();
                }
            }
        }
    }

    private class BeobDoc implements DocumentListener {

        private final JTextComponent textComponent;
        private final int psetIndex;
        private final boolean fireUpdate;

        public BeobDoc(JTextComponent comp, int psetIndex, boolean fireUpdate) {
            this.psetIndex = psetIndex;
            textComponent = comp;
            this.fireUpdate = fireUpdate;
        }

        public BeobDoc(JTextComponent comp, int psetIndex) {
            this(comp, psetIndex, true);
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
            //unused in plaintext components
        }

        private void setNamePruefen() {
            //doppelte Gruppennamen suchen
            int row = tabellePset.getSelectedRow();
            if (row != -1) {
                int foundgruppe = 0;
                for (DatenPset gruppe : listePset) {
                    if (jTextFieldSetName.getText().equals(gruppe.arr[DatenPset.PROGRAMMSET_NAME])) {
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

        private void eingabe() {
            if (!stopBeob) {
                final int row = tabellePset.getSelectedRow();
                if (row != -1) {
                    stopBeob = true;
                    final int modelIndex = tabellePset.convertRowIndexToModel(row);
                    var datenPset = listePset.get(modelIndex);
                    datenPset.arr[psetIndex] = textComponent.getText();
                    if (psetIndex == DatenPset.PROGRAMMSET_NAME) {
                        tabellePset.getModel().setValueAt(jTextFieldSetName.getText(), modelIndex, DatenPset.PROGRAMMSET_NAME);
                        jTabbedPane.setTitleAt(0, "Set Name: " + datenPset.arr[DatenPset.PROGRAMMSET_NAME]);
                    }
                    if (fireUpdate)
                        notifyProgramSetChanged();
                    stopBeob = false;
                } else {
                    NoSelectionErrorDialog.show();
                }
            }
            setNamePruefen();
        }
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        jButtonHilfe = new JButton();
        jButtonPruefen = new JButton();
        var jSplitPane1 = new JSplitPane();
        jTabbedPane = new JTabbedPane();
        var jPanelDetails = new JPanel();
        var jPanel7 = new JPanel();
        var jLabel6 = new JLabel();
        jTextFieldSetName = new JTextField();
        var jPanel4 = new JPanel();
        var jScrollPane2 = new JScrollPane();
        jTextAreaSetBeschreibung = new JTextArea();
        var jPanel6 = new JPanel();
        jCheckBoxSpeichern = new JCheckBox();
        jCheckBoxButton = new JCheckBox();
        jCheckBoxAbo = new JCheckBox();
        jButtonAbspielen = new JButton();
        jLabelMeldungAbspielen = new JLabel();
        jLabelMeldungSeichern = new JLabel();
        var jPanel10 = new JPanel();
        var jPanel5 = new JPanel();
        jButtonGruppeFarbe = new JButton();
        jButtonGruppeStandardfarbe = new JButton();
        var jLabel11 = new JLabel();
        var jLabel13 = new JLabel();
        var jPanel9 = new JPanel();
        var jPanel1 = new JPanel();
        var jLabel7 = new JLabel();
        tfGruppeZielPfad = new JTextField();
        jButtonGruppePfad = new JButton();
        jCheckBoxThema = new JCheckBox();
        var jLabel8 = new JLabel();
        tfGruppeZielName = new JTextField();
        jCheckBoxLaenge = new JCheckBox();
        jSpinnerLaenge = new JSpinner();
        var jLabel12 = new JLabel();
        var jLabel15 = new JLabel();
        jCheckBoxField = new JCheckBox();
        jSpinnerField = new JSpinner();
        var jLabel16 = new JLabel();
        var jPanel11 = new JPanel();
        var jPanel8 = new JPanel();
        var jLabel10 = new JLabel();
        tfGruppeDirektPraefix = new JTextField();
        var jLabel5 = new JLabel();
        tfGruppeDirektSuffix = new JTextField();
        var jTextArea1 = new JTextArea();
        var jPanel12 = new JPanel();
        jRadioButtonAufloesungNormal = new JRadioButton();
        jRadioButtonAufloesungKlein = new JRadioButton();
        jRadioButtonAufloesungHD = new JRadioButton();
        var jLabel14 = new JLabel();
        var jPanel13 = new JPanel();
        jCheckBoxInfodatei = new JCheckBox();
        jCheckBoxSpotlight = new JCheckBox();
        jCheckBoxSubtitle = new JCheckBox();
        var jPanelProgramme = new JPanel();
        jScrollPane1 = new JScrollPane();
        var jTableProgramme = new JTable();
        var jPanel2 = new JPanel();
        jButtonProgPlus = new JButton();
        jButtonProgMinus = new JButton();
        jButtonProgDuplizieren = new JButton();
        jButtonProgAuf = new JButton();
        jButtonProgAb = new JButton();
        var jPanelProgrammDetails = new JPanel();
        var jLabel = new JLabel();
        jTextFieldProgPfad = new JTextField();
        jButtonProgPfad = new JButton();
        jTextFieldProgSchalter = new JTextField();
        var jLabel1 = new JLabel();
        var jLabel2 = new JLabel();
        jTextFieldProgName = new JTextField();
        var jLabel3 = new JLabel();
        jTextFieldProgPraefix = new JTextField();
        var jLabel4 = new JLabel();
        jTextFieldProgSuffix = new JTextField();
        jCheckBoxRestart = new JCheckBox();
        var jLabel9 = new JLabel();
        jTextFieldProgZielDateiName = new JTextField();
        jCheckBoxRemoteDownload = new JCheckBox();
        var jPanel3 = new JPanel();
        jScrollPane3 = new JScrollPane();
        var jTablePset = new JTable();
        jButtonGruppeDuplizieren = new JButton();
        jButtonExport = new JButton();
        jButtonGruppeNeu = new JButton();
        jButtonGruppeLoeschen = new JButton();
        jButtonGruppeAuf = new JButton();
        var hSpacer1 = new JPanel(null);
        jButtonGruppeAb = new JButton();

        //======== this ========

        //---- jButtonHilfe ----
        jButtonHilfe.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-help.png"))); //NON-NLS
        jButtonHilfe.setToolTipText("Hilfe anzeigen"); //NON-NLS

        //---- jButtonPruefen ----
        jButtonPruefen.setText("Pr\u00fcfen"); //NON-NLS
        jButtonPruefen.setToolTipText("Programmpfade pr\u00fcfen"); //NON-NLS

        //======== jSplitPane1 ========
        {
            jSplitPane1.setDividerLocation(290);

            //======== jTabbedPane ========
            {

                //======== jPanelDetails ========
                {
                    jPanelDetails.setBorder(new SoftBevelBorder(SoftBevelBorder.RAISED));

                    //======== jPanel7 ========
                    {
                        jPanel7.setBorder(new EtchedBorder());

                        //---- jLabel6 ----
                        jLabel6.setText("Set Name:"); //NON-NLS

                        //======== jPanel4 ========
                        {
                            jPanel4.setBorder(new TitledBorder("Beschreibung")); //NON-NLS

                            //======== jScrollPane2 ========
                            {

                                //---- jTextAreaSetBeschreibung ----
                                jTextAreaSetBeschreibung.setColumns(20);
                                jTextAreaSetBeschreibung.setRows(8);
                                jTextAreaSetBeschreibung.setPreferredSize(new Dimension(160, 130));
                                jTextAreaSetBeschreibung.setMinimumSize(new Dimension(50, 50));
                                jScrollPane2.setViewportView(jTextAreaSetBeschreibung);
                            }

                            GroupLayout jPanel4Layout = new GroupLayout(jPanel4);
                            jPanel4.setLayout(jPanel4Layout);
                            jPanel4Layout.setHorizontalGroup(
                                jPanel4Layout.createParallelGroup()
                                    .addGroup(jPanel4Layout.createSequentialGroup()
                                        .addContainerGap()
                                        .addComponent(jScrollPane2, GroupLayout.DEFAULT_SIZE, 567, Short.MAX_VALUE)
                                        .addContainerGap())
                            );
                            jPanel4Layout.setVerticalGroup(
                                jPanel4Layout.createParallelGroup()
                                    .addGroup(jPanel4Layout.createSequentialGroup()
                                        .addComponent(jScrollPane2, GroupLayout.DEFAULT_SIZE, 211, Short.MAX_VALUE)
                                        .addContainerGap())
                            );
                        }

                        //======== jPanel6 ========
                        {
                            jPanel6.setBorder(new TitledBorder("Funktion")); //NON-NLS

                            //---- jCheckBoxSpeichern ----
                            jCheckBoxSpeichern.setText("Speichern"); //NON-NLS

                            //---- jCheckBoxButton ----
                            jCheckBoxButton.setText("Button"); //NON-NLS

                            //---- jCheckBoxAbo ----
                            jCheckBoxAbo.setText("Abo"); //NON-NLS

                            //---- jButtonAbspielen ----
                            jButtonAbspielen.setText("Abspielen via schwarzem Play-Button"); //NON-NLS

                            GroupLayout jPanel6Layout = new GroupLayout(jPanel6);
                            jPanel6.setLayout(jPanel6Layout);
                            jPanel6Layout.setHorizontalGroup(
                                jPanel6Layout.createParallelGroup()
                                    .addGroup(jPanel6Layout.createSequentialGroup()
                                        .addContainerGap()
                                        .addComponent(jButtonAbspielen)
                                        .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                        .addComponent(jCheckBoxSpeichern)
                                        .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                        .addComponent(jCheckBoxButton)
                                        .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                        .addComponent(jCheckBoxAbo)
                                        .addContainerGap(71, Short.MAX_VALUE))
                            );
                            jPanel6Layout.setVerticalGroup(
                                jPanel6Layout.createParallelGroup()
                                    .addGroup(jPanel6Layout.createSequentialGroup()
                                        .addContainerGap()
                                        .addGroup(jPanel6Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                            .addComponent(jCheckBoxSpeichern)
                                            .addComponent(jCheckBoxButton)
                                            .addComponent(jCheckBoxAbo)
                                            .addComponent(jButtonAbspielen))
                                        .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                            );
                        }

                        //---- jLabelMeldungAbspielen ----
                        jLabelMeldungAbspielen.setText("kein Set zum Abspielen ausgew\u00e4hlt!"); //NON-NLS

                        //---- jLabelMeldungSeichern ----
                        jLabelMeldungSeichern.setText("kein Set zum Speichern ausgew\u00e4hlt!"); //NON-NLS

                        GroupLayout jPanel7Layout = new GroupLayout(jPanel7);
                        jPanel7.setLayout(jPanel7Layout);
                        jPanel7Layout.setHorizontalGroup(
                            jPanel7Layout.createParallelGroup()
                                .addGroup(jPanel7Layout.createSequentialGroup()
                                    .addContainerGap()
                                    .addGroup(jPanel7Layout.createParallelGroup()
                                        .addGroup(jPanel7Layout.createSequentialGroup()
                                            .addComponent(jLabel6)
                                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                            .addComponent(jTextFieldSetName))
                                        .addComponent(jPanel4, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                        .addComponent(jPanel6, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                        .addGroup(jPanel7Layout.createSequentialGroup()
                                            .addGroup(jPanel7Layout.createParallelGroup()
                                                .addComponent(jLabelMeldungAbspielen)
                                                .addComponent(jLabelMeldungSeichern))
                                            .addGap(0, 0, Short.MAX_VALUE)))
                                    .addContainerGap())
                        );
                        jPanel7Layout.setVerticalGroup(
                            jPanel7Layout.createParallelGroup()
                                .addGroup(jPanel7Layout.createSequentialGroup()
                                    .addContainerGap()
                                    .addGroup(jPanel7Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                        .addComponent(jTextFieldSetName, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                        .addComponent(jLabel6))
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                    .addComponent(jPanel4, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addGap(18, 18, 18)
                                    .addComponent(jPanel6, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                    .addComponent(jLabelMeldungAbspielen)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                    .addComponent(jLabelMeldungSeichern)
                                    .addGap(38, 38, 38))
                        );
                    }

                    GroupLayout jPanelDetailsLayout = new GroupLayout(jPanelDetails);
                    jPanelDetails.setLayout(jPanelDetailsLayout);
                    jPanelDetailsLayout.setHorizontalGroup(
                        jPanelDetailsLayout.createParallelGroup()
                            .addGroup(jPanelDetailsLayout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jPanel7, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addContainerGap())
                    );
                    jPanelDetailsLayout.setVerticalGroup(
                        jPanelDetailsLayout.createParallelGroup()
                            .addGroup(jPanelDetailsLayout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jPanel7, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addGap(21, 21, 21))
                    );
                }
                jTabbedPane.addTab("Einstellungen", jPanelDetails); //NON-NLS

                //======== jPanel10 ========
                {

                    //======== jPanel5 ========
                    {
                        jPanel5.setBorder(new EtchedBorder());

                        //---- jButtonGruppeFarbe ----
                        jButtonGruppeFarbe.setText("Farbe"); //NON-NLS
                        jButtonGruppeFarbe.setToolTipText("Farbauswahldialog anzeigen"); //NON-NLS

                        //---- jButtonGruppeStandardfarbe ----
                        jButtonGruppeStandardfarbe.setText("Standardfarbe"); //NON-NLS
                        jButtonGruppeStandardfarbe.setToolTipText("Farbe zur\u00fccksetzen"); //NON-NLS

                        //---- jLabel11 ----
                        jLabel11.setText("Wird das Set als Button verwendet,"); //NON-NLS

                        //---- jLabel13 ----
                        jLabel13.setText("kann damit die Schriftfarbe ver\u00e4ndert werden."); //NON-NLS

                        GroupLayout jPanel5Layout = new GroupLayout(jPanel5);
                        jPanel5.setLayout(jPanel5Layout);
                        jPanel5Layout.setHorizontalGroup(
                            jPanel5Layout.createParallelGroup()
                                .addGroup(jPanel5Layout.createSequentialGroup()
                                    .addContainerGap()
                                    .addGroup(jPanel5Layout.createParallelGroup()
                                        .addGroup(jPanel5Layout.createSequentialGroup()
                                            .addComponent(jButtonGruppeFarbe)
                                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                            .addComponent(jButtonGruppeStandardfarbe))
                                        .addComponent(jLabel11)
                                        .addComponent(jLabel13))
                                    .addContainerGap(317, Short.MAX_VALUE))
                        );
                        jPanel5Layout.linkSize(SwingConstants.HORIZONTAL, new Component[] {jButtonGruppeFarbe, jButtonGruppeStandardfarbe});
                        jPanel5Layout.setVerticalGroup(
                            jPanel5Layout.createParallelGroup()
                                .addGroup(jPanel5Layout.createSequentialGroup()
                                    .addContainerGap()
                                    .addComponent(jLabel11)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(jLabel13)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                    .addGroup(jPanel5Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                        .addComponent(jButtonGruppeFarbe)
                                        .addComponent(jButtonGruppeStandardfarbe))
                                    .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        );
                    }

                    GroupLayout jPanel10Layout = new GroupLayout(jPanel10);
                    jPanel10.setLayout(jPanel10Layout);
                    jPanel10Layout.setHorizontalGroup(
                        jPanel10Layout.createParallelGroup()
                            .addGroup(jPanel10Layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jPanel5, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addContainerGap())
                    );
                    jPanel10Layout.setVerticalGroup(
                        jPanel10Layout.createParallelGroup()
                            .addGroup(jPanel10Layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jPanel5, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                .addContainerGap(402, Short.MAX_VALUE))
                    );
                }
                jTabbedPane.addTab("Aussehen", jPanel10); //NON-NLS

                //======== jPanel9 ========
                {

                    //======== jPanel1 ========
                    {
                        jPanel1.setBorder(new EtchedBorder());

                        //---- jLabel7 ----
                        jLabel7.setText("Zielpfad:"); //NON-NLS

                        //---- jButtonGruppePfad ----
                        jButtonGruppePfad.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png"))); //NON-NLS
                        jButtonGruppePfad.setToolTipText("Pfad ausw\u00e4hlen"); //NON-NLS

                        //---- jCheckBoxThema ----
                        jCheckBoxThema.setText("einen Unterordner mit dem Thema / Abo-Zielpfad anlegen"); //NON-NLS
                        jCheckBoxThema.setToolTipText("im Zielverzeichnis wird ein Unterordner mit dem Namen des Themas zum Speichern der Filme angelegt"); //NON-NLS

                        //---- jLabel8 ----
                        jLabel8.setText("Zieldateiname:"); //NON-NLS

                        //---- jCheckBoxLaenge ----
                        jCheckBoxLaenge.setText("ganzen Dateiname beschr\u00e4nken auf:"); //NON-NLS
                        jCheckBoxLaenge.setToolTipText("die L\u00e4nge des Dateinamens wird auf die Anzahl Zeichen beschr\u00e4nkt"); //NON-NLS

                        //---- jSpinnerLaenge ----
                        jSpinnerLaenge.setModel(new SpinnerNumberModel(25, 10, 200, 1));

                        //---- jLabel12 ----
                        jLabel12.setText("Zeichen"); //NON-NLS

                        //---- jLabel15 ----
                        jLabel15.setText("(ist der Abo-Zielpfad leer, wird das Thema verwendet)"); //NON-NLS

                        //---- jCheckBoxField ----
                        jCheckBoxField.setText("einzelne Felder beschr\u00e4nken auf:"); //NON-NLS

                        //---- jSpinnerField ----
                        jSpinnerField.setModel(new SpinnerNumberModel(10, 3, 100, 1));

                        //---- jLabel16 ----
                        jLabel16.setText("Zeichen"); //NON-NLS

                        GroupLayout jPanel1Layout = new GroupLayout(jPanel1);
                        jPanel1.setLayout(jPanel1Layout);
                        jPanel1Layout.setHorizontalGroup(
                            jPanel1Layout.createParallelGroup()
                                .addGroup(jPanel1Layout.createSequentialGroup()
                                    .addContainerGap()
                                    .addGroup(jPanel1Layout.createParallelGroup()
                                        .addGroup(jPanel1Layout.createSequentialGroup()
                                            .addGap(21, 21, 21)
                                            .addComponent(jLabel15)
                                            .addGap(0, 0, Short.MAX_VALUE))
                                        .addGroup(jPanel1Layout.createSequentialGroup()
                                            .addGroup(jPanel1Layout.createParallelGroup()
                                                .addGroup(jPanel1Layout.createSequentialGroup()
                                                    .addComponent(jLabel7)
                                                    .addGap(56, 56, 56)
                                                    .addComponent(tfGruppeZielPfad)
                                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                                    .addComponent(jButtonGruppePfad))
                                                .addGroup(jPanel1Layout.createSequentialGroup()
                                                    .addComponent(jCheckBoxThema)
                                                    .addGap(0, 205, Short.MAX_VALUE)))
                                            .addGap(16, 16, 16))
                                        .addGroup(jPanel1Layout.createSequentialGroup()
                                            .addComponent(jLabel8)
                                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                            .addGroup(jPanel1Layout.createParallelGroup()
                                                .addComponent(tfGruppeZielName)
                                                .addGroup(jPanel1Layout.createSequentialGroup()
                                                    .addGroup(jPanel1Layout.createParallelGroup(GroupLayout.Alignment.LEADING, false)
                                                        .addGroup(jPanel1Layout.createSequentialGroup()
                                                            .addComponent(jCheckBoxLaenge)
                                                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                                            .addComponent(jSpinnerLaenge, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                                            .addComponent(jLabel12))
                                                        .addGroup(jPanel1Layout.createSequentialGroup()
                                                            .addComponent(jCheckBoxField)
                                                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                            .addComponent(jSpinnerField, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                                            .addComponent(jLabel16)))
                                                    .addGap(0, 0, Short.MAX_VALUE)))
                                            .addContainerGap())))
                        );
                        jPanel1Layout.setVerticalGroup(
                            jPanel1Layout.createParallelGroup()
                                .addGroup(jPanel1Layout.createSequentialGroup()
                                    .addContainerGap()
                                    .addComponent(jCheckBoxThema)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(jLabel15)
                                    .addGap(18, 18, 18)
                                    .addGroup(jPanel1Layout.createParallelGroup(GroupLayout.Alignment.CENTER)
                                        .addComponent(jLabel7)
                                        .addComponent(tfGruppeZielPfad, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                        .addComponent(jButtonGruppePfad))
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                    .addGroup(jPanel1Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                        .addComponent(jLabel8)
                                        .addComponent(tfGruppeZielName, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                    .addGroup(jPanel1Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                        .addComponent(jCheckBoxLaenge)
                                        .addComponent(jSpinnerLaenge, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                        .addComponent(jLabel12))
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addGroup(jPanel1Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                        .addComponent(jCheckBoxField)
                                        .addComponent(jSpinnerField, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                        .addComponent(jLabel16))
                                    .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        );
                        jPanel1Layout.linkSize(SwingConstants.VERTICAL, new Component[] {jButtonGruppePfad, tfGruppeZielName, tfGruppeZielPfad});
                    }

                    GroupLayout jPanel9Layout = new GroupLayout(jPanel9);
                    jPanel9.setLayout(jPanel9Layout);
                    jPanel9Layout.setHorizontalGroup(
                        jPanel9Layout.createParallelGroup()
                            .addGroup(jPanel9Layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jPanel1, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addContainerGap())
                    );
                    jPanel9Layout.setVerticalGroup(
                        jPanel9Layout.createParallelGroup()
                            .addGroup(jPanel9Layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jPanel1, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                .addContainerGap(271, Short.MAX_VALUE))
                    );
                }
                jTabbedPane.addTab("Speicherziel", jPanel9); //NON-NLS

                //======== jPanel11 ========
                {

                    //======== jPanel8 ========
                    {
                        jPanel8.setBorder(new TitledBorder("direkt speichern")); //NON-NLS

                        //---- jLabel10 ----
                        jLabel10.setText("direkter Download, Pr\u00e4fix ( z.B. http ):"); //NON-NLS

                        //---- jLabel5 ----
                        jLabel5.setText("Suffix ( z.B. mp4,mp3):"); //NON-NLS

                        GroupLayout jPanel8Layout = new GroupLayout(jPanel8);
                        jPanel8.setLayout(jPanel8Layout);
                        jPanel8Layout.setHorizontalGroup(
                            jPanel8Layout.createParallelGroup()
                                .addGroup(jPanel8Layout.createSequentialGroup()
                                    .addContainerGap()
                                    .addComponent(jLabel10)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(tfGruppeDirektPraefix)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(jLabel5)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(tfGruppeDirektSuffix)
                                    .addContainerGap())
                        );
                        jPanel8Layout.setVerticalGroup(
                            jPanel8Layout.createParallelGroup()
                                .addGroup(jPanel8Layout.createSequentialGroup()
                                    .addContainerGap()
                                    .addGroup(jPanel8Layout.createParallelGroup(GroupLayout.Alignment.CENTER)
                                        .addComponent(jLabel10)
                                        .addComponent(tfGruppeDirektPraefix, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                        .addComponent(jLabel5)
                                        .addComponent(tfGruppeDirektSuffix, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                                    .addContainerGap(34, Short.MAX_VALUE))
                        );
                    }

                    //---- jTextArea1 ----
                    jTextArea1.setEditable(false);
                    jTextArea1.setBackground(new Color(238, 238, 238));
                    jTextArea1.setColumns(20);
                    jTextArea1.setRows(4);
                    jTextArea1.setText("Filme, deren URL mit \"Pr\u00e4fix\" beginnt und mit \"Suffix\" endet, werden nicht\nmit einem Hilfsprogramm gespeichert, sondern direkt geladen.\n\nEine geringere Aufl\u00f6sung ist nicht bei jedem Sender m\u00f6glich, es wird dann in der gleichen\nAufl\u00f6sung geladen."); //NON-NLS
                    jTextArea1.setBorder(null);

                    //======== jPanel12 ========
                    {
                        jPanel12.setBorder(new TitledBorder("Film downloaden in")); //NON-NLS

                        //---- jRadioButtonAufloesungNormal ----
                        jRadioButtonAufloesungNormal.setSelected(true);
                        jRadioButtonAufloesungNormal.setText("Mittlere Qualit\u00e4t"); //NON-NLS

                        //---- jRadioButtonAufloesungKlein ----
                        jRadioButtonAufloesungKlein.setText("Niedrige Qualit\u00e4t"); //NON-NLS

                        //---- jRadioButtonAufloesungHD ----
                        jRadioButtonAufloesungHD.setText("H\u00f6chste/Hohe Qualit\u00e4t"); //NON-NLS

                        //---- jLabel14 ----
                        jLabel14.setText("Wenn es die Qualit\u00e4tstufe im Angebot nicht gibt, wird die n\u00e4chstkleinere genommen."); //NON-NLS

                        GroupLayout jPanel12Layout = new GroupLayout(jPanel12);
                        jPanel12.setLayout(jPanel12Layout);
                        jPanel12Layout.setHorizontalGroup(
                            jPanel12Layout.createParallelGroup()
                                .addGroup(jPanel12Layout.createSequentialGroup()
                                    .addContainerGap()
                                    .addGroup(jPanel12Layout.createParallelGroup()
                                        .addComponent(jLabel14)
                                        .addComponent(jRadioButtonAufloesungNormal)
                                        .addComponent(jRadioButtonAufloesungKlein)
                                        .addComponent(jRadioButtonAufloesungHD))
                                    .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        );
                        jPanel12Layout.setVerticalGroup(
                            jPanel12Layout.createParallelGroup()
                                .addGroup(jPanel12Layout.createSequentialGroup()
                                    .addContainerGap()
                                    .addComponent(jRadioButtonAufloesungHD)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                    .addComponent(jRadioButtonAufloesungNormal)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                    .addComponent(jRadioButtonAufloesungKlein)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                    .addComponent(jLabel14)
                                    .addContainerGap(30, Short.MAX_VALUE))
                        );
                    }

                    //======== jPanel13 ========
                    {
                        jPanel13.setBorder(new TitledBorder("nach dem Speichern")); //NON-NLS

                        //---- jCheckBoxInfodatei ----
                        jCheckBoxInfodatei.setText("Infodatei anlegen: \"Filmname.txt\""); //NON-NLS

                        //---- jCheckBoxSpotlight ----
                        jCheckBoxSpotlight.setText("Filmbeschreibung als Finder-Kommentar f\u00fcr Spotlight speichern (nur OS X)"); //NON-NLS

                        //---- jCheckBoxSubtitle ----
                        jCheckBoxSubtitle.setText("Untertitel speichern: \"Filmname.xxx\""); //NON-NLS

                        GroupLayout jPanel13Layout = new GroupLayout(jPanel13);
                        jPanel13.setLayout(jPanel13Layout);
                        jPanel13Layout.setHorizontalGroup(
                            jPanel13Layout.createParallelGroup()
                                .addGroup(jPanel13Layout.createSequentialGroup()
                                    .addContainerGap()
                                    .addGroup(jPanel13Layout.createParallelGroup()
                                        .addComponent(jCheckBoxSpotlight)
                                        .addComponent(jCheckBoxInfodatei)
                                        .addComponent(jCheckBoxSubtitle))
                                    .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        );
                        jPanel13Layout.setVerticalGroup(
                            jPanel13Layout.createParallelGroup()
                                .addGroup(jPanel13Layout.createSequentialGroup()
                                    .addContainerGap()
                                    .addComponent(jCheckBoxInfodatei)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                    .addComponent(jCheckBoxSubtitle)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(jCheckBoxSpotlight))
                        );
                    }

                    GroupLayout jPanel11Layout = new GroupLayout(jPanel11);
                    jPanel11.setLayout(jPanel11Layout);
                    jPanel11Layout.setHorizontalGroup(
                        jPanel11Layout.createParallelGroup()
                            .addGroup(jPanel11Layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanel11Layout.createParallelGroup()
                                    .addComponent(jTextArea1)
                                    .addComponent(jPanel8, GroupLayout.Alignment.TRAILING, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(jPanel12, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(jPanel13, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                                .addContainerGap())
                    );
                    jPanel11Layout.setVerticalGroup(
                        jPanel11Layout.createParallelGroup()
                            .addGroup(jPanel11Layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jTextArea1, GroupLayout.PREFERRED_SIZE, 91, GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jPanel8, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jPanel12, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jPanel13, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    );
                }
                jTabbedPane.addTab("Download", jPanel11); //NON-NLS

                //======== jPanelProgramme ========
                {

                    //======== jScrollPane1 ========
                    {
                        jScrollPane1.setBorder(new TitledBorder(null, "Titel", TitledBorder.LEFT, TitledBorder.TOP)); //NON-NLS

                        //---- jTableProgramme ----
                        jTableProgramme.setModel(new TModel());
                        jTableProgramme.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
                        jScrollPane1.setViewportView(jTableProgramme);
                    }

                    //======== jPanel2 ========
                    {
                        jPanel2.setBorder(new EtchedBorder());

                        //---- jButtonProgPlus ----
                        jButtonProgPlus.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-add.png"))); //NON-NLS
                        jButtonProgPlus.setToolTipText("neues Programm anlegen"); //NON-NLS

                        //---- jButtonProgMinus ----
                        jButtonProgMinus.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-remove.png"))); //NON-NLS
                        jButtonProgMinus.setToolTipText("markiertes Programm l\u00f6schen"); //NON-NLS

                        //---- jButtonProgDuplizieren ----
                        jButtonProgDuplizieren.setText("Duplizieren"); //NON-NLS
                        jButtonProgDuplizieren.setToolTipText("markierte Zeile duplizieren"); //NON-NLS

                        //---- jButtonProgAuf ----
                        jButtonProgAuf.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-move-up.png"))); //NON-NLS
                        jButtonProgAuf.setToolTipText("markierte Zeile eins nach oben"); //NON-NLS

                        //---- jButtonProgAb ----
                        jButtonProgAb.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-move-down.png"))); //NON-NLS
                        jButtonProgAb.setToolTipText("markierte Zeile eins nach unten"); //NON-NLS

                        GroupLayout jPanel2Layout = new GroupLayout(jPanel2);
                        jPanel2.setLayout(jPanel2Layout);
                        jPanel2Layout.setHorizontalGroup(
                            jPanel2Layout.createParallelGroup()
                                .addGroup(jPanel2Layout.createSequentialGroup()
                                    .addContainerGap()
                                    .addComponent(jButtonProgPlus)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(jButtonProgMinus)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(jButtonProgAuf)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(jButtonProgAb)
                                    .addGap(18, 18, 18)
                                    .addComponent(jButtonProgDuplizieren)
                                    .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        );
                        jPanel2Layout.setVerticalGroup(
                            jPanel2Layout.createParallelGroup()
                                .addGroup(jPanel2Layout.createSequentialGroup()
                                    .addContainerGap()
                                    .addGroup(jPanel2Layout.createParallelGroup()
                                        .addGroup(jPanel2Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                            .addComponent(jButtonProgPlus)
                                            .addComponent(jButtonProgMinus)
                                            .addComponent(jButtonProgAuf)
                                            .addComponent(jButtonProgAb))
                                        .addComponent(jButtonProgDuplizieren, GroupLayout.Alignment.TRAILING))
                                    .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        );
                    }

                    //======== jPanelProgrammDetails ========
                    {
                        jPanelProgrammDetails.setBorder(new EtchedBorder());

                        //---- jLabel ----
                        jLabel.setText("Programm:"); //NON-NLS

                        //---- jButtonProgPfad ----
                        jButtonProgPfad.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png"))); //NON-NLS
                        jButtonProgPfad.setToolTipText("Programm ausw\u00e4hlen"); //NON-NLS

                        //---- jLabel1 ----
                        jLabel1.setText("Schalter:"); //NON-NLS

                        //---- jLabel2 ----
                        jLabel2.setText("Beschreibung:"); //NON-NLS

                        //---- jLabel3 ----
                        jLabel3.setText("Pr\u00e4fix (z.B. http):"); //NON-NLS

                        //---- jLabel4 ----
                        jLabel4.setText("Suffix ( z.B. mp4,mp3):"); //NON-NLS

                        //---- jCheckBoxRestart ----
                        jCheckBoxRestart.setText("fehlgeschlagene Downloads wieder starten"); //NON-NLS

                        //---- jLabel9 ----
                        jLabel9.setText("Zieldateiname:"); //NON-NLS

                        //---- jCheckBoxRemoteDownload ----
                        jCheckBoxRemoteDownload.setText("externer Downloadmanager"); //NON-NLS

                        GroupLayout jPanelProgrammDetailsLayout = new GroupLayout(jPanelProgrammDetails);
                        jPanelProgrammDetails.setLayout(jPanelProgrammDetailsLayout);
                        jPanelProgrammDetailsLayout.setHorizontalGroup(
                            jPanelProgrammDetailsLayout.createParallelGroup()
                                .addGroup(jPanelProgrammDetailsLayout.createSequentialGroup()
                                    .addContainerGap()
                                    .addGroup(jPanelProgrammDetailsLayout.createParallelGroup()
                                        .addGroup(jPanelProgrammDetailsLayout.createSequentialGroup()
                                            .addGroup(jPanelProgrammDetailsLayout.createParallelGroup()
                                                .addComponent(jLabel2)
                                                .addComponent(jLabel9))
                                            .addGap(24, 24, 24)
                                            .addGroup(jPanelProgrammDetailsLayout.createParallelGroup()
                                                .addComponent(jTextFieldProgZielDateiName)
                                                .addComponent(jTextFieldProgName)))
                                        .addGroup(jPanelProgrammDetailsLayout.createSequentialGroup()
                                            .addGroup(jPanelProgrammDetailsLayout.createParallelGroup()
                                                .addComponent(jLabel3)
                                                .addComponent(jLabel1)
                                                .addComponent(jLabel))
                                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                            .addGroup(jPanelProgrammDetailsLayout.createParallelGroup()
                                                .addComponent(jTextFieldProgSchalter)
                                                .addGroup(GroupLayout.Alignment.TRAILING, jPanelProgrammDetailsLayout.createSequentialGroup()
                                                    .addComponent(jTextFieldProgPfad)
                                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                                    .addComponent(jButtonProgPfad))
                                                .addGroup(jPanelProgrammDetailsLayout.createSequentialGroup()
                                                    .addComponent(jTextFieldProgPraefix, GroupLayout.PREFERRED_SIZE, 146, GroupLayout.PREFERRED_SIZE)
                                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                                    .addComponent(jLabel4)
                                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                                    .addComponent(jTextFieldProgSuffix, GroupLayout.DEFAULT_SIZE, 183, Short.MAX_VALUE))
                                                .addGroup(jPanelProgrammDetailsLayout.createSequentialGroup()
                                                    .addGroup(jPanelProgrammDetailsLayout.createParallelGroup()
                                                        .addComponent(jCheckBoxRemoteDownload)
                                                        .addComponent(jCheckBoxRestart))
                                                    .addGap(0, 194, Short.MAX_VALUE)))))
                                    .addContainerGap())
                        );
                        jPanelProgrammDetailsLayout.setVerticalGroup(
                            jPanelProgrammDetailsLayout.createParallelGroup()
                                .addGroup(jPanelProgrammDetailsLayout.createSequentialGroup()
                                    .addContainerGap()
                                    .addGroup(jPanelProgrammDetailsLayout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                        .addComponent(jLabel2)
                                        .addComponent(jTextFieldProgName, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addGroup(jPanelProgrammDetailsLayout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                        .addComponent(jLabel9)
                                        .addComponent(jTextFieldProgZielDateiName, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addGroup(jPanelProgrammDetailsLayout.createParallelGroup(GroupLayout.Alignment.CENTER)
                                        .addComponent(jLabel)
                                        .addComponent(jTextFieldProgPfad, GroupLayout.PREFERRED_SIZE, 12, GroupLayout.PREFERRED_SIZE)
                                        .addComponent(jButtonProgPfad))
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addGroup(jPanelProgrammDetailsLayout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                        .addComponent(jLabel1)
                                        .addComponent(jTextFieldProgSchalter, GroupLayout.PREFERRED_SIZE, 15, GroupLayout.PREFERRED_SIZE))
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addGroup(jPanelProgrammDetailsLayout.createParallelGroup(GroupLayout.Alignment.BASELINE, false)
                                        .addComponent(jTextFieldProgPraefix, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                        .addComponent(jLabel3)
                                        .addComponent(jTextFieldProgSuffix, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                        .addComponent(jLabel4))
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                    .addComponent(jCheckBoxRestart)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                                    .addComponent(jCheckBoxRemoteDownload)
                                    .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        );
                        jPanelProgrammDetailsLayout.linkSize(SwingConstants.VERTICAL, new Component[] {jButtonProgPfad, jTextFieldProgName, jTextFieldProgPfad, jTextFieldProgPraefix, jTextFieldProgSchalter, jTextFieldProgSuffix, jTextFieldProgZielDateiName});
                    }

                    GroupLayout jPanelProgrammeLayout = new GroupLayout(jPanelProgramme);
                    jPanelProgramme.setLayout(jPanelProgrammeLayout);
                    jPanelProgrammeLayout.setHorizontalGroup(
                        jPanelProgrammeLayout.createParallelGroup()
                            .addGroup(jPanelProgrammeLayout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanelProgrammeLayout.createParallelGroup()
                                    .addComponent(jPanelProgrammDetails, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(jScrollPane1)
                                    .addComponent(jPanel2, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                                .addContainerGap())
                    );
                    jPanelProgrammeLayout.setVerticalGroup(
                        jPanelProgrammeLayout.createParallelGroup()
                            .addGroup(GroupLayout.Alignment.TRAILING, jPanelProgrammeLayout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jScrollPane1, GroupLayout.DEFAULT_SIZE, 184, Short.MAX_VALUE)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jPanel2, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jPanelProgrammDetails, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                    );
                }
                jTabbedPane.addTab("Hilfsprogramme", jPanelProgramme); //NON-NLS
            }
            jSplitPane1.setRightComponent(jTabbedPane);

            //======== jPanel3 ========
            {
                jPanel3.setLayout(new MigLayout(
                    new LC().insets("0").hideMode(3).gridGap("5", "5"), //NON-NLS
                    // columns
                    new AC()
                        .fill().gap()
                        .grow().fill().gap()
                        .fill(),
                    // rows
                    new AC()
                        .grow().fill().gap()
                        .fill().gap()
                        .fill().gap()
                        .fill().gap()
                        .fill()));

                //======== jScrollPane3 ========
                {
                    jScrollPane3.setPreferredSize(new Dimension(150, 150));

                    //---- jTablePset ----
                    jTablePset.setModel(new DefaultTableModel(
                        new Object[][] {
                            {null, null, null},
                        },
                        new String[] {
                            "Title 1", "Title 2", "Title 3" //NON-NLS
                        }
                    ));
                    jTablePset.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
                    jTablePset.setMinimumSize(new Dimension(50, 16));
                    jTablePset.setPreferredSize(new Dimension(150, 50));
                    jScrollPane3.setViewportView(jTablePset);
                }
                jPanel3.add(jScrollPane3, new CC().cell(0, 0, 3, 1));

                //---- jButtonGruppeDuplizieren ----
                jButtonGruppeDuplizieren.setText("Duplizieren"); //NON-NLS
                jButtonGruppeDuplizieren.setToolTipText("Programmgruppe kopieren"); //NON-NLS
                jPanel3.add(jButtonGruppeDuplizieren, new CC().cell(0, 3, 3, 1));

                //---- jButtonExport ----
                jButtonExport.setText("Export"); //NON-NLS
                jButtonExport.setToolTipText("Programmgruppe in Datei exportieren"); //NON-NLS
                jPanel3.add(jButtonExport, new CC().cell(0, 4, 3, 1));

                //---- jButtonGruppeNeu ----
                jButtonGruppeNeu.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-add.png"))); //NON-NLS
                jButtonGruppeNeu.setToolTipText("neue Programmgruppe anlegen"); //NON-NLS
                jPanel3.add(jButtonGruppeNeu, new CC().cell(0, 2).alignX("center").growX(0)); //NON-NLS

                //---- jButtonGruppeLoeschen ----
                jButtonGruppeLoeschen.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-remove.png"))); //NON-NLS
                jButtonGruppeLoeschen.setToolTipText("Programmgruppe l\u00f6schen"); //NON-NLS
                jPanel3.add(jButtonGruppeLoeschen, new CC().cell(2, 2).alignX("center").growX(0)); //NON-NLS

                //---- jButtonGruppeAuf ----
                jButtonGruppeAuf.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-move-up.png"))); //NON-NLS
                jButtonGruppeAuf.setToolTipText("Programmgruppe nach oben schieben"); //NON-NLS
                jPanel3.add(jButtonGruppeAuf, new CC().cell(0, 1).alignX("center").growX(0)); //NON-NLS
                jPanel3.add(hSpacer1, new CC().cell(1, 1));

                //---- jButtonGruppeAb ----
                jButtonGruppeAb.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-move-down.png"))); //NON-NLS
                jButtonGruppeAb.setToolTipText("Programmgruppe nach unten schieben"); //NON-NLS
                jPanel3.add(jButtonGruppeAb, new CC().cell(2, 1).alignX("center").growX(0)); //NON-NLS
            }
            jSplitPane1.setLeftComponent(jPanel3);
        }

        GroupLayout layout = new GroupLayout(this);
        setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addGroup(layout.createParallelGroup()
                        .addComponent(jSplitPane1, GroupLayout.DEFAULT_SIZE, 926, Short.MAX_VALUE)
                        .addGroup(GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                            .addContainerGap(742, Short.MAX_VALUE)
                            .addComponent(jButtonPruefen, GroupLayout.PREFERRED_SIZE, 100, GroupLayout.PREFERRED_SIZE)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jButtonHilfe)))
                    .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jSplitPane1)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                    .addGroup(layout.createParallelGroup(GroupLayout.Alignment.CENTER)
                        .addComponent(jButtonHilfe)
                        .addComponent(jButtonPruefen))
                    .addContainerGap())
        );

        //---- buttonGroup1 ----
        var buttonGroup1 = new ButtonGroup();
        buttonGroup1.add(jRadioButtonAufloesungNormal);
        buttonGroup1.add(jRadioButtonAufloesungKlein);
        buttonGroup1.add(jRadioButtonAufloesungHD);
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JButton jButtonHilfe;
    private JButton jButtonPruefen;
    private JTabbedPane jTabbedPane;
    private JTextField jTextFieldSetName;
    private JTextArea jTextAreaSetBeschreibung;
    private JCheckBox jCheckBoxSpeichern;
    private JCheckBox jCheckBoxButton;
    private JCheckBox jCheckBoxAbo;
    private JButton jButtonAbspielen;
    private JLabel jLabelMeldungAbspielen;
    private JLabel jLabelMeldungSeichern;
    private JButton jButtonGruppeFarbe;
    private JButton jButtonGruppeStandardfarbe;
    private JTextField tfGruppeZielPfad;
    private JButton jButtonGruppePfad;
    private JCheckBox jCheckBoxThema;
    private JTextField tfGruppeZielName;
    private JCheckBox jCheckBoxLaenge;
    private JSpinner jSpinnerLaenge;
    private JCheckBox jCheckBoxField;
    private JSpinner jSpinnerField;
    private JTextField tfGruppeDirektPraefix;
    private JTextField tfGruppeDirektSuffix;
    private JRadioButton jRadioButtonAufloesungNormal;
    private JRadioButton jRadioButtonAufloesungKlein;
    private JRadioButton jRadioButtonAufloesungHD;
    private JCheckBox jCheckBoxInfodatei;
    private JCheckBox jCheckBoxSpotlight;
    private JCheckBox jCheckBoxSubtitle;
    private JScrollPane jScrollPane1;
    private JButton jButtonProgPlus;
    private JButton jButtonProgMinus;
    private JButton jButtonProgDuplizieren;
    private JButton jButtonProgAuf;
    private JButton jButtonProgAb;
    private JTextField jTextFieldProgPfad;
    private JButton jButtonProgPfad;
    private JTextField jTextFieldProgSchalter;
    private JTextField jTextFieldProgName;
    private JTextField jTextFieldProgPraefix;
    private JTextField jTextFieldProgSuffix;
    private JCheckBox jCheckBoxRestart;
    private JTextField jTextFieldProgZielDateiName;
    private JCheckBox jCheckBoxRemoteDownload;
    private JScrollPane jScrollPane3;
    private JButton jButtonGruppeDuplizieren;
    private JButton jButtonExport;
    private JButton jButtonGruppeNeu;
    private JButton jButtonGruppeLoeschen;
    private JButton jButtonGruppeAuf;
    private JButton jButtonGruppeAb;
    // End of variables declaration//GEN-END:variables
}
