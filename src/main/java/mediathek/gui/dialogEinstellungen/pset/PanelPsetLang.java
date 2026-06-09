/*
 * Copyright (c) 2025-2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.dialogEinstellungen.pset;

import ca.odell.glazedlists.swing.AdvancedTableModel;
import ca.odell.glazedlists.swing.GlazedListsSwing;
import mediathek.audiothek.ui.table.TriStateTableRowSorter;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.application.ApplicationConfiguration;
import mediathek.controller.IoXmlSchreiben;
import mediathek.controller.starter.RuntimeExec;
import mediathek.daten.*;
import mediathek.gui.messages.ProgramSetChangedEvent;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.*;
import mediathek.tool.cellrenderer.PsetNameCellRenderer;
import mediathek.tool.models.NonEditableTableModel;
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
import javax.swing.table.TableModel;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;

public class PanelPsetLang extends JPanel {
    private static final ProgramTableFormat PROGRAM_TABLE_FORMAT = new ProgramTableFormat();

    private int neuZaehler;
    private final ListePset listePset;
    private final MVTable tabellePset;
    private final JTable tabelleProgramme;
    private final PsetNameCellRenderer psetNameRenderer = new PsetNameCellRenderer();
    private final JFrame parentComponent;
    private final ListeProg emptyProgramList = new ListeProg();
    private TriStateTableRowSorter<TableModel> programTableSorter;
    private ListeProg currentProgramList;
    private boolean stopBeob;

    public PanelPsetLang(JFrame parentComponent, ListePset llistePset) {
        this.parentComponent = parentComponent;
        initComponents();
        tabellePset = new MVPsetTable();
        jScrollPane3.setViewportView(tabellePset);
        tabelleProgramme = new JTable();
        tabelleProgramme.setAutoCreateRowSorter(false);
        tabelleProgramme.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        jScrollPane1.setViewportView(tabelleProgramme);
        listePset = llistePset;
        init();
    }

    @Handler
    private void handleProgramSetChanged(ProgramSetChangedEvent e) {
        if (!stopBeob) {
            tabellePset();
        }
    }

    private void init() {
        configureIcons();
        MessageBus.getMessageBus().subscribe(this);

        configureProgramTables();
        installProgramFieldListeners();
        installProgramFieldPopupMenus();
        disableProgramFields();
        installProgramActions();

        installProgramSetActions();
        installProgramSetDocumentListeners();
        installHelpAndCheckActions();
        installTableSelectionListeners();

        tabellePset();
        selectFirstProgramSet();
    }

    private void configureIcons() {
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
    }

    private void configureProgramTables() {
        tabellePset.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
    }

    private void installProgramFieldListeners() {
        BeobProgDoc beobDoc = new BeobProgDoc();
        installDocumentListener(beobDoc,
                jTextFieldProgPfad,
                jTextFieldProgSchalter,
                jTextFieldProgName,
                jTextFieldProgZielDateiName,
                jTextFieldProgPraefix,
                jTextFieldProgSuffix);
    }

    private void installProgramFieldPopupMenus() {
        installTextPopupMenus(
                jTextFieldProgPfad,
                jTextFieldProgSchalter,
                jTextFieldProgName,
                jTextFieldProgZielDateiName,
                jTextFieldProgPraefix,
                jTextFieldProgSuffix);
    }

    private void disableProgramFields() {
        setProgramFieldsEnabled(false);
    }

    private void installProgramActions() {
        jButtonProgPfad.addActionListener(_ -> chooseProgramPath());
        jButtonProgPlus.addActionListener(_ -> progNeueZeile(new DatenProg()));
        jButtonProgMinus.addActionListener(_ -> deleteSelectedProgramEntries());
        jButtonProgDuplizieren.addActionListener(_ -> duplicateSelectedProgramEntry());
        jButtonProgAuf.addActionListener(_ -> progAufAb(true));
        jButtonProgAb.addActionListener(_ -> progAufAb(false));

        jButtonProgPfad.setEnabled(false);
        updateProgramMoveButtons(null);
        jCheckBoxRestart.addActionListener(_ -> updateSelectedProgramFlag(
                DatenProg.PROGRAMM_RESTART,
                jCheckBoxRestart.isSelected()));
        jCheckBoxRemoteDownload.addActionListener(_ -> updateSelectedProgramFlag(
                DatenProg.PROGRAMM_DOWNLOADMANAGER,
                jCheckBoxRemoteDownload.isSelected()));
    }

    private void installProgramSetActions() {
        jButtonAbspielen.addActionListener(_ -> {
            if (getPset() instanceof DatenPset pset) {
                Daten.getInstance().getListePset().activateAsPlayer(pset);
                nurtabellePset();
                notifyProgramSetChanged();
            }
        });
        jCheckBoxSpeichern.addActionListener(_ -> updateSelectedProgramSet(pset -> pset.setSpeichern(jCheckBoxSpeichern.isSelected()), true));
        jCheckBoxButton.addActionListener(_ -> updateSelectedProgramSet(pset -> pset.setButton(jCheckBoxButton.isSelected()), true));
        jCheckBoxAbo.addActionListener(_ -> updateSelectedProgramSet(pset -> pset.setAbo(jCheckBoxAbo.isSelected()), true));
        jCheckBoxLaenge.addActionListener(_ -> updateSelectedProgramSet(pset -> pset.setLaengeBeschraenken(jCheckBoxLaenge.isSelected()), false));
        jCheckBoxField.addActionListener(_ -> updateSelectedProgramSet(pset -> pset.setLaengeFieldBeschraenken(jCheckBoxField.isSelected()), false));
        jCheckBoxThema.addActionListener(_ -> updateSelectedProgramSet(pset -> pset.setThemaAnlegen(jCheckBoxThema.isSelected()), false));
        jSpinnerLaenge.addChangeListener(_ -> {
            if (getPset() instanceof DatenPset pset && jSpinnerLaenge.getModel().getValue() instanceof Number value) {
                pset.setMaxLaenge(value.intValue());
            }
        });
        jSpinnerField.addChangeListener(_ -> {
            if (getPset() instanceof DatenPset pset && jSpinnerField.getModel().getValue() instanceof Number value) {
                pset.setMaxLaengeField(value.intValue());
            }
        });
        jCheckBoxInfodatei.addActionListener(_ -> updateSelectedProgramSet(pset -> pset.setInfodatei(jCheckBoxInfodatei.isSelected()), false));
        jCheckBoxSubtitle.addActionListener(_ -> updateSelectedProgramSet(pset -> pset.setSubtitle(jCheckBoxSubtitle.isSelected()), false));

        jCheckBoxSpotlight.setEnabled(SystemUtils.IS_OS_MAC_OSX);
        jCheckBoxSpotlight.addActionListener(_ -> updateSelectedProgramSet(pset -> pset.setSpotlight(jCheckBoxSpotlight.isSelected()), false));

        jButtonGruppeNeu.addActionListener(_ -> setNeu());
        jButtonGruppeLoeschen.addActionListener(_ -> setLoeschen());
        jButtonGruppeFarbe.addActionListener(_ -> chooseProgramSetColor());
        jButtonGruppeStandardfarbe.addActionListener(_ -> clearProgramSetColor());
        jButtonGruppeAuf.addActionListener(_ -> setAufAb(true));
        jButtonGruppeAb.addActionListener(_ -> setAufAb(false));
        jButtonGruppeDuplizieren.addActionListener(_ -> duplicateSelectedProgramSet());
        jButtonExport.addActionListener(_ -> setExport());
        jButtonGruppePfad.addActionListener(_ -> chooseProgramSetTargetPath());
    }

    private void installProgramSetDocumentListeners() {
        jTextAreaSetBeschreibung.getDocument().addDocumentListener(new BeobDoc(jTextAreaSetBeschreibung, DatenPset.PROGRAMMSET_BESCHREIBUNG));
        installTextPopupMenus(jTextAreaSetBeschreibung);

        tfGruppeDirektSuffix.getDocument().addDocumentListener(
                new BeobDoc(tfGruppeDirektSuffix, DatenPset.PROGRAMMSET_SUFFIX_DIREKT, false));
        tfGruppeDirektPraefix.getDocument().addDocumentListener(
                new BeobDoc(tfGruppeDirektPraefix, DatenPset.PROGRAMMSET_PRAEFIX_DIREKT, false));
        tfGruppeZielName.getDocument().addDocumentListener(new BeobDoc(tfGruppeZielName,
                DatenPset.PROGRAMMSET_ZIEL_DATEINAME, false));
        tfGruppeZielPfad.getDocument().addDocumentListener(
                new BeobDoc(tfGruppeZielPfad, DatenPset.PROGRAMMSET_ZIEL_PFAD, false));

        jTextFieldSetName.getDocument().addDocumentListener(new DuplicatePsetNameCheckListener(jTextFieldSetName));
        jTextFieldSetName.getDocument().addDocumentListener(new BeobDoc(jTextFieldSetName, DatenPset.PROGRAMMSET_NAME));

        installTextPopupMenus(
                jTextFieldSetName,
                tfGruppeDirektSuffix,
                tfGruppeDirektPraefix,
                tfGruppeZielName,
                tfGruppeZielPfad);
    }

    private void installDocumentListener(DocumentListener documentListener, JTextComponent... textComponents) {
        for (var textComponent : textComponents) {
            textComponent.getDocument().addDocumentListener(documentListener);
        }
    }

    private void installTextPopupMenus(JTextComponent... textComponents) {
        for (var textComponent : textComponents) {
            var handler = new TextCopyPasteHandler<>(textComponent);
            textComponent.setComponentPopupMenu(handler.getPopupMenu());
        }
    }

    private void installHelpAndCheckActions() {
        jButtonHilfe.addActionListener(_ -> {
            var str = GetFile.getHilfeSuchen(Konstanten.PFAD_HILFETEXT_PRGRAMME).trim();
            JTextArea area = new JTextArea(str);
            area.setRows(20);
            area.setColumns(60);
            area.setLineWrap(true);
            area.setEditable(false);
            JScrollPane pane = new JScrollPane(area);
            JOptionPane.showMessageDialog(this,pane,"Hilfe", JOptionPane.INFORMATION_MESSAGE);
        });
        jRadioButtonAufloesungKlein.addActionListener(_ -> setAufloesung());
        jRadioButtonAufloesungNormal.addActionListener(_ -> setAufloesung());
        jRadioButtonAufloesungHD.addActionListener(_ -> setAufloesung());
        jButtonPruefen.addActionListener(_ -> programmePruefen());
    }

    private void installTableSelectionListeners() {
        tabelleProgramme.getSelectionModel().addListSelectionListener(e -> {
            if (!e.getValueIsAdjusting()) {
                if (!stopBeob) {
                    fillTextProgramme();
                }
            }
        });
        tabellePset.getSelectionModel().addListSelectionListener(event -> {
            if (!stopBeob) {
                if (!event.getValueIsAdjusting()) {
                    tabelleProgramme();
                    var row = tabellePset.getSelectedRow();
                    if (row != -1) {
                        var modelRow = tabellePset.convertRowIndexToModel(row);
                        var datenPset = listePset.get(modelRow);
                        tabellePset.getModel().setValueAt(jTextFieldSetName.getText(), modelRow, DatenPset.PROGRAMMSET_NAME);
                        jTabbedPane.setTitleAt(0, "Set Name: " + datenPset.getName());
                    }
                }
            }
        });
    }

    private void selectFirstProgramSet() {
        if (tabellePset.getRowCount() > 0) {
            tabellePset.setRowSelectionInterval(0, 0);
            tabellePset.scrollRectToVisible(tabellePset.getCellRect(0, 0, false));
        }
    }

    private void chooseProgramPath() {
        var initialFile = "";
        if (!jTextFieldProgPfad.getText().isEmpty()) {
            initialFile = jTextFieldProgPfad.getText();
        }
        var destFile = FileDialogs.chooseLoadFileLocation(MediathekGui.ui(),"Programm auswählen", initialFile);
        if (destFile != null) {
            jTextFieldProgPfad.setText(destFile.getAbsolutePath());
        }
    }

    private void deleteSelectedProgramEntries() {
        var modelRows = getSelectedProgramModelRows();
        if (modelRows.length == 0) {
            NoSelectionErrorDialog.show(this);
            return;
        }

        var programList = getCurrentProgramList();
        String text;
        if (modelRows.length == 1) {
            var modelRow = modelRows[0];
            var prog = programList.get(modelRow);
            if (isEmptyProgramEntry(prog)) {
                programList.removeEntryAtIndex(modelRow);
                tabelleProgramme();
                return;
            }
            text = prog.getName();
        } else {
            text = modelRows.length + " Programme löschen?";
        }

        var ret = JOptionPane.showConfirmDialog(parentComponent, text, "Löschen?", JOptionPane.YES_NO_OPTION);
        if (ret == JOptionPane.OK_OPTION) {
            var progsToDelete = Arrays.stream(modelRows)
                    .mapToObj(programList::get)
                    .toList();
            programList.removeAllEntries(progsToDelete);
            tabelleProgramme();
        }
    }

    private int[] getSelectedProgramModelRows() {
        return Arrays.stream(tabelleProgramme.getSelectedRows())
                .map(tabelleProgramme::convertRowIndexToModel)
                .toArray();
    }

    private void duplicateSelectedProgramEntry() {
        var rows = tabelleProgramme.getSelectedRow();
        if (rows != -1) {
            var row = tabelleProgramme.convertRowIndexToModel(rows);
            var prog = getCurrentProgramList().get(row);
            progNeueZeile(prog.copy());
        } else {
            NoSelectionErrorDialog.show(this);
        }
    }

    private void updateSelectedProgramFlag(int dataIndex, boolean selected) {
        if (stopBeob) {
            return;
        }

        var rows = tabelleProgramme.getSelectedRow();
        if (rows != -1) {
            var modelIndex = tabelleProgramme.convertRowIndexToModel(rows);
            var listeProg = getCurrentProgramList();
            var prog = listeProg.get(modelIndex);
            if (dataIndex == DatenProg.PROGRAMM_RESTART) {
                prog.setRestart(selected);
            } else if (dataIndex == DatenProg.PROGRAMM_DOWNLOADMANAGER) {
                prog.setDownloadManager(selected);
            }
            listeProg.fireEntryChanged(modelIndex);
            updateProgramMoveButtons(prog);
        }
    }

    private void chooseProgramSetColor() {
        if (getPset() instanceof DatenPset pSet) {
            var selectedColor = JColorChooser.showDialog(PanelPsetLang.this, "Farbe auswählen", pSet.getFarbe());
            if (selectedColor != null) {
                pSet.setFarbe(selectedColor);
                tabellePset();
                notifyProgramSetChanged();
            }
        }
    }

    private void clearProgramSetColor() {
        if (getPset() instanceof DatenPset pSet) {
            pSet.clearFarbe();
            tabellePset();
            notifyProgramSetChanged();
        }
    }

    private void duplicateSelectedProgramSet() {
        final int row = tabellePset.getSelectedRow();
        if (row != -1) {
            var gruppe = listePset.get(tabellePset.convertRowIndexToModel(row));
            listePset.addPset(gruppe.copy());
            tabellePset();
            notifyProgramSetChanged();
        } else {
            NoSelectionErrorDialog.show(this);
        }
    }

    private void chooseProgramSetTargetPath() {
        var initialFile = "";
        if (!tfGruppeZielPfad.getText().isEmpty()) {
            initialFile = tfGruppeZielPfad.getText();
        }
        var destDirectory = FileDialogs.chooseDirectoryLocation(MediathekGui.ui(), "Filme speichern unter", initialFile);
        if (destDirectory != null) {
            tfGruppeZielPfad.setText(destDirectory.getAbsolutePath());
        }
    }

    private void updateSelectedProgramSet(Consumer<DatenPset> update, boolean notifyChange) {
        if (getPset() instanceof DatenPset pset) {
            update.accept(pset);
            nurtabellePset();
            if (notifyChange) {
                notifyProgramSetChanged();
            }
        }
    }

    /**
     * Prüfen ob die eingestellten Programmsets passen
     */
    public void programmePruefen() {
        final var PIPE = "| ";
        final var LEER = "      ";
        final var PFEIL = " -> ";
        var checkResultList = new ArrayList<Boolean>();
        var text = new StringBuilder();

        //check only pset which are not label or free line
        Daten.getInstance().getListePset().stream()
                .filter(pset -> !pset.isFreeLine())
                .filter(pset -> !pset.isLabel())
                .forEach(datenPset -> {
                    var ret = true;
                    text.append("++++++++++++++++++++++++++++++++++++++++++++" + '\n');
                    text.append(PIPE + "Programmgruppe: ").append(datenPset.getName()).append('\n');
                    var zielPfad = datenPset.getZielPfad();
                    if (datenPset.progsContainPath()) {
                        // beim nur Abspielen wird er nicht gebraucht
                        if (zielPfad.isEmpty()) {
                            ret = false;
                            text.append(PIPE + LEER + "Zielpfad fehlt!\n");
                        }
                        else // Pfad beschreibbar?
                            if (!GuiFunktionenProgramme.checkPathWriteable(zielPfad)) {
                                //da Pfad-leer und "kein" Pfad schon abgeprüft
                                ret = false;
                                text.append(PIPE + LEER + "Falscher Zielpfad!\n");
                                text.append(PIPE + LEER + PFEIL + "Zielpfad \"").append(zielPfad).append("\" nicht beschreibbar!").append('\n');
                            }
                    }

                    for (var datenProg : datenPset.getListeProg()) {
                        // Programmpfad prüfen
                        final var progPfad = datenProg.getProgramPath();
                        final var progName = datenProg.getName();
                        if (progPfad.isEmpty()) {
                            ret = false;
                            text.append(PIPE + LEER + "Kein Programm angegeben!\n");
                            text.append(PIPE + LEER + PFEIL + "Programmname: ").append(progName).append('\n');
                            text.append(PIPE + LEER + LEER + "Pfad: ").append(progPfad).append('\n');
                        }
                        else if (!Files.isExecutable(Paths.get(progPfad))) {
                            // dann noch mit RuntimeExec versuchen
                            var r = new RuntimeExec(progPfad);
                            var pr = r.exec(false);
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

                    //store the result of each check
                    checkResultList.add(ret);

                    if (ret) {
                        //sollte alles passen
                        text.append(PIPE + PFEIL + "Ok!" + '\n');
                    }
                    text.append("""
                            ++++++++++++++++++++++++++++++++++++++++++++
                            
                            
                            """);
                });

        var allTrue = checkResultList.stream().allMatch(Boolean::booleanValue);
        if (allTrue) {
            JOptionPane.showMessageDialog(this, "Alle Programm-Sets sind in Ordnung.", Konstanten.PROGRAMMNAME, JOptionPane.INFORMATION_MESSAGE);
        }
        else
            JOptionPane.showMessageDialog(this, text.toString(), Konstanten.PROGRAMMNAME, JOptionPane.WARNING_MESSAGE);
    }

    private void setAufloesung() {
        if (getPset() instanceof DatenPset pset) {
            if (jRadioButtonAufloesungNormal.isSelected()) {
                pset.setAufloesung(FilmResolution.Enum.NORMAL);
            } else if (jRadioButtonAufloesungHD.isSelected()) {
                pset.setAufloesung(FilmResolution.Enum.HIGH_QUALITY);
            } else if (jRadioButtonAufloesungKlein.isSelected()) {
                pset.setAufloesung(FilmResolution.Enum.LOW);
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
        tabellePset.setModel(listePset.createModel());
        tabellePset.setSpalten();
        spaltenSetzen();
        jLabelMeldungAbspielen.setVisible(listePset.getPsetAbspielen() == null);
        jLabelMeldungSeichern.setVisible(listePset.getListeSpeichern().isEmpty());
        stopBeob = false;
    }

    private void spaltenSetzen() {
        final var columnModel = tabellePset.getColumnModel();
        for (int i = 0; i < tabellePset.getColumnCount(); ++i) {
            var column = columnModel.getColumn(tabellePset.convertColumnIndexToView(i));
            if (i == DatenPset.PROGRAMMSET_NAME) {
                column.setCellRenderer(psetNameRenderer);
                column.setMinWidth(10);
                column.setPreferredWidth(120);
                column.setMaxWidth(1000);
            } else if (i == DatenPset.PROGRAMMSET_IST_ABSPIELEN
                    || i == DatenPset.PROGRAMMSET_IST_SPEICHERN) {
                column.setMinWidth(10);
                column.setPreferredWidth(80);
                column.setMaxWidth(1000);
            } else {
                column.setMinWidth(0);
                column.setPreferredWidth(0);
                column.setMaxWidth(0);
            }
        }
    }

    private void tabelleProgramme() {
        //Tabelle mit den Programmen füllen
        var pSet = getPset();
        stopBeob = true;

        enableComponents(jTabbedPane, pSet != null);
        jButtonAbspielen.setBackground(null);
        if (pSet != null) {
            jTabbedPane.setTitleAt(0, "Set Name: " + pSet.getName());
            if (pSet.getMaxLaenge() == null) {
                jSpinnerLaenge.setValue(Konstanten.LAENGE_DATEINAME);
                pSet.setMaxLaenge(Konstanten.LAENGE_DATEINAME);
            } else {
                jSpinnerLaenge.setValue(pSet.getMaxLaenge());
            }
            if (pSet.getMaxLaengeField() == null) {
                jSpinnerField.setValue(Konstanten.LAENGE_FELD);
                pSet.setMaxLaengeField(Konstanten.LAENGE_FELD);
            } else {
                jSpinnerField.setValue(pSet.getMaxLaengeField());
            }
            jCheckBoxLaenge.setSelected(pSet.isLaengeBeschraenken());
            jCheckBoxField.setSelected(pSet.isLaengeFieldBeschraenken());
            jCheckBoxThema.setSelected(pSet.isThemaAnlegen());
            jCheckBoxInfodatei.setSelected(pSet.shouldCreateInfofile());
            jCheckBoxSubtitle.setSelected(pSet.shouldDownloadSubtitle());
            jCheckBoxSpotlight.setEnabled(SystemUtils.IS_OS_MAC_OSX);
            jCheckBoxSpotlight.setSelected(pSet.isSpotlight());
            jScrollPane1.setBorder(BorderFactory.createTitledBorder(null, "Set Name: " + pSet.getName(), TitledBorder.LEFT, TitledBorder.TOP));
            jTextFieldSetName.setText(pSet.getName());
            tfGruppeDirektSuffix.setText(pSet.getSuffixDirekt());
            tfGruppeDirektPraefix.setText(pSet.getPraefixDirekt());
            tfGruppeZielName.setText(pSet.getZielDateiname());
            tfGruppeZielPfad.setText(pSet.getZielPfad());
            jTextAreaSetBeschreibung.setText(pSet.getBeschreibung());

            jCheckBoxSpeichern.setSelected(pSet.istSpeichern());
            jCheckBoxButton.setSelected(pSet.istButton());
            jCheckBoxAbo.setSelected(pSet.istAbo());
            switch (pSet.getAufloesung()) {
                case HIGH_QUALITY -> jRadioButtonAufloesungHD.setSelected(true);
                case LOW -> jRadioButtonAufloesungKlein.setSelected(true);
                default -> jRadioButtonAufloesungNormal.setSelected(true);
            }
            bindProgramTableModel(pSet.getListeProg());
            if (tabelleProgramme.getRowCount() > 0) {
                spaltenSetzenProgramme();
                tabelleProgramme.setRowSelectionInterval(0, 0);
                tabelleProgramme.scrollRectToVisible(tabelleProgramme.getCellRect(0, 0, true));
            }
        } else {
            jScrollPane1.setBorder(BorderFactory.createTitledBorder(null, "", javax.swing.border.TitledBorder.LEFT, javax.swing.border.TitledBorder.TOP));
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
            bindProgramTableModel(emptyProgramList);
        }
        stopBeob = false;
        fillTextProgramme();
    }

    private void enableComponents(Container container, boolean enable) {
        Component[] components = container.getComponents();
        for (Component component : components) {
            component.setEnabled(enable);
            if (component instanceof Container childContainer) {
                enableComponents(childContainer, enable);
            }
        }
    }

    private void bindProgramTableModel(ListeProg listeProg) {
        if (currentProgramList == listeProg) {
            return;
        }
        AdvancedTableModel<?> oldModel = null;
        if (tabelleProgramme.getModel() instanceof AdvancedTableModel<?> model) {
            oldModel = model;
        }
        var newModel = GlazedListsSwing.eventTableModelWithThreadProxyList(listeProg, PROGRAM_TABLE_FORMAT);
        tabelleProgramme.setRowSorter(null);
        tabelleProgramme.setModel(newModel);
        currentProgramList = listeProg;
        if (programTableSorter == null) {
            programTableSorter = new TriStateTableRowSorter<>(newModel);
            programTableSorter.addRowSorterListener(_ -> updateProgramMoveButtonsForSelection());
        } else {
            programTableSorter.setModel(newModel);
        }
        programTableSorter.setSortKeys(List.of());
        tabelleProgramme.setRowSorter(programTableSorter);
        updateProgramMoveButtonsForSelection();
        if (oldModel != null) {
            oldModel.dispose();
        }
    }

    public void spaltenSetzenProgramme() {
        for (int i = 0; i < tabelleProgramme.getColumnCount(); ++i) {
            var column = tabelleProgramme.getColumnModel().getColumn(tabelleProgramme.convertColumnIndexToView(i));
            if (i == DatenProg.PROGRAMM_PRAEFIX
                    || i == DatenProg.PROGRAMM_RESTART
                    || i == DatenProg.PROGRAMM_DOWNLOADMANAGER
                    || i == DatenProg.PROGRAMM_SUFFIX) {
                column.setMinWidth(10);
                column.setMaxWidth(3000);
                column.setPreferredWidth(75);
            } else {
                column.setMinWidth(10);
                column.setMaxWidth(3000);
                column.setPreferredWidth(150);
            }
        }
    }

    /**
     * Send message that changes to the Pset were performed.
     */
    private void notifyProgramSetChanged() {
        MessageBus.getMessageBus().publish(new ProgramSetChangedEvent());
    }

    private void fillTextProgramme() {
        //Textfelder mit Programmdaten füllen
        stopBeob = true;
        var row = tabelleProgramme.getSelectedRow();
        var validRowSelected = row != -1;
        var modelRow = validRowSelected ? tabelleProgramme.convertRowIndexToModel(row) : -1;
        var modelRowCount = tabelleProgramme.getModel().getRowCount();
        var letzteZeile = modelRowCount <= 1 || modelRow == modelRowCount - 1;

        setProgramFieldsEnabled(validRowSelected);
        jButtonProgPfad.setEnabled(validRowSelected);
        jCheckBoxRestart.setEnabled(validRowSelected);
        jCheckBoxRemoteDownload.setEnabled(validRowSelected);
        if (validRowSelected) {
            var prog = getCurrentProgramList().get(modelRow);
            updateProgramMoveButtons(prog);
            fillProgramFields(prog);
            jCheckBoxRestart.setSelected(prog.isRestart());
            jCheckBoxRemoteDownload.setSelected(prog.isDownloadManager());
        } else {
            clearProgramFields();
            updateProgramMoveButtons(null);
        }
        if (letzteZeile) {
            jTextFieldProgPraefix.setEnabled(false);
            jTextFieldProgSuffix.setEnabled(false);
        }
        stopBeob = false;
    }

    private void setProgramFieldsEnabled(boolean enabled) {
        jTextFieldProgPfad.setEnabled(enabled);
        jTextFieldProgSchalter.setEnabled(enabled);
        jTextFieldProgZielDateiName.setEnabled(enabled);
        jTextFieldProgName.setEnabled(enabled);
        jTextFieldProgPraefix.setEnabled(enabled);
        jTextFieldProgSuffix.setEnabled(enabled);
    }

    private void fillProgramFields(DatenProg prog) {
        jTextFieldProgPfad.setText(prog.getProgramPath());
        jTextFieldProgSchalter.setText(prog.getSwitches());
        jTextFieldProgZielDateiName.setText(prog.getTargetFileName());
        jTextFieldProgName.setText(prog.getName());
        jTextFieldProgPraefix.setText(prog.getPrefix());
        jTextFieldProgSuffix.setText(prog.getSuffix());
    }

    private void clearProgramFields() {
        jTextFieldProgPfad.setText("");
        jTextFieldProgSchalter.setText("");
        jTextFieldProgZielDateiName.setText("");
        jTextFieldProgName.setText("");
        jTextFieldProgPraefix.setText("");
        jTextFieldProgSuffix.setText("");
    }

    //Pset
    private DatenPset getPset() {
        var row = tabellePset.getSelectedRow();
        return row == -1 ? null : listePset.get(tabellePset.convertRowIndexToModel(row));
    }

    private ListeProg getCurrentProgramList() {
        return Objects.requireNonNull(currentProgramList, "program table model");
    }

    private void setAufAb(boolean auf) {
        var row = tabellePset.getSelectedRow();
        if (row != -1) {
            var neu = listePset.auf(tabellePset.convertRowIndexToModel(row), auf);
            neu = tabellePset.convertRowIndexToView(neu);
            tabellePset.setRowSelectionInterval(neu, neu);
            tabellePset.scrollRectToVisible(tabellePset.getCellRect(neu, 0, false));
            notifyProgramSetChanged();
        } else {
            NoSelectionErrorDialog.show(this);
        }
    }

    private void setNeu() {
        listePset.addPset(new DatenPset("Neu-" + ++neuZaehler));
        tabellePset();
        notifyProgramSetChanged();
    }

    private void setLoeschen() {
        var rows = tabellePset.getSelectedRows();
        if (rows.length > 0) {
            DatenPset pSet;
            String text;
            if (rows.length == 1) {
                pSet = listePset.get(tabellePset.convertRowIndexToModel(rows[0]));
                text = pSet.getName();
            } else {
                text = rows.length + " Set löschen?";
            }
            var ret = JOptionPane.showConfirmDialog(parentComponent, text, "Löschen?", JOptionPane.YES_NO_OPTION);
            if (ret == JOptionPane.OK_OPTION) {
                for (int i = rows.length - 1; i >= 0; --i) {
                    var delRow = tabellePset.convertRowIndexToModel(rows[i]);
                    ((NonEditableTableModel) tabellePset.getModel()).removeRow(delRow);
                    listePset.remove(delRow);
                }
                tabellePset();
                notifyProgramSetChanged();
            }
        } else {
            NoSelectionErrorDialog.show(this);
        }
    }

    private void setExport() {
        var rows = tabellePset.getSelectedRows();
        if (rows.length > 0) {
            var liste = Arrays.stream(rows)
                    .map(tabellePset::convertRowIndexToModel)
                    .mapToObj(listePset::get)
                    .toList();

            var entryName = liste.getFirst().getName();
            var name = entryName.isEmpty() ? "Name.xml" : entryName + ".xml";
            var applicationConfiguration = ApplicationConfiguration.getInstance();
            var fileName = FilenameUtils.replaceLeerDateiname(name, false,
                    applicationConfiguration.getUseFilenameReplaceTable(),
                    applicationConfiguration.getOnlyAsciiFilenames());
            var resultFile = FileDialogs.chooseSaveFileLocation(parentComponent,"PSet exportieren", fileName);
            if (resultFile != null) {
                var ziel = resultFile.getAbsolutePath();

                var configWriter = new IoXmlSchreiben();
                configWriter.exportPset(liste.toArray(new DatenPset[0]), ziel);
                JOptionPane.showMessageDialog(this,
                        "Das Programmset wurde erfolgreich exportiert.",
                        Konstanten.PROGRAMMNAME, JOptionPane.INFORMATION_MESSAGE);
            }
        } else {
            NoSelectionErrorDialog.show(this);
        }
    }

    private boolean isEmptyProgramEntry(DatenProg prog) {
        for (int i = 0; i < DatenProg.PROGRAMM_RESTART; ++i) {
            var value = prog.get(i);
            if (value != null && !value.isBlank()) {
                return false;
            }
        }
        return !prog.isRestart() && !prog.isDownloadManager();
    }

    private void updateProgramMoveButtons(DatenProg prog) {
        var enabled = prog != null && !isEmptyProgramEntry(prog) && !isProgramTableSorted();
        jButtonProgAuf.setEnabled(enabled);
        jButtonProgAb.setEnabled(enabled);
    }

    private void updateProgramMoveButtonsForSelection() {
        updateProgramMoveButtons(getSelectedProgramEntry());
    }

    private DatenProg getSelectedProgramEntry() {
        if (currentProgramList == null) {
            return null;
        }
        var viewRow = tabelleProgramme.getSelectedRow();
        if (viewRow == -1) {
            return null;
        }
        var modelRow = tabelleProgramme.convertRowIndexToModel(viewRow);
        if (modelRow < 0 || modelRow >= currentProgramList.size()) {
            return null;
        }
        return currentProgramList.get(modelRow);
    }

    private boolean isProgramTableSorted() {
        return programTableSorter != null && !programTableSorter.getSortKeys().isEmpty();
    }

    private void progNeueZeile(DatenProg prog) {
        var gruppe = getPset();
        if (gruppe != null) {
            var newRow = gruppe.getListeProg().size();
            gruppe.addProg(prog);
            tabelleProgramme();
            selectProgramModelRow(newRow);
        }
    }

    private void selectProgramModelRow(int modelRow) {
        var viewRow = tabelleProgramme.convertRowIndexToView(modelRow);
        if (viewRow != -1) {
            tabelleProgramme.setRowSelectionInterval(viewRow, viewRow);
            tabelleProgramme.scrollRectToVisible(tabelleProgramme.getCellRect(viewRow, 0, true));
        }
    }

    private void progAufAb(boolean auf) {
        if (isProgramTableSorted()) {
            return;
        }
        var rows = tabelleProgramme.getSelectedRow();
        if (rows != -1) {
            var row = tabelleProgramme.convertRowIndexToModel(rows);
            var neu = getCurrentProgramList().moveEntryAtIndex(row, auf);
            tabelleProgramme();
            selectProgramModelRow(neu);
        } else {
            NoSelectionErrorDialog.show(this);
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
                    var listeProg = getCurrentProgramList();
                    DatenProg prog = listeProg.get(row);
                    prog.setProgramPath(jTextFieldProgPfad.getText());
                    prog.setSwitches(jTextFieldProgSchalter.getText());
                    prog.setName(jTextFieldProgName.getText());
                    prog.setTargetFileName(jTextFieldProgZielDateiName.getText());
                    prog.setSuffix(jTextFieldProgSuffix.getText());
                    prog.setPrefix(jTextFieldProgPraefix.getText());
                    listeProg.fireEntryChanged(row);
                    updateProgramMoveButtons(prog);
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

        private void eingabe() {
            if (!stopBeob) {
                final int row = tabellePset.getSelectedRow();
                if (row != -1) {
                    stopBeob = true;
                    final int modelIndex = tabellePset.convertRowIndexToModel(row);
                    var datenPset = listePset.get(modelIndex);
                    datenPset.set(psetIndex, textComponent.getText());
                    if (psetIndex == DatenPset.PROGRAMMSET_NAME) {
                        tabellePset.getModel().setValueAt(jTextFieldSetName.getText(), modelIndex, DatenPset.PROGRAMMSET_NAME);
                        jTabbedPane.setTitleAt(0, "Set Name: " + datenPset.getName());
                    }
                    if (fireUpdate)
                        notifyProgramSetChanged();
                    stopBeob = false;
                } else {
                    NoSelectionErrorDialog.show(null);
                }
            }
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
        jButtonHilfe.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-help.png")));
        jButtonHilfe.setToolTipText("Hilfe anzeigen");

        //---- jButtonPruefen ----
        jButtonPruefen.setText("Pr\u00fcfen");
        jButtonPruefen.setToolTipText("Programmpfade pr\u00fcfen");

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
                        jLabel6.setText("Set Name:");

                        //======== jPanel4 ========
                        {
                            jPanel4.setBorder(new TitledBorder("Beschreibung"));

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
                                        .addComponent(jScrollPane2, GroupLayout.DEFAULT_SIZE, 234, Short.MAX_VALUE)
                                        .addContainerGap())
                            );
                        }

                        //======== jPanel6 ========
                        {
                            jPanel6.setBorder(new TitledBorder("Funktion"));

                            //---- jCheckBoxSpeichern ----
                            jCheckBoxSpeichern.setText("Speichern");

                            //---- jCheckBoxButton ----
                            jCheckBoxButton.setText("Button");

                            //---- jCheckBoxAbo ----
                            jCheckBoxAbo.setText("Abo");

                            //---- jButtonAbspielen ----
                            jButtonAbspielen.setText("Abspielen via schwarzem Play-Button");

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
                                        .addContainerGap(59, Short.MAX_VALUE))
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
                        jLabelMeldungAbspielen.setText("kein Set zum Abspielen ausgew\u00e4hlt!");

                        //---- jLabelMeldungSeichern ----
                        jLabelMeldungSeichern.setText("kein Set zum Speichern ausgew\u00e4hlt!");

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
                jTabbedPane.addTab("Einstellungen", jPanelDetails);

                //======== jPanel10 ========
                {

                    //======== jPanel5 ========
                    {
                        jPanel5.setBorder(new EtchedBorder());

                        //---- jButtonGruppeFarbe ----
                        jButtonGruppeFarbe.setText("Farbe");
                        jButtonGruppeFarbe.setToolTipText("Farbauswahldialog anzeigen");

                        //---- jButtonGruppeStandardfarbe ----
                        jButtonGruppeStandardfarbe.setText("Standardfarbe");
                        jButtonGruppeStandardfarbe.setToolTipText("Farbe zur\u00fccksetzen");

                        //---- jLabel11 ----
                        jLabel11.setText("Wird das Set als Button verwendet,");

                        //---- jLabel13 ----
                        jLabel13.setText("kann damit die Schriftfarbe ver\u00e4ndert werden.");

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
                                    .addContainerGap(319, Short.MAX_VALUE))
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
                                .addContainerGap(431, Short.MAX_VALUE))
                    );
                }
                jTabbedPane.addTab("Aussehen", jPanel10);

                //======== jPanel9 ========
                {

                    //======== jPanel1 ========
                    {
                        jPanel1.setBorder(new EtchedBorder());

                        //---- jLabel7 ----
                        jLabel7.setText("Zielpfad:");

                        //---- jButtonGruppePfad ----
                        jButtonGruppePfad.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png")));
                        jButtonGruppePfad.setToolTipText("Pfad ausw\u00e4hlen");

                        //---- jCheckBoxThema ----
                        jCheckBoxThema.setText("Einen Unterordner mit dem Thema / Abo-Zielpfad anlegen");
                        jCheckBoxThema.setToolTipText("im Zielverzeichnis wird ein Unterordner mit dem Namen des Themas zum Speichern der Filme angelegt");

                        //---- jLabel8 ----
                        jLabel8.setText("Zieldateiname:");

                        //---- jCheckBoxLaenge ----
                        jCheckBoxLaenge.setText("Ganzen Dateinamen beschr\u00e4nken auf:");
                        jCheckBoxLaenge.setToolTipText("die L\u00e4nge des Dateinamens wird auf die Anzahl Zeichen beschr\u00e4nkt");

                        //---- jSpinnerLaenge ----
                        jSpinnerLaenge.setModel(new SpinnerNumberModel(25, 10, 200, 1));

                        //---- jLabel12 ----
                        jLabel12.setText("Zeichen");

                        //---- jLabel15 ----
                        jLabel15.setText("(ist der Abo-Zielpfad leer, wird das Thema verwendet)");

                        //---- jCheckBoxField ----
                        jCheckBoxField.setText("Einzelne Felder beschr\u00e4nken auf:");

                        //---- jSpinnerField ----
                        jSpinnerField.setModel(new SpinnerNumberModel(10, 3, 100, 1));

                        //---- jLabel16 ----
                        jLabel16.setText("Zeichen");

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
                                                    .addGap(0, 203, Short.MAX_VALUE)))
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
                                .addContainerGap(285, Short.MAX_VALUE))
                    );
                }
                jTabbedPane.addTab("Speicherziel", jPanel9);

                //======== jPanel11 ========
                {

                    //======== jPanel8 ========
                    {
                        jPanel8.setBorder(new TitledBorder("Direkt speichern"));

                        //---- jLabel10 ----
                        jLabel10.setText("Direkter Download, Pr\u00e4fix ( z.B. http ):");

                        //---- jLabel5 ----
                        jLabel5.setText("Suffix ( z.B. mp4,mp3):");

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
                    jTextArea1.setBackground(new Color(0xeeeeee));
                    jTextArea1.setColumns(20);
                    jTextArea1.setRows(4);
                    jTextArea1.setText("Filme, deren URL mit \"Pr\u00e4fix\" beginnt und mit \"Suffix\" endet, werden nicht\nmit einem Hilfsprogramm gespeichert, sondern direkt geladen.\n\nEine geringere Aufl\u00f6sung ist nicht bei jedem Sender m\u00f6glich, es wird dann in der gleichen\nAufl\u00f6sung geladen.");
                    jTextArea1.setBorder(null);

                    //======== jPanel12 ========
                    {
                        jPanel12.setBorder(new TitledBorder("Film downloaden in"));

                        //---- jRadioButtonAufloesungNormal ----
                        jRadioButtonAufloesungNormal.setSelected(true);
                        jRadioButtonAufloesungNormal.setText("Mittlere Qualit\u00e4t");

                        //---- jRadioButtonAufloesungKlein ----
                        jRadioButtonAufloesungKlein.setText("Niedrige Qualit\u00e4t");

                        //---- jRadioButtonAufloesungHD ----
                        jRadioButtonAufloesungHD.setText("H\u00f6chste/Hohe Qualit\u00e4t");

                        //---- jLabel14 ----
                        jLabel14.setText("Wenn es die Qualit\u00e4tstufe im Angebot nicht gibt, wird die n\u00e4chstkleinere genommen.");

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
                        jPanel13.setBorder(new TitledBorder("Nach dem Speichern"));

                        //---- jCheckBoxInfodatei ----
                        jCheckBoxInfodatei.setText("Infodatei anlegen: \"Filmname.txt\"");

                        //---- jCheckBoxSpotlight ----
                        jCheckBoxSpotlight.setText("Filmbeschreibung als Finder-Kommentar f\u00fcr Spotlight speichern (nur OS X)");

                        //---- jCheckBoxSubtitle ----
                        jCheckBoxSubtitle.setText("Untertitel speichern: \"Filmname.xxx\"");

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
                jTabbedPane.addTab("Download", jPanel11);

                //======== jPanelProgramme ========
                {

                    //======== jScrollPane1 ========
                    {
                        jScrollPane1.setBorder(new TitledBorder(null, "Titel", TitledBorder.LEFT, TitledBorder.TOP));

                        //---- jTableProgramme ----
                        jTableProgramme.setModel(new NonEditableTableModel());
                        jTableProgramme.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
                        jScrollPane1.setViewportView(jTableProgramme);
                    }

                    //======== jPanel2 ========
                    {
                        jPanel2.setBorder(new EtchedBorder());

                        //---- jButtonProgPlus ----
                        jButtonProgPlus.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-add.png")));
                        jButtonProgPlus.setToolTipText("neues Programm anlegen");

                        //---- jButtonProgMinus ----
                        jButtonProgMinus.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-remove.png")));
                        jButtonProgMinus.setToolTipText("markiertes Programm l\u00f6schen");

                        //---- jButtonProgDuplizieren ----
                        jButtonProgDuplizieren.setText("Duplizieren");
                        jButtonProgDuplizieren.setToolTipText("markierte Zeile duplizieren");

                        //---- jButtonProgAuf ----
                        jButtonProgAuf.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-move-up.png")));
                        jButtonProgAuf.setToolTipText("markierte Zeile eins nach oben");

                        //---- jButtonProgAb ----
                        jButtonProgAb.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-move-down.png")));
                        jButtonProgAb.setToolTipText("markierte Zeile eins nach unten");

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
                        jLabel.setText("Programm:");

                        //---- jButtonProgPfad ----
                        jButtonProgPfad.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png")));
                        jButtonProgPfad.setToolTipText("Programm ausw\u00e4hlen");

                        //---- jLabel1 ----
                        jLabel1.setText("Schalter:");

                        //---- jLabel2 ----
                        jLabel2.setText("Beschreibung:");

                        //---- jLabel3 ----
                        jLabel3.setText("Pr\u00e4fix (z.B. http):");

                        //---- jLabel4 ----
                        jLabel4.setText("Suffix ( z.B. mp4,mp3):");

                        //---- jCheckBoxRestart ----
                        jCheckBoxRestart.setText("Fehlgeschlagene Downloads wieder starten");

                        //---- jLabel9 ----
                        jLabel9.setText("Zieldateiname:");

                        //---- jCheckBoxRemoteDownload ----
                        jCheckBoxRemoteDownload.setText("Externer Downloadmanager");

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
                                                    .addComponent(jTextFieldProgSuffix, GroupLayout.DEFAULT_SIZE, 189, Short.MAX_VALUE))
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
                                .addComponent(jScrollPane1, GroupLayout.DEFAULT_SIZE, 187, Short.MAX_VALUE)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jPanel2, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jPanelProgrammDetails, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                    );
                }
                jTabbedPane.addTab("Hilfsprogramme", jPanelProgramme);
            }
            jSplitPane1.setRightComponent(jTabbedPane);

            //======== jPanel3 ========
            {
                jPanel3.setLayout(new MigLayout(
                    new LC().insets("0").hideMode(3).gridGap("5", "5"),
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
                            "Title 1", "Title 2", "Title 3"
                        }
                    ));
                    jTablePset.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
                    jTablePset.setMinimumSize(new Dimension(50, 16));
                    jTablePset.setPreferredSize(new Dimension(150, 50));
                    jScrollPane3.setViewportView(jTablePset);
                }
                jPanel3.add(jScrollPane3, new CC().cell(0, 0, 3, 1));

                //---- jButtonGruppeDuplizieren ----
                jButtonGruppeDuplizieren.setText("Duplizieren");
                jButtonGruppeDuplizieren.setToolTipText("Programmgruppe kopieren");
                jPanel3.add(jButtonGruppeDuplizieren, new CC().cell(0, 3, 3, 1));

                //---- jButtonExport ----
                jButtonExport.setText("Export");
                jButtonExport.setToolTipText("Programmgruppe in Datei exportieren");
                jPanel3.add(jButtonExport, new CC().cell(0, 4, 3, 1));

                //---- jButtonGruppeNeu ----
                jButtonGruppeNeu.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-add.png")));
                jButtonGruppeNeu.setToolTipText("neue Programmgruppe anlegen");
                jPanel3.add(jButtonGruppeNeu, new CC().cell(0, 2).alignX("center").growX(0));

                //---- jButtonGruppeLoeschen ----
                jButtonGruppeLoeschen.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-remove.png")));
                jButtonGruppeLoeschen.setToolTipText("Programmgruppe l\u00f6schen");
                jPanel3.add(jButtonGruppeLoeschen, new CC().cell(2, 2).alignX("center").growX(0));

                //---- jButtonGruppeAuf ----
                jButtonGruppeAuf.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-move-up.png")));
                jButtonGruppeAuf.setToolTipText("Programmgruppe nach oben schieben");
                jPanel3.add(jButtonGruppeAuf, new CC().cell(0, 1).alignX("center").growX(0));
                jPanel3.add(hSpacer1, new CC().cell(1, 1));

                //---- jButtonGruppeAb ----
                jButtonGruppeAb.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-move-down.png")));
                jButtonGruppeAb.setToolTipText("Programmgruppe nach unten schieben");
                jPanel3.add(jButtonGruppeAb, new CC().cell(2, 1).alignX("center").growX(0));
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
