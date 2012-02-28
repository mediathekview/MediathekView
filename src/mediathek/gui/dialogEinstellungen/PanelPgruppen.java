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

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.Iterator;
import java.util.LinkedList;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.event.*;
import mediathek.Daten;
import mediathek.Log;
import mediathek.controller.filme.filmeImportieren.MediathekListener;
import mediathek.daten.DDaten;
import mediathek.daten.DatenPgruppe;
import mediathek.daten.DatenProg;
import mediathek.gui.PanelVorlage;
import mediathek.gui.beobachter.CellRendererPguppen;
import mediathek.gui.dialog.DialogHilfeProgramme;
import mediathek.gui.dialog.DialogZielDatei;
import mediathek.tool.*;

public class PanelPgruppen extends PanelVorlage {

    private int neuZaehler = 0;
    private String exportPfad = "";

    public PanelPgruppen(DDaten d) {
        super(d);
        initComponents();
        init();
    }

    private void init() {
        //Programme
        Daten.addAdListener(new MediathekListener(MediathekListener.EREIGNIS_LISTE_PGRUPPE, PanelPgruppen.class.getSimpleName()) {

            @Override
            public void ping() {
                if (!stopBeob) {
                    tabellePgruppe();
                }
            }
        });
        BeobProgDoc beobDoc = new BeobProgDoc();
        jTableProgramme.getSelectionModel().addListSelectionListener(new BeobTableSelect());
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
        //Pgruppe
        jButtonAbspielen.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                getPgruppe().setAbspielen(ddaten);
                nurtabellePgruppe();
                notifyPgruppe();
            }
        });
        jCheckBoxSpeichern.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                getPgruppe().setSpeichern(jCheckBoxSpeichern.isSelected());
                nurtabellePgruppe();
                notifyPgruppe();
            }
        });
        jCheckBoxButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                getPgruppe().setButton(jCheckBoxButton.isSelected());
                nurtabellePgruppe();
                notifyPgruppe();
            }
        });
        jCheckBoxAbo.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                getPgruppe().setAbo(jCheckBoxAbo.isSelected());
                nurtabellePgruppe();
                notifyPgruppe();
            }
        });
        jCheckBoxLaenge.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                getPgruppe().arr[DatenPgruppe.PROGRAMMGRUPPE_LAENGE_BESCHRAENKEN_NR] = Boolean.toString(jCheckBoxLaenge.isSelected());
                nurtabellePgruppe();
            }
        });
        jSpinnerLaenge.addChangeListener(new ChangeListener() {

            @Override
            public void stateChanged(ChangeEvent e) {
                getPgruppe().arr[DatenPgruppe.PROGRAMMGRUPPE_MAX_LAENGE_NR] = String.valueOf(((Number) jSpinnerLaenge.getModel().getValue()).intValue());
//                nurtabellePgruppe();
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
        jTextFieldGruppeName.getDocument().addDocumentListener(new BeobGruppenDoc(jTextFieldGruppeName, DatenPgruppe.PROGRAMMGRUPPE_NAME_NR));
        jTextFieldGruppeDirektSuffix.getDocument().addDocumentListener(
                new BeobGruppenDoc(jTextFieldGruppeDirektSuffix, DatenPgruppe.PROGRAMMGRUPPE_SUFFIX_DIREKT_NR));
        jTextFieldGruppeDirektPraefix.getDocument().addDocumentListener(
                new BeobGruppenDoc(jTextFieldGruppeDirektPraefix, DatenPgruppe.PROGRAMMGRUPPE_PRAEFIX_DIREKT_NR));
        jTextFieldGruppeZielName.getDocument().addDocumentListener(new BeobGruppenDoc(jTextFieldGruppeZielName,
                DatenPgruppe.PROGRAMMGRUPPE_ZIEL_DATEINAME_NR));
        jTextFieldGruppeZielPfad.getDocument().addDocumentListener(
                new BeobGruppenDoc(jTextFieldGruppeZielPfad, DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR));
        //rest
        jButtonHilfe.addActionListener(new BeobHilfeTabelle());
        jButtonPruefen.addActionListener(new BeobPuefen());
        jTablePgruppen.setDefaultRenderer(Object.class, new CellRendererPguppen(ddaten));
        jTablePgruppen.getSelectionModel().addListSelectionListener(new BeobTableSelectPgruppe());
        tabellePgruppe();
    }

    private void tabellePgruppe() {
        nurtabellePgruppe();
        tabelleProgramme();
    }

    private void nurtabellePgruppe() {
        stopBeob = true;
//        int i = jTablePgruppen.getSelectedRow();
        getSpalten(jTablePgruppen);
        jTablePgruppen.setModel(ddaten.listePgruppe.getModel());
        spaltenSetzen();
        setSpalten(jTablePgruppen);
        jTablePgruppen.updateUI();
        stopBeob = false;
    }

    private void spaltenSetzen() {
        for (int i = 0; i < jTablePgruppen.getColumnCount(); ++i) {
            if (i == DatenPgruppe.PROGRAMMGRUPPE_NAME_NR) {
                jTablePgruppen.getColumnModel().getColumn(i).setMinWidth(10);
                jTablePgruppen.getColumnModel().getColumn(i).setMaxWidth(3000);
                jTablePgruppen.getColumnModel().getColumn(i).setPreferredWidth(200);
            } else if (i == DatenPgruppe.PROGRAMMGRUPPE_IST_SPEICHERN_NR
                    || i == DatenPgruppe.PROGRAMMGRUPPE_IST_ABSPIELEN_NR
                    || i == DatenPgruppe.PROGRAMMGRUPPE_IST_BUTTON_NR
                    || i == DatenPgruppe.PROGRAMMGRUPPE_IST_ABO_NR) {
                jTablePgruppen.getColumnModel().getColumn(i).setMinWidth(10);
                jTablePgruppen.getColumnModel().getColumn(i).setMaxWidth(3000);
                jTablePgruppen.getColumnModel().getColumn(i).setPreferredWidth(100);
            } else {
                jTablePgruppen.getColumnModel().getColumn(i).setMinWidth(0);
                jTablePgruppen.getColumnModel().getColumn(i).setMaxWidth(0);
                jTablePgruppen.getColumnModel().getColumn(i).setPreferredWidth(0);
            }
        }
    }

    private void tabelleProgramme() {
        //Tabelle mit den Programmen füllen
        DatenPgruppe pgruppe = getPgruppe();
        stopBeob = true;
        jTextFieldGruppeName.setEnabled(pgruppe != null);
        jTextFieldGruppeDirektSuffix.setEnabled(pgruppe != null);
        jTextFieldGruppeDirektPraefix.setEnabled(pgruppe != null);
        jTextFieldGruppeZielName.setEnabled(pgruppe != null);
        jTextFieldGruppeZielPfad.setEnabled(pgruppe != null);
        jButtonGruppePfad.setEnabled(pgruppe != null);
        jButtonAbspielen.setEnabled(pgruppe != null);
        jCheckBoxSpeichern.setEnabled(pgruppe != null);
        jCheckBoxButton.setEnabled(pgruppe != null);
        jCheckBoxAbo.setEnabled(pgruppe != null);
        jCheckBoxLaenge.setEnabled(pgruppe != null);
        jSpinnerLaenge.setEnabled(pgruppe != null);
        if (pgruppe != null) {
            if (pgruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_MAX_LAENGE_NR].equals("")) {
                jSpinnerLaenge.setValue(GuiKonstanten.LAENGE_DATEINAME);
                pgruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_MAX_LAENGE_NR] = String.valueOf(GuiKonstanten.LAENGE_DATEINAME);
            } else {
                jSpinnerLaenge.setValue(Integer.parseInt(pgruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_MAX_LAENGE_NR]));
            }
            jCheckBoxLaenge.setSelected(Boolean.parseBoolean(pgruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_LAENGE_BESCHRAENKEN_NR]));
            jScrollPane1.setBorder(javax.swing.BorderFactory.createTitledBorder(null, pgruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR], javax.swing.border.TitledBorder.LEFT, javax.swing.border.TitledBorder.TOP));
            jTabbedPane.setTitleAt(0, pgruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR]);
            jTextFieldGruppeName.setText(pgruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR]);
            jTextFieldGruppeDirektSuffix.setText(pgruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_SUFFIX_DIREKT_NR]);
            jTextFieldGruppeDirektPraefix.setText(pgruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_PRAEFIX_DIREKT_NR]);
            jTextFieldGruppeZielName.setText(pgruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_DATEINAME_NR]);
            jTextFieldGruppeZielPfad.setText(pgruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR]);
            jCheckBoxSpeichern.setSelected(pgruppe.istSpeichern());
            jCheckBoxButton.setSelected(pgruppe.istButton());
            jCheckBoxAbo.setSelected(pgruppe.istAbo());
        } else {
            jScrollPane1.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "", javax.swing.border.TitledBorder.LEFT, javax.swing.border.TitledBorder.TOP));
            jTabbedPane.setTitleAt(0, "           ");
            //jSpinnerLaenge.setValue(GuiKonstanten.MAX_LAENGE_DATEINAME); Exception!
            jCheckBoxLaenge.setSelected(false);
            jTextFieldGruppeName.setText("");
            jTextFieldGruppeDirektSuffix.setText("");
            jTextFieldGruppeDirektPraefix.setText("");
            jTextFieldGruppeZielName.setText("");
            jTextFieldGruppeZielPfad.setText("");
        }
        if (pgruppe != null) {
            jTableProgramme.setModel(pgruppe.getListeProg().getModel());
            if (jTableProgramme.getRowCount() > 0) {
                spaltenSetzenProgramme();
                jTableProgramme.setRowSelectionInterval(0, 0);
                jTableProgramme.scrollRectToVisible(jTableProgramme.getCellRect(0, 0, true));
            }
        } else {
            jTableProgramme.setModel(new TModel(new Object[0][DatenProg.PROGRAMM_MAX_ELEM], DatenProg.PROGRAMM_COLUMN_NAMES));
        }
        stopBeob = false;
        fillTextProgramme();
    }

    public void spaltenSetzenProgramme() {
        for (int i = 0; i < jTableProgramme.getColumnCount(); ++i) {
            if (i == DatenProg.PROGRAMM_PRAEFIX_NR
                    || i == DatenProg.PROGRAMM_RESTART_NR
                    || i == DatenProg.PROGRAMM_SUFFIX_NR) {
                jTableProgramme.getColumnModel().getColumn(i).setMinWidth(10);
                jTableProgramme.getColumnModel().getColumn(i).setMaxWidth(3000);
                jTableProgramme.getColumnModel().getColumn(i).setPreferredWidth(100);
            } else {
                jTableProgramme.getColumnModel().getColumn(i).setMinWidth(10);
                jTableProgramme.getColumnModel().getColumn(i).setMaxWidth(3000);
                jTableProgramme.getColumnModel().getColumn(i).setPreferredWidth(200);
            }
        }
    }

    private void notifyPgruppe() {
        Daten.notifyMediathekListener(MediathekListener.EREIGNIS_LISTE_PGRUPPE, PanelPgruppen.class.getSimpleName());
    }

    private void fillTextProgramme() {
        //Textfelder mit Programmdaten füllen
        stopBeob = true;
        int row = jTableProgramme.getSelectedRow();
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
            DatenProg prog = getPgruppe().getProg(jTableProgramme.convertRowIndexToModel(row));
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
        stopBeob = false;
    }

    //Pgruppe
    private DatenPgruppe getPgruppe() {
        DatenPgruppe ret = null;
        int row = jTablePgruppen.getSelectedRow();
        if (row != -1) {
            ret = ddaten.listePgruppe.get(jTablePgruppen.convertRowIndexToModel(row));
        }
        return ret;
    }

    private void gruppenNamePruefen() {
        //doppelte Gruppennamen suchen
        int row = jTablePgruppen.getSelectedRow();
        if (row != -1) {
            int foundgruppe = 0;
            Iterator<DatenPgruppe> it = ddaten.listePgruppe.iterator();
            while (it.hasNext()) {
                DatenPgruppe gruppe = it.next();
                if (jTextFieldGruppeName.getText().equals(gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR])) {
                    ++foundgruppe;
                }
            }
            if (foundgruppe > 1) {
                jTextFieldGruppeName.setBackground(Color.ORANGE);
            } else {
                jTextFieldGruppeName.setBackground(Color.WHITE);
            }
        }
    }

    private void gruppeAufAb(boolean auf) {
        int row = jTablePgruppen.getSelectedRow();
        if (row != -1) {
            int neu = ddaten.listePgruppe.auf(row, auf);
            tabellePgruppe();
            jTablePgruppen.setRowSelectionInterval(neu, neu);
            jTablePgruppen.scrollRectToVisible(jTablePgruppen.getCellRect(neu, 0, false));
            Daten.setGeaendert();
            notifyPgruppe();
        } else {
            new HinweisKeineAuswahl().zeigen();
        }
    }

    private void gruppeNeu() {
        //DatenPgruppe(String name, String suffix, String farbe, String zielPfad, String zielDateiname) {
        ddaten.listePgruppe.addPgruppe(new DatenPgruppe("Neu-" + ++neuZaehler));
        tabellePgruppe();
        notifyPgruppe();
    }

    private void gruppeLoeschen() {
        int rows[] = jTablePgruppen.getSelectedRows();
        if (rows.length > 0) {
            DatenPgruppe pGruppe;
            String text;
            if (rows.length == 1) {
                pGruppe = ddaten.listePgruppe.get(jTablePgruppen.convertRowIndexToModel(rows[0]));
                text = pGruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR];
            } else {
                text = rows.length + " Programmgruppen löschen?";
            }
            int ret = JOptionPane.showConfirmDialog(null, text, "Löschen?", JOptionPane.YES_NO_OPTION);
            if (ret == JOptionPane.OK_OPTION) {
                for (int i = rows.length - 1; i >= 0; --i) {
                    int delRow = jTablePgruppen.convertRowIndexToModel(rows[i]);
                    ((TModel) jTablePgruppen.getModel()).removeRow(delRow);
                    ddaten.listePgruppe.remove(delRow);
                }
                tabellePgruppe();
                notifyPgruppe();
            }
        } else {
            new HinweisKeineAuswahl().zeigen();
        }
    }

    private void gruppeExport() {
        LinkedList<DatenPgruppe> liste = new LinkedList<DatenPgruppe>();
        int rows[] = jTablePgruppen.getSelectedRows();
        if (rows.length > 0) {
            DatenPgruppe pGruppe;
            for (int i = 0; i < rows.length; ++i) {
                String name;
                int delRow = jTablePgruppen.convertRowIndexToModel(rows[i]);
                pGruppe = ddaten.listePgruppe.get(delRow);
                if (pGruppe != null) {
                    liste.add(pGruppe);
                }
            }
            String name = liste.getFirst().arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR].equals("") ? "Name.xml" : liste.getFirst().arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR] + ".xml";
            DialogZiel dialogZiel = new DialogZiel(null, true, exportPfad, GuiFunktionen.replaceLeerDateiname(name, true /* pfadtrennerEntfernen */));
            dialogZiel.setVisible(true);
            if (dialogZiel.ok) {
                if (dialogZiel.ziel.contains(File.separator)) {
                    exportPfad = dialogZiel.ziel.substring(0, dialogZiel.ziel.lastIndexOf(File.separator));
                }
                ddaten.ioXmlSchreiben.exportPgruppe(liste.toArray(new DatenPgruppe[0]), dialogZiel.ziel);
            }
        } else {
            new HinweisKeineAuswahl().zeigen();
        }
    }

    private void progNeueZeile(DatenProg prog) {
        DatenPgruppe gruppe = getPgruppe();
        if (gruppe != null) {
            gruppe.addProg(prog);
            tabelleProgramme();
            Daten.setGeaendert();
        }
    }

    private void progAufAb(boolean auf) {
        int rows = jTableProgramme.getSelectedRow();
        if (rows != -1) {
            int row = jTableProgramme.convertRowIndexToModel(rows);
            int neu = getPgruppe().getListeProg().auf(row, auf);
            tabelleProgramme();
            jTableProgramme.setRowSelectionInterval(neu, neu);
            jTableProgramme.scrollRectToVisible(jTableProgramme.getCellRect(neu, 0, true));
            Daten.setGeaendert();
        } else {
            new HinweisKeineAuswahl().zeigen();
        }

    }

    private void dialogHilfe() {
        ///////////////////////////////////////////////////////
        new DialogHilfeProgramme(null, true, ddaten, Texte.PROGRAMMGRUPPE_ABO).setVisible(true);
        tabellePgruppe();
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jButtonHilfe = new javax.swing.JButton();
        jButtonPruefen = new javax.swing.JButton();
        jTabbedPane = new javax.swing.JTabbedPane();
        jPanelPgruppe = new javax.swing.JPanel();
        jScrollPane3 = new javax.swing.JScrollPane();
        jTablePgruppen = new javax.swing.JTable();
        jPanel6 = new javax.swing.JPanel();
        jButtonGruppeNeu = new javax.swing.JButton();
        jButtonGruppeLoeschen = new javax.swing.JButton();
        jButtonGruppeAuf = new javax.swing.JButton();
        jButtonGruppeAb = new javax.swing.JButton();
        jButtonGruppeDuplizieren = new javax.swing.JButton();
        jButtonExport = new javax.swing.JButton();
        jCheckBoxSpeichern = new javax.swing.JCheckBox();
        jCheckBoxButton = new javax.swing.JCheckBox();
        jCheckBoxAbo = new javax.swing.JCheckBox();
        jButtonAbspielen = new javax.swing.JButton();
        jPanelDetails = new javax.swing.JPanel();
        jPanel7 = new javax.swing.JPanel();
        jTextFieldGruppeDirektSuffix = new javax.swing.JTextField();
        jLabel5 = new javax.swing.JLabel();
        jButtonGruppeFarbe = new javax.swing.JButton();
        jButtonGruppeStandardfarbe = new javax.swing.JButton();
        jLabel7 = new javax.swing.JLabel();
        jTextFieldGruppeZielPfad = new javax.swing.JTextField();
        jLabel8 = new javax.swing.JLabel();
        jTextFieldGruppeZielName = new javax.swing.JTextField();
        jButtonGruppePfad = new javax.swing.JButton();
        jTextFieldGruppeDirektPraefix = new javax.swing.JTextField();
        jLabel10 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        jTextFieldGruppeName = new javax.swing.JTextField();
        jCheckBoxLaenge = new javax.swing.JCheckBox();
        jSpinnerLaenge = new javax.swing.JSpinner();
        jSeparator1 = new javax.swing.JSeparator();
        jPanelProgramme = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTableProgramme = new javax.swing.JTable();
        jPanel2 = new javax.swing.JPanel();
        jButtonProgPlus = new javax.swing.JButton();
        jButtonProgMinus = new javax.swing.JButton();
        jButtonProgDuplizieren = new javax.swing.JButton();
        jButtonProgAuf = new javax.swing.JButton();
        jButtonProgAb = new javax.swing.JButton();
        jPanelProgrammDetails = new javax.swing.JPanel();
        jLabel = new javax.swing.JLabel();
        jTextFieldProgPfad = new javax.swing.JTextField();
        jButtonProgPfad = new javax.swing.JButton();
        jTextFieldProgSchalter = new javax.swing.JTextField();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jTextFieldProgName = new javax.swing.JTextField();
        jLabel3 = new javax.swing.JLabel();
        jTextFieldProgPraefix = new javax.swing.JTextField();
        jLabel4 = new javax.swing.JLabel();
        jTextFieldProgSuffix = new javax.swing.JTextField();
        jCheckBoxRestart = new javax.swing.JCheckBox();
        jLabel9 = new javax.swing.JLabel();
        jTextFieldProgZielDateiName = new javax.swing.JTextField();

        jButtonHilfe.setText("Hilfe");
        jButtonHilfe.setToolTipText("Hilfedialog anzeigen");

        jButtonPruefen.setText("Prüfen");
        jButtonPruefen.setToolTipText("Programmpfade prüfen");

        jPanelPgruppe.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));

        jTablePgruppen.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {

            },
            new String [] {

            }
        ));
        jTablePgruppen.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        jScrollPane3.setViewportView(jTablePgruppen);

        jPanel6.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jButtonGruppeNeu.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/edit_add_16.png"))); // NOI18N
        jButtonGruppeNeu.setToolTipText("neue Programmgruppe anlegen");

        jButtonGruppeLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/edit_remove_16.png"))); // NOI18N
        jButtonGruppeLoeschen.setToolTipText("Programmgruppe löschen");

        jButtonGruppeAuf.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/up_blue_16.png"))); // NOI18N
        jButtonGruppeAuf.setToolTipText("Programmgruppe nach oben schieben");

        jButtonGruppeAb.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/down_blue_16.png"))); // NOI18N
        jButtonGruppeAb.setToolTipText("Programmgruppe nach unten schieben");

        jButtonGruppeDuplizieren.setText("Duplizieren");
        jButtonGruppeDuplizieren.setToolTipText("Programmgruppe kopieren");

        jButtonExport.setText("Export");

        jCheckBoxSpeichern.setText("Speichern");

        jCheckBoxButton.setText("Button");

        jCheckBoxAbo.setText("Abo");

        jButtonAbspielen.setText("Abspielen");

        javax.swing.GroupLayout jPanel6Layout = new javax.swing.GroupLayout(jPanel6);
        jPanel6.setLayout(jPanel6Layout);
        jPanel6Layout.setHorizontalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel6Layout.createSequentialGroup()
                        .addComponent(jButtonGruppeNeu)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonGruppeLoeschen)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonGruppeAuf)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonGruppeAb)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jButtonGruppeDuplizieren, javax.swing.GroupLayout.PREFERRED_SIZE, 125, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonExport, javax.swing.GroupLayout.PREFERRED_SIZE, 125, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(jPanel6Layout.createSequentialGroup()
                        .addComponent(jButtonAbspielen)
                        .addGap(5, 5, 5)
                        .addComponent(jCheckBoxSpeichern)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jCheckBoxButton)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jCheckBoxAbo)
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanel6Layout.setVerticalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jButtonExport)
                        .addComponent(jButtonGruppeDuplizieren))
                    .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                        .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jButtonGruppeNeu)
                            .addComponent(jButtonGruppeLoeschen))
                        .addComponent(jButtonGruppeAuf)
                        .addComponent(jButtonGruppeAb)))
                .addGap(18, 18, 18)
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jCheckBoxSpeichern)
                    .addComponent(jCheckBoxButton)
                    .addComponent(jCheckBoxAbo)
                    .addComponent(jButtonAbspielen))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout jPanelPgruppeLayout = new javax.swing.GroupLayout(jPanelPgruppe);
        jPanelPgruppe.setLayout(jPanelPgruppeLayout);
        jPanelPgruppeLayout.setHorizontalGroup(
            jPanelPgruppeLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelPgruppeLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelPgruppeLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane3, javax.swing.GroupLayout.DEFAULT_SIZE, 631, Short.MAX_VALUE)
                    .addComponent(jPanel6, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanelPgruppeLayout.setVerticalGroup(
            jPanelPgruppeLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelPgruppeLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane3, javax.swing.GroupLayout.DEFAULT_SIZE, 265, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel6, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        jTabbedPane.addTab("Programmgruppe", jPanelPgruppe);

        jPanelDetails.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));

        jPanel7.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jLabel5.setText("Suffix:");

        jButtonGruppeFarbe.setText("Farbe");
        jButtonGruppeFarbe.setToolTipText("Farbauswahldialog anzeigen");

        jButtonGruppeStandardfarbe.setText("Standardfarbe");
        jButtonGruppeStandardfarbe.setToolTipText("Farbe zurücksetzen");

        jLabel7.setText("Zielpfad:");

        jLabel8.setText("Zieldateiname:");

        jButtonGruppePfad.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N
        jButtonGruppePfad.setToolTipText("Pfad auswählen");

        jLabel10.setText("direkter Download, Präfix:");

        jLabel6.setText("Name:");

        jCheckBoxLaenge.setText("Länge auf Anzahl Zeichen beschränken:");

        jSpinnerLaenge.setModel(new javax.swing.SpinnerNumberModel(25, 12, 100, 1));

        javax.swing.GroupLayout jPanel7Layout = new javax.swing.GroupLayout(jPanel7);
        jPanel7.setLayout(jPanel7Layout);
        jPanel7Layout.setHorizontalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jSeparator1)
                    .addGroup(jPanel7Layout.createSequentialGroup()
                        .addComponent(jLabel10)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldGruppeDirektPraefix, javax.swing.GroupLayout.PREFERRED_SIZE, 127, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel5)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldGruppeDirektSuffix, javax.swing.GroupLayout.DEFAULT_SIZE, 210, Short.MAX_VALUE))
                    .addGroup(jPanel7Layout.createSequentialGroup()
                        .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel8)
                            .addComponent(jLabel7))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel7Layout.createSequentialGroup()
                                .addComponent(jTextFieldGruppeZielPfad)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonGruppePfad))
                            .addComponent(jTextFieldGruppeZielName)))
                    .addGroup(jPanel7Layout.createSequentialGroup()
                        .addComponent(jLabel6)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldGruppeName))
                    .addGroup(jPanel7Layout.createSequentialGroup()
                        .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel7Layout.createSequentialGroup()
                                .addComponent(jButtonGruppeFarbe)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonGruppeStandardfarbe))
                            .addGroup(jPanel7Layout.createSequentialGroup()
                                .addComponent(jCheckBoxLaenge)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jSpinnerLaenge, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );

        jPanel7Layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonGruppeFarbe, jButtonGruppeStandardfarbe});

        jPanel7Layout.setVerticalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jTextFieldGruppeName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel6))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel7)
                    .addComponent(jTextFieldGruppeZielPfad, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonGruppePfad))
                .addGap(7, 7, 7)
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel8)
                    .addComponent(jTextFieldGruppeZielName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jCheckBoxLaenge)
                    .addComponent(jSpinnerLaenge, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(18, 18, 18)
                .addComponent(jSeparator1, javax.swing.GroupLayout.PREFERRED_SIZE, 10, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel10)
                    .addComponent(jTextFieldGruppeDirektPraefix, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel5)
                    .addComponent(jTextFieldGruppeDirektSuffix, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(18, 18, 18)
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonGruppeFarbe)
                    .addComponent(jButtonGruppeStandardfarbe))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel7Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonGruppePfad, jTextFieldGruppeDirektPraefix, jTextFieldGruppeDirektSuffix, jTextFieldGruppeName, jTextFieldGruppeZielName, jTextFieldGruppeZielPfad});

        javax.swing.GroupLayout jPanelDetailsLayout = new javax.swing.GroupLayout(jPanelDetails);
        jPanelDetails.setLayout(jPanelDetailsLayout);
        jPanelDetailsLayout.setHorizontalGroup(
            jPanelDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelDetailsLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel7, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        jPanelDetailsLayout.setVerticalGroup(
            jPanelDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelDetailsLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel7, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(125, Short.MAX_VALUE))
        );

        jTabbedPane.addTab("Details", jPanelDetails);

        jScrollPane1.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Titel", javax.swing.border.TitledBorder.LEFT, javax.swing.border.TitledBorder.TOP));

        jTableProgramme.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {

            },
            new String [] {

            }
        ));
        jTableProgramme.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        jScrollPane1.setViewportView(jTableProgramme);

        jPanel2.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jButtonProgPlus.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/edit_add_16.png"))); // NOI18N
        jButtonProgPlus.setToolTipText("neues Programm anlegen");

        jButtonProgMinus.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/edit_remove_16.png"))); // NOI18N
        jButtonProgMinus.setToolTipText("markiertes Programm löschen");

        jButtonProgDuplizieren.setText("Duplizieren");
        jButtonProgDuplizieren.setToolTipText("markierte Zeile duplizieren");

        jButtonProgAuf.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/up_blue_16.png"))); // NOI18N
        jButtonProgAuf.setToolTipText("markierte Zeile eins nach oben");

        jButtonProgAb.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/down_blue_16.png"))); // NOI18N
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
                .addContainerGap(270, Short.MAX_VALUE))
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

        jButtonProgPfad.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N
        jButtonProgPfad.setToolTipText("Pfad auswählen");

        jLabel1.setText("Schalter:");

        jLabel2.setText("Name:");

        jLabel3.setText("Präfix ( zB. http ):");

        jLabel4.setText("Suffix ( zB. mp4):");

        jCheckBoxRestart.setText("fehlgeschlagene Downloads wieder Starten");

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
                                .addComponent(jTextFieldProgSuffix, javax.swing.GroupLayout.DEFAULT_SIZE, 187, Short.MAX_VALUE))))
                    .addGroup(jPanelProgrammDetailsLayout.createSequentialGroup()
                        .addGroup(jPanelProgrammDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel2)
                            .addComponent(jLabel9))
                        .addGap(27, 27, 27)
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
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 105, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanelProgrammDetails, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        jTabbedPane.addTab("Programme", jPanelProgramme);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jButtonPruefen, javax.swing.GroupLayout.PREFERRED_SIZE, 84, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonHilfe))
                    .addComponent(jTabbedPane, javax.swing.GroupLayout.DEFAULT_SIZE, 659, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTabbedPane)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonHilfe)
                    .addComponent(jButtonPruefen))
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
    private javax.swing.JCheckBox jCheckBoxLaenge;
    private javax.swing.JCheckBox jCheckBoxRestart;
    private javax.swing.JCheckBox jCheckBoxSpeichern;
    private javax.swing.JLabel jLabel;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JPanel jPanelDetails;
    private javax.swing.JPanel jPanelPgruppe;
    private javax.swing.JPanel jPanelProgrammDetails;
    private javax.swing.JPanel jPanelProgramme;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSpinner jSpinnerLaenge;
    private javax.swing.JTabbedPane jTabbedPane;
    private javax.swing.JTable jTablePgruppen;
    private javax.swing.JTable jTableProgramme;
    private javax.swing.JTextField jTextFieldGruppeDirektPraefix;
    private javax.swing.JTextField jTextFieldGruppeDirektSuffix;
    private javax.swing.JTextField jTextFieldGruppeName;
    private javax.swing.JTextField jTextFieldGruppeZielName;
    private javax.swing.JTextField jTextFieldGruppeZielPfad;
    private javax.swing.JTextField jTextFieldProgName;
    private javax.swing.JTextField jTextFieldProgPfad;
    private javax.swing.JTextField jTextFieldProgPraefix;
    private javax.swing.JTextField jTextFieldProgSchalter;
    private javax.swing.JTextField jTextFieldProgSuffix;
    private javax.swing.JTextField jTextFieldProgZielDateiName;
    // End of variables declaration//GEN-END:variables

    private class BeobProgRestart implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            if (!stopBeob) {
                Daten.setGeaendert();
                int rows = jTableProgramme.getSelectedRow();
                if (rows != -1) {
                    int row = jTableProgramme.convertRowIndexToModel(rows);
                    DatenProg prog = getPgruppe().getListeProg().get(row);
                    prog.arr[DatenProg.PROGRAMM_RESTART_NR] = Boolean.toString(jCheckBoxRestart.isSelected());
                    jTableProgramme.getModel().setValueAt(Boolean.toString(jCheckBoxRestart.isSelected()), row, DatenProg.PROGRAMM_RESTART_NR);
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
                Daten.setGeaendert();
                int rows = jTableProgramme.getSelectedRow();
                if (rows != -1) {
                    int row = jTableProgramme.convertRowIndexToModel(rows);
                    DatenProg prog = getPgruppe().getListeProg().get(row);
                    prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD_NR] = jTextFieldProgPfad.getText();
                    prog.arr[DatenProg.PROGRAMM_SCHALTER_NR] = jTextFieldProgSchalter.getText();
                    prog.arr[DatenProg.PROGRAMM_NAME_NR] = jTextFieldProgName.getText();
                    prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR] = jTextFieldProgZielDateiName.getText();
                    prog.arr[DatenProg.PROGRAMM_SUFFIX_NR] = jTextFieldProgSuffix.getText();
                    prog.arr[DatenProg.PROGRAMM_PRAEFIX_NR] = jTextFieldProgPraefix.getText();
                    jTableProgramme.getModel().setValueAt(jTextFieldProgPfad.getText(), row, DatenProg.PROGRAMM_PROGRAMMPFAD_NR);
                    jTableProgramme.getModel().setValueAt(jTextFieldProgSchalter.getText(), row, DatenProg.PROGRAMM_SCHALTER_NR);
                    jTableProgramme.getModel().setValueAt(jTextFieldProgName.getText(), row, DatenProg.PROGRAMM_NAME_NR);
                    jTableProgramme.getModel().setValueAt(jTextFieldProgZielDateiName.getText(), row, DatenProg.PROGRAMM_ZIEL_DATEINAME_NR);
                    jTableProgramme.getModel().setValueAt(jTextFieldProgSuffix.getText(), row, DatenProg.PROGRAMM_SUFFIX_NR);
                    jTableProgramme.getModel().setValueAt(jTextFieldProgPraefix.getText(), row, DatenProg.PROGRAMM_PRAEFIX_NR);
//                    progNamePruefen();
                }
            }
        }
    }

    private class BeobTableSelectPgruppe implements ListSelectionListener {

        @Override
        public void valueChanged(ListSelectionEvent event) {
            if (!stopBeob) {
                if (!event.getValueIsAdjusting()) {
                    tabelleProgramme();
                }
            }
        }
    }

    private class BeobDateiDialogProg implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
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
                    Log.fehlerMeldung("PanelPgruppen.BeobDateiDialogProg", ex);
                }
            }
        }
    }

    private class BeobDateiDialogPfad implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            int returnVal;
            JFileChooser chooser = new JFileChooser();
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            if (!jTextFieldGruppeZielPfad.getText().equals("")) {
                chooser.setCurrentDirectory(new File(jTextFieldGruppeZielPfad.getText()));
            }
            returnVal = chooser.showOpenDialog(null);
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                try {
                    String str = chooser.getSelectedFile().getPath();
                    jTextFieldGruppeZielPfad.setText(str);
                } catch (Exception ex) {
                    Log.fehlerMeldung("PanelPgruppen.BeobDateiDialogPfad", ex);
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
            int rows = jTableProgramme.getSelectedRow();
            if (rows != -1) {
                int row = jTableProgramme.convertRowIndexToModel(rows);
                DatenProg prog = getPgruppe().getListeProg().get(row);
                progNeueZeile(prog.copy());
            } else {
                new HinweisKeineAuswahl().zeigen();
            }
        }
    }

    private class BeobGruppenDoc implements DocumentListener {

        JTextField textfeld;
        int nr;

        public BeobGruppenDoc(JTextField ttextfeld, int nnr) {
            textfeld = ttextfeld;
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
                DatenPgruppe gruppe;
                int row = jTablePgruppen.getSelectedRow();
                if (row != -1) {
                    gruppe = ddaten.listePgruppe.get(jTablePgruppen.convertRowIndexToModel(row));
                    stopBeob = true;
                    gruppe.arr[nr] = textfeld.getText();
                    if (nr == DatenPgruppe.PROGRAMMGRUPPE_NAME_NR) {
                        jTablePgruppen.getModel().setValueAt(jTextFieldGruppeName.getText(), row, DatenPgruppe.PROGRAMMGRUPPE_NAME_NR);
                        jTabbedPane.setTitleAt(0, gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR]);
                    }
                    notifyPgruppe();
                    Daten.setGeaendert();
                    stopBeob = false;
                } else {
                    new HinweisKeineAuswahl().zeigen();
                }
            }
            gruppenNamePruefen();
        }
    }

    private class BeobGruppeDuplizieren implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            DatenPgruppe gruppe = null;
            int row = jTablePgruppen.getSelectedRow();
            if (row != -1) {
                gruppe = ddaten.listePgruppe.get(jTablePgruppen.convertRowIndexToModel(row));
                ddaten.listePgruppe.duplicate(gruppe);
                tabellePgruppe();
                Daten.setGeaendert();
            } else {
                new HinweisKeineAuswahl().zeigen();
            }
        }
    }

    private class BeobHilfeTabelle implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            dialogHilfe();
        }
    }

    private class BeobProgLoeschen implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            int rows[] = jTableProgramme.getSelectedRows();
            if (rows.length > 0) {
                DatenPgruppe pGruppe = getPgruppe();
                String text;
                if (rows.length == 1) {
                    int delRow = jTableProgramme.convertRowIndexToModel(rows[0]);
                    text = pGruppe.getProg(delRow).arr[DatenProg.PROGRAMM_NAME_NR];
                } else {
                    text = rows.length + " Programme löschen?";
                }
                int ret = JOptionPane.showConfirmDialog(null, text, "Löschen?", JOptionPane.YES_NO_OPTION);
                if (ret == JOptionPane.OK_OPTION) {
                    for (int i = rows.length - 1; i >= 0; --i) {
                        int delRow = jTableProgramme.convertRowIndexToModel(rows[i]);
                        pGruppe.getListeProg().remove(delRow);
                    }
                    tabelleProgramme();
                    Daten.setGeaendert();
                }
            } else {
                new HinweisKeineAuswahl().zeigen();
            }
        }
    }

    private class BeobPuefen implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            GuiFunktionenProgramme.programmePruefen(ddaten);
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
            gruppeAufAb(auf);
        }
    }

    private class BeobGruppeNeu implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            gruppeNeu();
        }
    }

    private class BeobGruppeLoeschen implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            gruppeLoeschen();
        }
    }

    private class BeobGruppeExport implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            gruppeExport();
        }
    }

    private class BeobachterFarbe implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            DatenPgruppe pgruppe = getPgruppe();
            if (pgruppe != null) {
                DialogFarbe dialog = new DialogFarbe(null, true);
                dialog.setVisible(true);
                if (dialog.farbe != null) {
                    pgruppe.setFarbe(dialog.farbe);
                    tabellePgruppe();
                    Daten.setGeaendert();
                    notifyPgruppe();
                }
            }

        }
    }

    private class BeobStandardfarbe implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            DatenPgruppe pgruppe = getPgruppe();
            if (pgruppe != null) {
                pgruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_FARBE_NR] = "";
                tabellePgruppe();
                Daten.setGeaendert();
                notifyPgruppe();
            }

        }
    }

//    private class ComboBoxRenderer extends JLabel implements ListCellRenderer {
//
//        @Override
//        public Component getListCellRendererComponent(
//                JList list, // the list
//                Object value, // value to display
//                int index, // cell index
//                boolean isSelected, // is the cell selected
//                boolean cellHasFocus) // does the cell have focus
//        {
//            if (value.toString().equals("")) {
//                //für Leerzeichen
//                this.setMinimumSize(new Dimension(19, 19));
//                this.setPreferredSize(new Dimension(19, 19));
//            }
//            setText(value.toString());
//            setBackground(null);
//            setBorder(null);
//            Color col = null;
//            if (col != null) {
//                setBorder(javax.swing.BorderFactory.createLineBorder(col, 2));
//            }
//            setEnabled(list.isEnabled());
//            setFont(list.getFont());
//            setOpaque(true);
//            return this;
//        }
//    }
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
