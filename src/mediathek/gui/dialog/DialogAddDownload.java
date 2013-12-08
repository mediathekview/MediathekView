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

import com.jidesoft.utils.SystemInfo;
import java.awt.Color;
import java.awt.Component;
import java.awt.FileDialog;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.JTextComponent;
import mediathek.controller.starter.Start;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenPset;
import mediathek.res.GetIcon;
import mediathek.tool.EscBeenden;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.GuiFunktionenProgramme;
import mediathek.tool.GuiKonstanten;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;
import mediathek.tool.MVMessageDialog;
import msearch.daten.DatenFilm;

public class DialogAddDownload extends javax.swing.JDialog {

    private DatenPset pSet = null;
    private boolean ok = false;
    private DatenDownload datenDownload = null;
    private final Daten daten;
    private final DatenFilm datenFilm;
    private Component parentComponent = null;
    private String orgPfad = "";

    public DialogAddDownload(java.awt.Frame parent, Daten dd, DatenFilm film, DatenPset ppSet) {
        super(parent, true);
        parentComponent = parent;
        initComponents();
        daten = dd;
        datenFilm = film;
        pSet = ppSet;
        this.setTitle("Film Speichern");
        // Felder init
        init();
        if (parent != null) {
            setLocationRelativeTo(parent);
        }
    }

    private void init() {
        jCheckBoxStarten.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN_NR]));
        jCheckBoxStarten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.system[Konstanten.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN_NR] = String.valueOf(jCheckBoxStarten.isSelected());
            }
        });
        jButtonZiel.setIcon(GetIcon.getIcon("fileopen_16.png"));
        if (daten.listePset.getListeSpeichern().size() == 0) {
            // Satz mit x, war wohl nix
            ok = false;
            beenden();
        }
        jButtonZiel.addActionListener(new ZielBeobachter());
        jButtonOk.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (check()) {
                    beenden();
                }
            }
        });
        getRootPane().setDefaultButton(jButtonOk); //TH
        new EscBeenden(this) {
            @Override
            public void beenden_() {
                ok = false;
                beenden();
            }
        };
        jButtonAbbrechen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ok = false;
                beenden();
            }
        });
        jComboBoxPgr.setModel(new javax.swing.DefaultComboBoxModel<>(daten.listePset.getListeSpeichern().getObjectDataCombo()));
        if (pSet != null) {
            jComboBoxPgr.setSelectedItem(pSet.arr[DatenPset.PROGRAMMSET_NAME_NR]);
        } else {
            pSet = daten.listePset.getListeSpeichern().get(jComboBoxPgr.getSelectedIndex());
        }
        if (daten.listePset.getListeSpeichern().size() == 1) {
            // macht dann keinen Sinn
            jComboBoxPgr.setEnabled(false);
        } else {
            jComboBoxPgr.addActionListener(new BeobComboProgramm());
        }
        jTextFieldSender.setText(datenFilm.arr[DatenFilm.FILM_SENDER_NR]);
        jTextFieldTitel.setText(datenFilm.arr[DatenFilm.FILM_TITEL_NR]);
        jTextFieldName.getDocument().addDocumentListener(new BeobCheckNamen());
        ((JTextComponent) jComboBoxPfad.getEditor().getEditorComponent()).getDocument().addDocumentListener(new BeobCheckNamen());
        jRadioButtonAufloesungHd.addActionListener(new BeobRadio());
        jRadioButtonAufloesungKlein.addActionListener(new BeobRadio());
        jRadioButtonAufloesungHoch.addActionListener(new BeobRadio());
        jRadioButtonAufloesungHd.setEnabled(!datenFilm.arr[DatenFilm.FILM_URL_HD_NR].isEmpty());
        jRadioButtonAufloesungKlein.setEnabled(!datenFilm.arr[DatenFilm.FILM_URL_KLEIN_NR].isEmpty());
        jRadioButtonAufloesungHoch.setSelected(true);
        String mb;
        if (jRadioButtonAufloesungHd.isEnabled()) {
            mb = datenFilm.getDateigroesse(datenFilm.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_HD));
            if (!mb.isEmpty()) {
                jRadioButtonAufloesungHd.setText(jRadioButtonAufloesungHd.getText() + "   [ " + mb + " MB ]");
            }
        }
        mb = datenFilm.getDateigroesse(datenFilm.arr[DatenFilm.FILM_URL_NR]);
        if (!mb.isEmpty()) {
            jRadioButtonAufloesungHoch.setText(jRadioButtonAufloesungHoch.getText() + "   [ " + mb + " MB ]");
        }
        if (jRadioButtonAufloesungKlein.isEnabled()) {
            mb = datenFilm.getDateigroesse(datenFilm.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_KLEIN));
            if (!mb.isEmpty()) {
                jRadioButtonAufloesungKlein.setText(jRadioButtonAufloesungKlein.getText() + "   [ " + mb + " MB ]");
            }
        }
        jButtonDelHistory.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.system[Konstanten.SYSTEM_PFADE_SPEICHERN_NR] = "";
                jComboBoxPfad.setModel(new DefaultComboBoxModel<>(new String[]{orgPfad}));
            }
        });
        jCheckBoxPfadSpeichern.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_DIALOG_DOWNLOAD_PFAD_SPEICHERN_NR]));
        jCheckBoxPfadSpeichern.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.system[Konstanten.SYSTEM_DIALOG_DOWNLOAD_PFAD_SPEICHERN_NR] = Boolean.toString(jCheckBoxPfadSpeichern.isSelected());
            }
        });

        setCombo();
    }

    private void setNameFilm() {
        // beim ersten mal werden die Standardpfade gesucht
        // datenDownload = new DatenDownload(pSet, datenFilm, Start.QUELLE_DOWNLOAD, null, "", "", "" /*Aufloesung*/);
        datenDownload = new DatenDownload(pSet, datenFilm, Start.QUELLE_DOWNLOAD, null, "", "", getAufloesung());
        if (datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME_NR].equals("")) {
            jTextFieldName.setEnabled(false);
            jComboBoxPfad.setEnabled(false);
            jButtonZiel.setEnabled(false);
            jTextFieldName.setText("");
            setModelPfad("");
        } else {
            jTextFieldName.setEnabled(true);
            jComboBoxPfad.setEnabled(true);
            jButtonZiel.setEnabled(true);
            jTextFieldName.setText(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME_NR]);
            setModelPfad(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR]);
            orgPfad = datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR];
        }
    }

    private void setModelPfad(String pfad) {
        ArrayList<String> pfade = new ArrayList<>();
        // wenn gewünscht, den letzten verwendeten Pfad an den Anfang setzen
        if (Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_DIALOG_DOWNLOAD_PFAD_SPEICHERN_NR])) {
            if (!Daten.system[Konstanten.SYSTEM_LETZTER_PFAD_SPEICHERN_NR].isEmpty()) {
                pfade.add(Daten.system[Konstanten.SYSTEM_LETZTER_PFAD_SPEICHERN_NR]);
            }
        }
        // dann den Rest
        pfade.add(pfad);
        if (!Daten.system[Konstanten.SYSTEM_PFADE_SPEICHERN_NR].isEmpty()) {
            String[] p = Daten.system[Konstanten.SYSTEM_PFADE_SPEICHERN_NR].split("<>");
            if (p.length != 0) {
                pfade.addAll(Arrays.asList(p));
            }
        }
        jComboBoxPfad.setModel(new DefaultComboBoxModel<>(pfade.toArray(new String[]{})));
    }

    private void saveComboPfad() {
        String akt = jComboBoxPfad.getSelectedItem().toString();
        Daten.system[Konstanten.SYSTEM_LETZTER_PFAD_SPEICHERN_NR] = akt;

        ArrayList<String> pfade = new ArrayList<>();
        if (!Daten.system[Konstanten.SYSTEM_PFADE_SPEICHERN_NR].isEmpty()) {
            String[] p = Daten.system[Konstanten.SYSTEM_PFADE_SPEICHERN_NR].split("<>");
            if (p.length != 0) {
                pfade.addAll(Arrays.asList(p));
            }
        }
        if (!pfade.contains(akt) && !akt.equals(orgPfad)) {
            pfade.add(0, akt);
        }
        Daten.system[Konstanten.SYSTEM_PFADE_SPEICHERN_NR] = "";
        if (pfade.size() > 0) {
            Daten.system[Konstanten.SYSTEM_PFADE_SPEICHERN_NR] = pfade.get(0);
            for (int i = 1; i < 5 && i < pfade.size(); ++i) {
                if (!pfade.get(i).isEmpty() && !pfade.get(i).equals(orgPfad)) {
                    Daten.system[Konstanten.SYSTEM_PFADE_SPEICHERN_NR] += "<>" + pfade.get(i);
                }
            }
        }
    }

    private void setCombo() {
        // stellt den Namen/Radios passend zum Combo ein
        pSet = daten.listePset.getListeSpeichern().get(jComboBoxPgr.getSelectedIndex());
        if (!datenFilm.arr[DatenFilm.FILM_URL_HD_NR].isEmpty() && pSet.arr[DatenPset.PROGRAMMSET_AUFLOESUNG_NR].equals(DatenFilm.AUFLOESUNG_HD)) {
            jRadioButtonAufloesungHd.setSelected(true);
        } else if (!datenFilm.arr[DatenFilm.FILM_URL_KLEIN_NR].isEmpty() && pSet.arr[DatenPset.PROGRAMMSET_AUFLOESUNG_NR].equals(DatenFilm.AUFLOESUNG_KLEIN)) {
            jRadioButtonAufloesungKlein.setSelected(true);
        } else {
            jRadioButtonAufloesungHoch.setSelected(true);
        }
        jCheckBoxInfodatei.setSelected(Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_INFODATEI_NR]));
        setNameFilm();
    }

    private String getAufloesung() {
        if (jRadioButtonAufloesungHd.isSelected()) {
            return DatenFilm.AUFLOESUNG_HD;
        } else if (jRadioButtonAufloesungKlein.isSelected()) {
            return DatenFilm.AUFLOESUNG_KLEIN;
        } else {
            return DatenFilm.AUFLOESUNG_NORMAL;
        }
    }

    private boolean check() {
        ok = false;
        String pfad = jComboBoxPfad.getSelectedItem().toString();
        String name = jTextFieldName.getText();
        if (datenDownload != null) {
            if (pfad.equals("") || name.equals("")) {
                MVMessageDialog.showMessageDialog(parentComponent, "Pfad oder Name ist leer", "Fehlerhafter Pfad/Name!", JOptionPane.ERROR_MESSAGE);
            } else {
                if (!pfad.substring(pfad.length() - 1).equals(File.separator)) {
                    pfad += File.separator;
                }
                if (GuiFunktionenProgramme.checkPfadBeschreibbar(pfad)) {
                    ok = true;
                } else {
                    MVMessageDialog.showMessageDialog(parentComponent, "Pfad ist nicht beschreibbar", "Fehlerhafter Pfad!", JOptionPane.ERROR_MESSAGE);
                }
            }
        }
        return ok;
    }

    private void beenden() {
        if (ok) {
            // jetzt wird mit den angegebenen Pfaden gearbeitet
            datenDownload = new DatenDownload(pSet, datenFilm, Start.QUELLE_DOWNLOAD, null, jTextFieldName.getText(), jComboBoxPfad.getSelectedItem().toString(), getAufloesung());
            datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI_NR] = Boolean.toString(jCheckBoxInfodatei.isSelected());
            Daten.listeDownloads.addMitNummer(datenDownload);
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
            if (jCheckBoxStarten.isSelected()) {
                // und evtl. auch gleich starten
                datenDownload.startenDownload(daten);
            }
        }
        saveComboPfad();
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
        jButtonOk = new javax.swing.JButton();
        jButtonAbbrechen = new javax.swing.JButton();
        jTabbedPane1 = new javax.swing.JTabbedPane();
        jPanel6 = new javax.swing.JPanel();
        javax.swing.JPanel jPanel3 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel2 = new javax.swing.JLabel();
        javax.swing.JLabel jLabel3 = new javax.swing.JLabel();
        jTextFieldTitel = new javax.swing.JTextField();
        jTextFieldSender = new javax.swing.JTextField();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        jTextFieldName = new javax.swing.JTextField();
        jButtonZiel = new javax.swing.JButton();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        javax.swing.JLabel jLabel4 = new javax.swing.JLabel();
        jComboBoxPfad = new javax.swing.JComboBox<String>();
        jButtonDelHistory = new javax.swing.JButton();
        jPanel7 = new javax.swing.JPanel();
        jPanel4 = new javax.swing.JPanel();
        jCheckBoxInfodatei = new javax.swing.JCheckBox();
        jPanel8 = new javax.swing.JPanel();
        jCheckBoxPfadSpeichern = new javax.swing.JCheckBox();
        javax.swing.JPanel jPanel5 = new javax.swing.JPanel();
        jComboBoxPgr = new javax.swing.JComboBox<String>();
        jPanel2 = new javax.swing.JPanel();
        jRadioButtonAufloesungHd = new javax.swing.JRadioButton();
        jRadioButtonAufloesungHoch = new javax.swing.JRadioButton();
        jRadioButtonAufloesungKlein = new javax.swing.JRadioButton();
        jCheckBoxStarten = new javax.swing.JCheckBox();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        jButtonOk.setText("Ok");

        jButtonAbbrechen.setText("Abbrechen");

        jPanel3.setBorder(javax.swing.BorderFactory.createTitledBorder("Film"));

        jLabel2.setText("Sender:");

        jLabel3.setText("Titel:");

        jTextFieldTitel.setEditable(false);
        jTextFieldTitel.setText("jTextField2");

        jTextFieldSender.setEditable(false);
        jTextFieldSender.setText("jTextField1");

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel2)
                    .addComponent(jLabel3))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jTextFieldSender, javax.swing.GroupLayout.DEFAULT_SIZE, 403, Short.MAX_VALUE)
                    .addComponent(jTextFieldTitel))
                .addContainerGap())
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel2)
                    .addComponent(jTextFieldSender, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel3)
                    .addComponent(jTextFieldTitel, javax.swing.GroupLayout.PREFERRED_SIZE, 25, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(18, Short.MAX_VALUE))
        );

        jPanel3Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jTextFieldSender, jTextFieldTitel});

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("Speicherort"));

        jButtonZiel.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N

        jLabel1.setText("Zielpfad:");

        jLabel4.setText("Dateiname:");

        jComboBoxPfad.setEditable(true);
        jComboBoxPfad.setModel(new javax.swing.DefaultComboBoxModel(new String[] { " " }));

        jButtonDelHistory.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_16.png"))); // NOI18N
        jButtonDelHistory.setToolTipText("History löschen");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addGap(30, 30, 30)
                        .addComponent(jComboBoxPfad, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonZiel)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonDelHistory))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jLabel4)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldName)))
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel1)
                    .addComponent(jButtonZiel)
                    .addComponent(jComboBoxPfad, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonDelHistory))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel4)
                    .addComponent(jTextFieldName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(13, Short.MAX_VALUE))
        );

        jPanel1Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonZiel, jTextFieldName});

        javax.swing.GroupLayout jPanel6Layout = new javax.swing.GroupLayout(jPanel6);
        jPanel6.setLayout(jPanel6Layout);
        jPanel6Layout.setHorizontalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel6Layout.setVerticalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(156, Short.MAX_VALUE))
        );

        jTabbedPane1.addTab("Speicherziel", jPanel6);

        jPanel4.setBorder(javax.swing.BorderFactory.createTitledBorder("Infodatei"));

        jCheckBoxInfodatei.setText("Infodatei anlegen: \"Filmname.txt\"");

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jCheckBoxInfodatei)
                .addContainerGap(224, Short.MAX_VALUE))
        );
        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jCheckBoxInfodatei)
                .addContainerGap(11, Short.MAX_VALUE))
        );

        jPanel8.setBorder(javax.swing.BorderFactory.createTitledBorder("Zielpfad speichern"));

        jCheckBoxPfadSpeichern.setText("Zielpfad speichern und als Vorgabe verwenden");

        javax.swing.GroupLayout jPanel8Layout = new javax.swing.GroupLayout(jPanel8);
        jPanel8.setLayout(jPanel8Layout);
        jPanel8Layout.setHorizontalGroup(
            jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel8Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jCheckBoxPfadSpeichern)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel8Layout.setVerticalGroup(
            jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel8Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jCheckBoxPfadSpeichern)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel5.setBorder(javax.swing.BorderFactory.createTitledBorder("Programmset zum Aufzeichnen"));

        javax.swing.GroupLayout jPanel5Layout = new javax.swing.GroupLayout(jPanel5);
        jPanel5.setLayout(jPanel5Layout);
        jPanel5Layout.setHorizontalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jComboBoxPgr, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        jPanel5Layout.setVerticalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jComboBoxPgr, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(23, Short.MAX_VALUE))
        );

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder("Auflösung"));

        buttonGroup1.add(jRadioButtonAufloesungHd);
        jRadioButtonAufloesungHd.setText("Film in HD laden");

        buttonGroup1.add(jRadioButtonAufloesungHoch);
        jRadioButtonAufloesungHoch.setText("Film in hoher Auflösung laden");

        buttonGroup1.add(jRadioButtonAufloesungKlein);
        jRadioButtonAufloesungKlein.setText("Film in niedriger Auflösung laden");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jRadioButtonAufloesungHd)
                    .addComponent(jRadioButtonAufloesungHoch)
                    .addComponent(jRadioButtonAufloesungKlein))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jRadioButtonAufloesungHd)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jRadioButtonAufloesungHoch)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jRadioButtonAufloesungKlein)
                .addContainerGap(14, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout jPanel7Layout = new javax.swing.GroupLayout(jPanel7);
        jPanel7.setLayout(jPanel7Layout);
        jPanel7Layout.setHorizontalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel8, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel5, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel7Layout.setVerticalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel5, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jPanel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jPanel8, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jTabbedPane1.addTab("Erweitert", jPanel7);

        jCheckBoxStarten.setSelected(true);
        jCheckBoxStarten.setText("Download sofort starten");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jTabbedPane1)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jCheckBoxStarten)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jButtonOk, javax.swing.GroupLayout.PREFERRED_SIZE, 93, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jButtonAbbrechen)))
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonAbbrechen, jButtonOk});

        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTabbedPane1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jButtonOk)
                    .addComponent(jButtonAbbrechen)
                    .addComponent(jCheckBoxStarten))
                .addContainerGap())
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JButton jButtonAbbrechen;
    private javax.swing.JButton jButtonDelHistory;
    private javax.swing.JButton jButtonOk;
    private javax.swing.JButton jButtonZiel;
    private javax.swing.JCheckBox jCheckBoxInfodatei;
    private javax.swing.JCheckBox jCheckBoxPfadSpeichern;
    private javax.swing.JCheckBox jCheckBoxStarten;
    private javax.swing.JComboBox<String> jComboBoxPfad;
    private javax.swing.JComboBox<String> jComboBoxPgr;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JPanel jPanel8;
    private javax.swing.JRadioButton jRadioButtonAufloesungHd;
    private javax.swing.JRadioButton jRadioButtonAufloesungHoch;
    private javax.swing.JRadioButton jRadioButtonAufloesungKlein;
    private javax.swing.JTabbedPane jTabbedPane1;
    private javax.swing.JTextField jTextFieldName;
    private javax.swing.JTextField jTextFieldSender;
    private javax.swing.JTextField jTextFieldTitel;
    // End of variables declaration//GEN-END:variables

    private class BeobCheckNamen implements DocumentListener {

        @Override
        public void insertUpdate(DocumentEvent e) {
            checkPfadName();
        }

        @Override
        public void removeUpdate(DocumentEvent e) {
            checkPfadName();
        }

        @Override
        public void changedUpdate(DocumentEvent e) {
            checkPfadName();
        }

        private void checkPfadName() {
            if (!jTextFieldName.getText().equals(GuiFunktionen.replaceLeerDateiname(jTextFieldName.getText(), true/* istDatei */, true /* leerEntfernen */))) {
                jTextFieldName.setBackground(GuiKonstanten.DOWNLOAD_FARBE_ERR);
            } else {
                jTextFieldName.setBackground(Color.WHITE);
            }
            if (!jComboBoxPfad.getSelectedItem().toString().equals(GuiFunktionen.replaceLeerDateiname(jComboBoxPfad.getSelectedItem().toString(), false/* istDatei */, true /* leerEntfernen */))) {
                jComboBoxPfad.setBackground(GuiKonstanten.DOWNLOAD_FARBE_ERR);
            } else {
                jComboBoxPfad.setBackground(Color.WHITE);
            }
        }
    }

    private class BeobComboProgramm implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            setCombo();
        }
    }

    private class BeobRadio implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            setNameFilm();
        }
    }

    private class ZielBeobachter implements ActionListener {

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
                        jComboBoxPfad.addItem(chooser.getDirectory() + chooser.getFile());
                        jComboBoxPfad.setSelectedItem(chooser.getDirectory() + chooser.getFile());
                    } catch (Exception ex) {
                        Log.fehlerMeldung(356871087, Log.FEHLER_ART_PROG, "DialogAddDownload.ZielBeobachter", ex);
                    }
                }
                System.setProperty("apple.awt.fileDialogForDirectories", "false");
            } else {
                //use the cross-platform swing chooser
                int returnVal;
                JFileChooser chooser = new JFileChooser();
                if (!jComboBoxPfad.getSelectedItem().toString().equals("")) {
                    chooser.setCurrentDirectory(new File(jComboBoxPfad.getSelectedItem().toString()));
                }
                chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                returnVal = chooser.showOpenDialog(null);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        jComboBoxPfad.addItem(chooser.getSelectedFile().getAbsolutePath());
                        jComboBoxPfad.setSelectedItem(chooser.getSelectedFile().getAbsolutePath());
                    } catch (Exception ex) {
                        Log.fehlerMeldung(356871087, Log.FEHLER_ART_PROG, "DialogAddDownload.ZielBeobachter", ex);
                    }
                }
            }
        }
    }
}
