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
import java.awt.FileDialog;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.JTextComponent;
import mediathek.controller.Log;
import mediathek.controller.starter.Start;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenPset;
import mediathek.res.GetIcon;
import mediathek.tool.EscBeenden;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.GuiFunktionenProgramme;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVColor;
import mediathek.tool.MVConfig;
import mediathek.tool.MVMessageDialog;
import msearch.daten.DatenFilm;

public class DialogAddDownloadSmall extends JDialog {
    
    private DatenPset pSet = null;
    private boolean ok = false;
    private DatenDownload datenDownload = null;
    private final Daten daten;
    private final DatenFilm datenFilm;
    private String orgPfad = "";
    private String aufloesung = "";
    private String dateiGroesse_HD = "";
    private String dateiGroesse_Hoch = "";
    private String dateiGroesse_Klein = "";
    private boolean nameGeaendert = false;
    private boolean stopBeob = false;
    
    public DialogAddDownloadSmall(Frame parent, Daten daten, DatenFilm film, DatenPset pSet, String aufloesung) {
        super(parent, true);
        this.aufloesung = aufloesung;
        initComponents();
        this.daten = daten;
        datenFilm = film;
        this.pSet = pSet;

        // Felder init
        init();
        if (parent != null) {
            setLocationRelativeTo(parent);
        }
    }
    
    private void init() {
        jComboBoxPset.setModel(new DefaultComboBoxModel<>(daten.listePset.getListeSpeichern().getObjectDataCombo()));
        
        jCheckBoxStarten.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN)));
        jCheckBoxStarten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN, String.valueOf(jCheckBoxStarten.isSelected()));
            }
        });
        jButtonZiel.setIcon(GetIcon.getProgramIcon("fileopen_16.png"));
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
        
        if (pSet != null) {
            jComboBoxPset.setSelectedItem(pSet.arr[DatenPset.PROGRAMMSET_NAME_NR]);
        } else {
            pSet = daten.listePset.getListeSpeichern().get(jComboBoxPset.getSelectedIndex());
        }
        if (daten.listePset.getListeSpeichern().size() == 1) {
            // macht dann keinen Sinn
            jPanelSet.setVisible(false);
            jComboBoxPset.setEnabled(false);
        } else {
            jComboBoxPset.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    setupResolutionButtons();
                }
            });
        }
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
                    if (!jTextFieldName.getText().equals(GuiFunktionen.checkDateiname(jTextFieldName.getText(), false /*pfad*/))) {
                        jTextFieldName.setBackground(MVColor.DOWNLOAD_FEHLER.color);
                    } else {
                        jTextFieldName.setBackground(javax.swing.UIManager.getDefaults().getColor("TextField.background"));
                    }
                }
                
            }
        });
        ((JTextComponent) jComboBoxPfad.getEditor().getEditorComponent()).setOpaque(true);
        ((JTextComponent) jComboBoxPfad.getEditor().getEditorComponent()).getDocument().addDocumentListener(new DocumentListener() {
            
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
                    String s = ((JTextComponent) jComboBoxPfad.getEditor().getEditorComponent()).getText();
                    if (!s.equals(GuiFunktionen.checkDateiname(s, true /*pfad*/))) {
                        jComboBoxPfad.getEditor().getEditorComponent().setBackground(MVColor.DOWNLOAD_FEHLER.color);
                    } else {
                        jComboBoxPfad.getEditor().getEditorComponent().setBackground(Color.WHITE);
                    }
                }
                
            }
        });
        jRadioButtonAufloesungHd.addActionListener(new BeobRadio());
        jRadioButtonAufloesungKlein.addActionListener(new BeobRadio());
        jRadioButtonAufloesungHoch.addActionListener(new BeobRadio());
        jRadioButtonAufloesungHd.setEnabled(!datenFilm.arr[DatenFilm.FILM_URL_HD_NR].isEmpty());
        jRadioButtonAufloesungKlein.setEnabled(!datenFilm.arr[DatenFilm.FILM_URL_KLEIN_NR].isEmpty());
        jRadioButtonAufloesungHoch.setSelected(true);
        if (jRadioButtonAufloesungHd.isEnabled()) {
            dateiGroesse_HD = datenFilm.getDateigroesse(datenFilm.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_HD));
            if (!dateiGroesse_HD.isEmpty()) {
                jRadioButtonAufloesungHd.setText(jRadioButtonAufloesungHd.getText() + "   [ " + dateiGroesse_HD + " MB ]");
            }
        }
        dateiGroesse_Hoch = datenFilm.getDateigroesse(datenFilm.arr[DatenFilm.FILM_URL_NR]);
        if (!dateiGroesse_Hoch.isEmpty()) {
            jRadioButtonAufloesungHoch.setText(jRadioButtonAufloesungHoch.getText() + "   [ " + dateiGroesse_Hoch + " MB ]");
        }
        if (jRadioButtonAufloesungKlein.isEnabled()) {
            dateiGroesse_Klein = datenFilm.getDateigroesse(datenFilm.getUrlFuerAufloesung(DatenFilm.AUFLOESUNG_KLEIN));
            if (!dateiGroesse_Klein.isEmpty()) {
                jRadioButtonAufloesungKlein.setText(jRadioButtonAufloesungKlein.getText() + "   [ " + dateiGroesse_Klein + " MB ]");
            }
        }
        jButtonDelHistory.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM__DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN, "");
                jComboBoxPfad.setModel(new DefaultComboBoxModel<>(new String[]{orgPfad}));
            }
        });
        jCheckBoxPfadSpeichern.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM__DIALOG_DOWNLOAD__LETZTEN_PFAD_ANZEIGEN)));
        jCheckBoxPfadSpeichern.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM__DIALOG_DOWNLOAD__LETZTEN_PFAD_ANZEIGEN, Boolean.toString(jCheckBoxPfadSpeichern.isSelected()));
            }
        });
        setupResolutionButtons();
        nameGeaendert = false;
    }
    
    private void setNameFilm() {
        // beim ersten mal werden die Standardpfade gesucht
        if (!nameGeaendert) {
            // nur wenn vom Benutzer noch nicht geänert!
            stopBeob = true;
            datenDownload = new DatenDownload(pSet, datenFilm, Start.QUELLE_DOWNLOAD, null, "", "", getFilmResolution());
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
            stopBeob = false;
        }
        int w = this.getWidth();
        pack();
        this.setSize(w, this.getHeight());
    }
    
    private void setModelPfad(String pfad) {
        ArrayList<String> pfade = new ArrayList<>();
        // wenn gewünscht, den letzten verwendeten Pfad an den Anfang setzen
        if (!Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM__DIALOG_DOWNLOAD__LETZTEN_PFAD_ANZEIGEN))) {
            // sonst kommt der Pfad des Sets an den Anfang
            if (!pfad.isEmpty()) {
                pfade.add(pfad);
            }
        }
        if (!Daten.mVConfig.get(MVConfig.SYSTEM__DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN).isEmpty()) {
            String[] p = Daten.mVConfig.get(MVConfig.SYSTEM__DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN).split("<>");
            if (p.length != 0) {
                pfade.addAll(Arrays.asList(p));
            }
        }
        if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM__DIALOG_DOWNLOAD__LETZTEN_PFAD_ANZEIGEN))) {
            if (!pfad.isEmpty()) {
                pfade.add(pfad);
            }
        }
        jComboBoxPfad.setModel(new DefaultComboBoxModel<>(pfade.toArray(new String[pfade.size()])));
    }
    
    private void saveComboPfad() {
        ArrayList<String> pfade = new ArrayList<>();
        String s = jComboBoxPfad.getSelectedItem().toString();
        if (!s.equals(orgPfad) || jCheckBoxPfadSpeichern.isSelected()) {
            pfade.add(s);
        }
        for (int i = 0; i < jComboBoxPfad.getItemCount(); ++i) {
            s = jComboBoxPfad.getItemAt(i);
            if (!s.equals(orgPfad) && !pfade.contains(s)) {
                pfade.add(s);
            }
        }
        if (pfade.size() > 0) {
            s = pfade.get(0);
            for (int i = 1; i < Konstanten.MAX_PFADE_DIALOG_DOWNLOAD && i < pfade.size(); ++i) {
                if (!pfade.get(i).isEmpty()) {
                    s += "<>" + pfade.get(i);
                }
            }
        }
        Daten.mVConfig.add(MVConfig.SYSTEM__DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN, s);
    }

    /**
     * Setup the resolution radio buttons based on available download URLs.
     */
    private void setupResolutionButtons() {
        pSet = daten.listePset.getListeSpeichern().get(jComboBoxPset.getSelectedIndex());
        if (aufloesung.equals(DatenFilm.AUFLOESUNG_HD) || pSet.arr[DatenPset.PROGRAMMSET_AUFLOESUNG_NR].equals(DatenFilm.AUFLOESUNG_HD)
                && !datenFilm.arr[DatenFilm.FILM_URL_HD_NR].isEmpty()) {
            /* Dann wurde im Filter HD ausgewählt und wird voreingestellt */
            jRadioButtonAufloesungHd.setSelected(true);
        } else if (pSet.arr[DatenPset.PROGRAMMSET_AUFLOESUNG_NR].equals(DatenFilm.AUFLOESUNG_KLEIN) && !datenFilm.arr[DatenFilm.FILM_URL_KLEIN_NR].isEmpty()) {
            jRadioButtonAufloesungKlein.setSelected(true);
        } else {
            jRadioButtonAufloesungHoch.setSelected(true);
        }
        jCheckBoxInfodatei.setSelected(Boolean.parseBoolean(pSet.arr[DatenPset.PROGRAMMSET_INFODATEI_NR]));
        setNameFilm();
    }

    /**
     * Return the resolution string based on selected {@link javax.swing.JRadioButton}.
     *
     * @return The resolution as a string.
     */
    private String getFilmResolution() {
        if (jRadioButtonAufloesungHd.isSelected()) {
            return DatenFilm.AUFLOESUNG_HD;
        } else if (jRadioButtonAufloesungKlein.isSelected()) {
            return DatenFilm.AUFLOESUNG_KLEIN;
        } else {
            return DatenFilm.AUFLOESUNG_NORMAL;
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
        ok = false;
        String pfad = jComboBoxPfad.getSelectedItem().toString();
        String name = jTextFieldName.getText();
        if (datenDownload != null) {
            if (pfad.equals("") || name.equals("")) {
                MVMessageDialog.showMessageDialog(this, "Pfad oder Name ist leer", "Fehlerhafter Pfad/Name!", JOptionPane.ERROR_MESSAGE);
            } else {
                if (!pfad.substring(pfad.length() - 1).equals(File.separator)) {
                    pfad += File.separator;
                }
                if (GuiFunktionenProgramme.checkPfadBeschreibbar(pfad)) {
                    ok = true;
                } else {
                    MVMessageDialog.showMessageDialog(this, "Pfad ist nicht beschreibbar", "Fehlerhafter Pfad!", JOptionPane.ERROR_MESSAGE);
                }
            }
        }
        return ok;
    }
    
    private void beenden() {
        if (ok) {
            // jetzt wird mit den angegebenen Pfaden gearbeitet
            datenDownload = new DatenDownload(pSet, datenFilm, Start.QUELLE_DOWNLOAD, null, jTextFieldName.getText(), jComboBoxPfad.getSelectedItem().toString(), getFilmResolution());
            datenDownload.setGroesse(getFilmSize());
            datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI_NR] = Boolean.toString(jCheckBoxInfodatei.isSelected());
            Daten.listeDownloads.addMitNummer(datenDownload);
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
            if (jCheckBoxStarten.isSelected()) {
                // und evtl. auch gleich starten
                datenDownload.startDownload(daten);
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
        jPanelSet = new javax.swing.JPanel();
        jComboBoxPset = new javax.swing.JComboBox<>();
        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        jRadioButtonAufloesungHd = new javax.swing.JRadioButton();
        jRadioButtonAufloesungHoch = new javax.swing.JRadioButton();
        jRadioButtonAufloesungKlein = new javax.swing.JRadioButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Film speichern");

        jButtonOk.setText("Ok");

        jButtonAbbrechen.setText("Abbrechen");

        jCheckBoxStarten.setSelected(true);
        jCheckBoxStarten.setText("Download sofort starten");

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("Speicherort"));

        jButtonZiel.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/fileopen_16.png"))); // NOI18N
        jButtonZiel.setToolTipText("Zielpfad auswählen");

        jLabel1.setText("Zielpfad:");

        jLabel4.setText("Dateiname:");

        jComboBoxPfad.setEditable(true);
        jComboBoxPfad.setMaximumRowCount(15);
        jComboBoxPfad.setModel(new javax.swing.DefaultComboBoxModel(new String[] { " " }));

        jButtonDelHistory.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/del_16.png"))); // NOI18N
        jButtonDelHistory.setToolTipText("History löschen");

        jCheckBoxPfadSpeichern.setText("Zielpfad speichern");

        jCheckBoxInfodatei.setText("Infodatei anlegen");

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
                        .addComponent(jComboBoxPfad, 0, 356, Short.MAX_VALUE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonZiel)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonDelHistory))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jLabel4)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldName))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jCheckBoxInfodatei)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jCheckBoxPfadSpeichern)))
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
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jCheckBoxPfadSpeichern)
                    .addComponent(jCheckBoxInfodatei))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel1Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonZiel, jTextFieldName});

        jPanelSet.setBorder(javax.swing.BorderFactory.createTitledBorder("Programmset zum Aufzeichnen"));

        javax.swing.GroupLayout jPanelSetLayout = new javax.swing.GroupLayout(jPanelSet);
        jPanelSet.setLayout(jPanelSetLayout);
        jPanelSetLayout.setHorizontalGroup(
            jPanelSetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelSetLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jComboBoxPset, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        jPanelSetLayout.setVerticalGroup(
            jPanelSetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelSetLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jComboBoxPset, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(14, Short.MAX_VALUE))
        );

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder("Film laden in"));

        buttonGroup1.add(jRadioButtonAufloesungHd);
        jRadioButtonAufloesungHd.setText("HD");

        buttonGroup1.add(jRadioButtonAufloesungHoch);
        jRadioButtonAufloesungHoch.setText("hoher Auflösung");

        buttonGroup1.add(jRadioButtonAufloesungKlein);
        jRadioButtonAufloesungKlein.setText("niedriger Auflösung");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jRadioButtonAufloesungHd)
                .addGap(18, 18, 18)
                .addComponent(jRadioButtonAufloesungHoch)
                .addGap(18, 18, 18)
                .addComponent(jRadioButtonAufloesungKlein)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jRadioButtonAufloesungHd)
                    .addComponent(jRadioButtonAufloesungHoch)
                    .addComponent(jRadioButtonAufloesungKlein))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanelSet, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
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
                .addComponent(jPanelSet, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jButtonOk)
                    .addComponent(jButtonAbbrechen)
                    .addComponent(jCheckBoxStarten))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
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
    private javax.swing.JComboBox<String> jComboBoxPfad;
    private javax.swing.JComboBox<String> jComboBoxPset;
    private javax.swing.JPanel jPanelSet;
    private javax.swing.JRadioButton jRadioButtonAufloesungHd;
    private javax.swing.JRadioButton jRadioButtonAufloesungHoch;
    private javax.swing.JRadioButton jRadioButtonAufloesungKlein;
    private javax.swing.JTextField jTextFieldName;
    // End of variables declaration//GEN-END:variables

//    private class BeobCheckNamen implements DocumentListener {
//
//        @Override
//        public void insertUpdate(DocumentEvent e) {
//            checkPfadName();
//        }
//
//        @Override
//        public void removeUpdate(DocumentEvent e) {
//            checkPfadName();
//        }
//
//        @Override
//        public void changedUpdate(DocumentEvent e) {
//            checkPfadName();
//        }
//
//        private void checkPfadName() {
//            if (!stopBeob) {
//                nameGeaendert = true;
//                if (!jTextFieldName.getText().equals(GuiFunktionen.checkDateiname(jTextFieldName.getText()))) {
//                    jTextFieldName.setBackground(MVColor.DOWNLOAD_FEHLER.color);
//                } else {
//                    jTextFieldName.setBackground(null);
//                }
//                if (!jComboBoxPfad.getSelectedItem().toString().equals(GuiFunktionen.checkDateiname(jComboBoxPfad.getSelectedItem().toString()))) {
//                    jComboBoxPfad.setBackground(MVColor.DOWNLOAD_FEHLER.color);
//                } else {
//                    jComboBoxPfad.setBackground(null);
//                }
//            }
//        }
//    }
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
                        Log.fehlerMeldung(356871087, "DialogAddDownload.ZielBeobachter", ex);
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
                        Log.fehlerMeldung(356871087, "DialogAddDownload.ZielBeobachter", ex);
                    }
                }
            }
        }
    }
}
