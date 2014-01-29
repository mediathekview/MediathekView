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
package mediathek.gui.dialogEinstellungen;

import com.jidesoft.utils.SystemInfo;
import java.awt.FileDialog;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import mediathek.daten.Daten;
import mediathek.file.GetFile;
import mediathek.gui.PanelVorlage;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.res.GetIcon;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.controller.Log;
import mediathek.tool.MVMessageDialog;

public class PanelEinstellungenErweitert extends PanelVorlage {

    public PanelEinstellungenErweitert(Daten d, JFrame pparentComponent) {
        super(d, pparentComponent);
        initComponents();
        jButtonProgrammDateimanager.setIcon(GetIcon.getIcon("fileopen_16.png"));
        jButtonProgrammUrl.setIcon(GetIcon.getIcon("fileopen_16.png"));
        daten = d;
        init();
        jButtonZielname.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                new DialogHilfe(null, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_UNICODE)).setVisible(true);
            }
        });
        jButtonHilfe.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                new DialogHilfe(null, true, "\n"
                        + "Dieser Text wird als User-Agent\n"
                        + "an den Webserver übertragen. Das entspricht\n"
                        + "der Kennung, die auch die Browser senden.").setVisible(true);
            }
        });
        jRadioButtonAuto.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setUserAgent();
            }
        });
        jRadioButtonManuel.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setUserAgent();
            }
        });
        jTextFieldUserAgent.getDocument().addDocumentListener(new BeobUserAgent());
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_PROGRAMM_OEFFNEN, PanelEinstellungenErweitert.class.getSimpleName()) {
            @Override
            public void ping() {
                init();
            }
        });
        jCheckBoxAboSuchen.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_ABOS_SOFORT_SUCHEN)));
        jCheckBoxAboSuchen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(Konstanten.SYSTEM_ABOS_SOFORT_SUCHEN, Boolean.toString(jCheckBoxAboSuchen.isSelected()));
            }
        });
        jCheckBoxDownloadSofortStarten.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_DOWNLOAD_SOFORT_STARTEN)));
        jCheckBoxDownloadSofortStarten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(Konstanten.SYSTEM_DOWNLOAD_SOFORT_STARTEN, Boolean.toString(jCheckBoxDownloadSofortStarten.isSelected()));
            }
        });
        // ============================
        // Zeildateiname
        switch (Daten.mVConfig.get(Konstanten.SYSTEM_ZIELNAMEN_ANPASSEN)) {
            case Konstanten.ZIELNAMEN_ANPASSEN_NORMAL:
                jRadioButtonNamenNormal.setSelected(true);
                break;
            case Konstanten.ZIELNAMEN_ANPASSEN_UNICODE:
                jRadioButtonNameUnicode.setSelected(true);
                break;
            case Konstanten.ZIELNAMEN_ANPASSEN_ASCII:
                jRadioButtonNameAscii.setSelected(true);
                break;
            case Konstanten.ZIELNAMEN_ANPASSEN_NIX:
                jRadioButtonNameNix.setSelected(true);
                break;
            default:
                jRadioButtonNameUnicode.setSelected(true);
        }
        jRadioButtonNamenNormal.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(Konstanten.SYSTEM_ZIELNAMEN_ANPASSEN, Konstanten.ZIELNAMEN_ANPASSEN_NORMAL);
            }
        });
        jRadioButtonNameAscii.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(Konstanten.SYSTEM_ZIELNAMEN_ANPASSEN, Konstanten.ZIELNAMEN_ANPASSEN_ASCII);
            }
        });
        jRadioButtonNameUnicode.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(Konstanten.SYSTEM_ZIELNAMEN_ANPASSEN, Konstanten.ZIELNAMEN_ANPASSEN_UNICODE);
            }
        });
        jRadioButtonNameNix.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(Konstanten.SYSTEM_ZIELNAMEN_ANPASSEN, Konstanten.ZIELNAMEN_ANPASSEN_NIX);
            }
        });
        // ====================================
        jButtonProgrammDateimanager.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                //we can use native chooser on Mac...
                if (SystemInfo.isMacOSX()) {
                    FileDialog chooser = new FileDialog(daten.mediathekGui, "Dateimanager suchen");
                    chooser.setMode(FileDialog.LOAD);
                    chooser.setVisible(true);
                    if (chooser.getFile() != null) {
                        try {
                            File destination = new File(chooser.getDirectory() + chooser.getFile());
                            jTextFieldProgrammDateimanager.setText(destination.getAbsolutePath());
                        } catch (Exception ex) {
                            Log.fehlerMeldung(798963047, Log.FEHLER_ART_PROG, "PanelEinstellungenErweitert.ZielBeobachter", ex);
                        }
                    }
                } else {
                    int returnVal;
                    JFileChooser chooser = new JFileChooser();
                    if (!jTextFieldProgrammDateimanager.getText().equals("")) {
                        chooser.setCurrentDirectory(new File(jTextFieldProgrammDateimanager.getText()));
                    } else {
                        chooser.setCurrentDirectory(new File(GuiFunktionen.getHomePath()));
                    }
                    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                    returnVal = chooser.showOpenDialog(null);
                    if (returnVal == JFileChooser.APPROVE_OPTION) {
                        try {
                            jTextFieldProgrammDateimanager.setText(chooser.getSelectedFile().getAbsolutePath());
                        } catch (Exception ex) {
                            Log.fehlerMeldung(963299647, Log.FEHLER_ART_PROG, "PanelEinstellungenErweitert.ZielBeobachter", ex);
                        }
                    }
                }
                // merken und prüfen
                Daten.mVConfig.add(Konstanten.SYSTEM_ORDNER_OEFFNEN,jTextFieldProgrammDateimanager.getText());
                String programm = jTextFieldProgrammDateimanager.getText();
                if (!programm.equals("")) {
                    try {
                        if (!new File(programm).exists()) {
                            MVMessageDialog.showMessageDialog(daten.mediathekGui, "Das Programm:  " + "\"" + programm + "\"" + "  existiert nicht!", "Fehler", JOptionPane.ERROR_MESSAGE);
                        } else if (!new File(programm).canExecute()) {
                            MVMessageDialog.showMessageDialog(daten.mediathekGui, "Das Programm:  " + "\"" + programm + "\"" + "  kann nicht ausgeführt werden!", "Fehler", JOptionPane.ERROR_MESSAGE);
                        }
                    } catch (Exception ignored) {
                    }
                }

            }
        });
        jButtonProgrammVideoplayer.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                //we can use native chooser on Mac...
                if (SystemInfo.isMacOSX()) {
                    FileDialog chooser = new FileDialog(daten.mediathekGui, "Dateimanager suchen");
                    chooser.setMode(FileDialog.LOAD);
                    chooser.setVisible(true);
                    if (chooser.getFile() != null) {
                        try {
                            File destination = new File(chooser.getDirectory() + chooser.getFile());
                            jTextFieldVideoplayer.setText(destination.getAbsolutePath());
                        } catch (Exception ex) {
                            Log.fehlerMeldung(821036489, Log.FEHLER_ART_PROG, "PanelEinstellungenErweitert.ZielBeobachter", ex);
                        }
                    }
                } else {
                    int returnVal;
                    JFileChooser chooser = new JFileChooser();
                    if (!jTextFieldVideoplayer.getText().equals("")) {
                        chooser.setCurrentDirectory(new File(jTextFieldVideoplayer.getText()));
                    } else {
                        chooser.setCurrentDirectory(new File(GuiFunktionen.getHomePath()));
                    }
                    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                    returnVal = chooser.showOpenDialog(null);
                    if (returnVal == JFileChooser.APPROVE_OPTION) {
                        try {
                            jTextFieldVideoplayer.setText(chooser.getSelectedFile().getAbsolutePath());
                        } catch (Exception ex) {
                            Log.fehlerMeldung(732656980, Log.FEHLER_ART_PROG, "PanelEinstellungenErweitert.ZielBeobachter", ex);
                        }
                    }
                }
                // merken und prüfen
                Daten.mVConfig.add(Konstanten.SYSTEM_PLAYER_ABSPIELEN, jTextFieldVideoplayer.getText());
                String programm = jTextFieldVideoplayer.getText();
                if (!programm.equals("")) {
                    try {
                        if (!new File(programm).exists()) {
                            MVMessageDialog.showMessageDialog(daten.mediathekGui, "Das Programm:  " + "\"" + programm + "\"" + "  existiert nicht!", "Fehler", JOptionPane.ERROR_MESSAGE);
                        } else if (!new File(programm).canExecute()) {
                            MVMessageDialog.showMessageDialog(daten.mediathekGui, "Das Programm:  " + "\"" + programm + "\"" + "  kann nicht ausgeführt werden!", "Fehler", JOptionPane.ERROR_MESSAGE);
                        }
                    } catch (Exception ignored) {
                    }
                }

            }
        });
        jButtonProgrammUrl.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                //we can use native chooser on Mac...
                if (SystemInfo.isMacOSX()) {
                    FileDialog chooser = new FileDialog(daten.mediathekGui, "Browser suchen");
                    chooser.setMode(FileDialog.LOAD);
                    chooser.setVisible(true);
                    if (chooser.getFile() != null) {
                        try {
                            File destination = new File(chooser.getDirectory() + chooser.getFile());
                            jTextFieldProgrammUrl.setText(destination.getAbsolutePath());
                        } catch (Exception ex) {
                            Log.fehlerMeldung(369874598, Log.FEHLER_ART_PROG, "PanelEinstellungenErweitert.ZielBeobachter", ex);
                        }
                    }
                } else {
                    int returnVal;
                    JFileChooser chooser = new JFileChooser();
                    if (!jTextFieldProgrammUrl.getText().equals("")) {
                        chooser.setCurrentDirectory(new File(jTextFieldProgrammUrl.getText()));
                    } else {
                        chooser.setCurrentDirectory(new File(GuiFunktionen.getHomePath()));
                    }
                    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                    returnVal = chooser.showOpenDialog(null);
                    if (returnVal == JFileChooser.APPROVE_OPTION) {
                        try {
                            jTextFieldProgrammUrl.setText(chooser.getSelectedFile().getAbsolutePath());
                        } catch (Exception ex) {
                            Log.fehlerMeldung(469012789, Log.FEHLER_ART_PROG, "PanelEinstellungenErweitert.ZielBeobachter", ex);
                        }
                    }
                }
                // merken und prüfen
                Daten.mVConfig.add(Konstanten.SYSTEM_URL_OEFFNEN, jTextFieldProgrammUrl.getText());
                String programm = jTextFieldProgrammUrl.getText();
                if (!programm.equals("")) {
                    try {
                        if (!new File(programm).exists()) {
                            MVMessageDialog.showMessageDialog(daten.mediathekGui, "Das Programm:  " + "\"" + programm + "\"" + "  existiert nicht!", "Fehler", JOptionPane.ERROR_MESSAGE);
                        } else if (!new File(programm).canExecute()) {
                            MVMessageDialog.showMessageDialog(daten.mediathekGui, "Das Programm:  " + "\"" + programm + "\"" + "  kann nicht ausgeführt werden!", "Fehler", JOptionPane.ERROR_MESSAGE);
                        }
                    } catch (Exception ignored) {
                    }
                }

            }
        });
        jButtonHilfeProgrammDateimanager.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                new DialogHilfe(parentComponent, true, "\n"
                        + "In der Tabelle \"Downloads\" kann man mit der rechten\n"
                        + "Maustaste den Downloadordner des jeweiligen Downloads\n"
                        + "öffnen. Normalerweise wird der Dateimanager des\n"
                        + "Betriebssystems gefunden und geöffnet. Klappt das nicht,\n"
                        + "kann hier ein Programm dafür angegeben werden.").setVisible(true);
            }
        });
        jButtonHilfeNeuladen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                new DialogHilfe(parentComponent, true, "\n"
                        + "Abos automatisch suchen:\n"
                        + "Nach dem Neuladen einer Filmliste wird dann\n"
                        + "sofort nach neuen Abos gesucht. Ansonsten muss man\n"
                        + "im Tab Download auf \"Auffrischen\" klicken.\n"
                        + "\n"
                        + "Downloads sofort starten:\n"
                        + "Neu angelegte Downloads (aus Abos) werden\n"
                        + "sofort gestartet. Ansonsten muss man sie\n"
                        + "selbst starten.\n"
                        + "\n"
                        + "Alte Filmliste vorher umbenennen:\n"
                        + "Vor dem Neuladen einer Filmliste wird\n"
                        + "die bestehende umbenannt und bleibt so erhalten.\n"
                        + "Ansonsten wir sie überschrieben.").setVisible(true);
            }
        });
        jButtonHilfeVideoplayer.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                new DialogHilfe(parentComponent, true, "\n"
                        + "In der Tabelle \"Downloads\" kann man mit der rechten\n"
                        + "Maustaste den geladenen Film in einem Videoplayer\n"
                        + "öffnen. Normalerweise wird der Videoplayer des\n"
                        + "Betriebssystems gefunden und geöffnet. Klappt das nicht,\n"
                        + "kann hier ein Programm dafür angegeben werden.").setVisible(true);
            }
        });
        jButtonHilfeProgrammUrl.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                new DialogHilfe(parentComponent, true, "\n"
                        + "Wenn das Programm versucht, eine URL zu öffnen\n"
                        + "und die Standardanwendung nicht startet,\n"
                        + "kann man damit ein Programm auswählen, mit\n"
                        + "dem URLs geöffnet werden sollen.").setVisible(true);
            }
        });
        jTextFieldProgrammDateimanager.setText(Daten.mVConfig.get(Konstanten.SYSTEM_ORDNER_OEFFNEN));
        jTextFieldProgrammDateimanager.getDocument().addDocumentListener(new DocumentListener() {
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
                Daten.mVConfig.add(Konstanten.SYSTEM_ORDNER_OEFFNEN, jTextFieldProgrammDateimanager.getText());
            }
        });
        jTextFieldVideoplayer.setText(Daten.mVConfig.get(Konstanten.SYSTEM_PLAYER_ABSPIELEN));
        jTextFieldVideoplayer.getDocument().addDocumentListener(new DocumentListener() {
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
                Daten.mVConfig.add(Konstanten.SYSTEM_PLAYER_ABSPIELEN, jTextFieldVideoplayer.getText());
            }
        });
        jTextFieldProgrammUrl.setText(Daten.mVConfig.get(Konstanten.SYSTEM_URL_OEFFNEN));
        jTextFieldProgrammUrl.getDocument().addDocumentListener(new DocumentListener() {
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
                Daten.mVConfig.add(Konstanten.SYSTEM_URL_OEFFNEN, jTextFieldProgrammUrl.getText());
            }
        });
    }

    private void init() {
        // UserAgent
        jTextFieldAuto.setText(Konstanten.USER_AGENT_DEFAULT);
        jTextFieldUserAgent.setText(Daten.mVConfig.get(Konstanten.SYSTEM_USER_AGENT));
        if (Daten.isUserAgentAuto()) {
            jRadioButtonAuto.setSelected(true);
        } else {
            jRadioButtonManuel.setSelected(true);
        }
        jTextFieldUserAgent.setEditable(jRadioButtonManuel.isSelected());
        jTextFieldProgrammDateimanager.setText(Daten.mVConfig.get(Konstanten.SYSTEM_ORDNER_OEFFNEN));
        jTextFieldProgrammUrl.setText(Daten.mVConfig.get(Konstanten.SYSTEM_URL_OEFFNEN));
    }

    private void setUserAgent() {
        if (jRadioButtonAuto.isSelected()) {
            Daten.setUserAgentAuto();
        } else {
            Daten.setUserAgentManuel(jTextFieldUserAgent.getText());
        }
        jTextFieldUserAgent.setEditable(jRadioButtonManuel.isSelected());
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.ButtonGroup buttonGroup1 = new javax.swing.ButtonGroup();
        buttonGroup2 = new javax.swing.ButtonGroup();
        javax.swing.JPanel jPanel6 = new javax.swing.JPanel();
        jCheckBoxAboSuchen = new javax.swing.JCheckBox();
        jCheckBoxDownloadSofortStarten = new javax.swing.JCheckBox();
        jButtonHilfeNeuladen = new javax.swing.JButton();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        jTextFieldUserAgent = new javax.swing.JTextField();
        jButtonHilfe = new javax.swing.JButton();
        jRadioButtonAuto = new javax.swing.JRadioButton();
        jRadioButtonManuel = new javax.swing.JRadioButton();
        jTextFieldAuto = new javax.swing.JTextField();
        javax.swing.JPanel jPanel3 = new javax.swing.JPanel();
        jButtonZielname = new javax.swing.JButton();
        jRadioButtonNamenNormal = new javax.swing.JRadioButton();
        jRadioButtonNameUnicode = new javax.swing.JRadioButton();
        jRadioButtonNameAscii = new javax.swing.JRadioButton();
        jRadioButtonNameNix = new javax.swing.JRadioButton();
        jPanel2 = new javax.swing.JPanel();
        jTextFieldProgrammDateimanager = new javax.swing.JTextField();
        jButtonProgrammDateimanager = new javax.swing.JButton();
        jButtonHilfeProgrammDateimanager = new javax.swing.JButton();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jTextFieldVideoplayer = new javax.swing.JTextField();
        jButtonHilfeVideoplayer = new javax.swing.JButton();
        jButtonProgrammVideoplayer = new javax.swing.JButton();
        jPanel4 = new javax.swing.JPanel();
        jTextFieldProgrammUrl = new javax.swing.JTextField();
        jButtonProgrammUrl = new javax.swing.JButton();
        jButtonHilfeProgrammUrl = new javax.swing.JButton();

        jPanel6.setBorder(javax.swing.BorderFactory.createTitledBorder("Nach dem Neuladen der Filmliste"));

        jCheckBoxAboSuchen.setText("Abos automatisch suchen");

        jCheckBoxDownloadSofortStarten.setText("Downloads sofort starten");

        jButtonHilfeNeuladen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/help_16.png"))); // NOI18N

        javax.swing.GroupLayout jPanel6Layout = new javax.swing.GroupLayout(jPanel6);
        jPanel6.setLayout(jPanel6Layout);
        jPanel6Layout.setHorizontalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jCheckBoxAboSuchen)
                    .addComponent(jCheckBoxDownloadSofortStarten))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(jButtonHilfeNeuladen)
                .addContainerGap())
        );
        jPanel6Layout.setVerticalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jCheckBoxAboSuchen)
                    .addComponent(jButtonHilfeNeuladen))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jCheckBoxDownloadSofortStarten)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("User-Agent"));

        jButtonHilfe.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/help_16.png"))); // NOI18N

        buttonGroup1.add(jRadioButtonAuto);
        jRadioButtonAuto.setText("Auto:");

        buttonGroup1.add(jRadioButtonManuel);

        jTextFieldAuto.setEditable(false);

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jRadioButtonManuel)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldUserAgent))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jRadioButtonAuto)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldAuto)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonHilfe)))
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addGap(13, 13, 13)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jRadioButtonAuto)
                    .addComponent(jTextFieldAuto, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonHilfe))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jRadioButtonManuel)
                    .addComponent(jTextFieldUserAgent, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(26, Short.MAX_VALUE))
        );

        jPanel1Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonHilfe, jTextFieldAuto, jTextFieldUserAgent});

        jPanel3.setBorder(javax.swing.BorderFactory.createTitledBorder("Dateiname der gespeicherten Filme"));

        jButtonZielname.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/help_16.png"))); // NOI18N

        buttonGroup2.add(jRadioButtonNamenNormal);
        jRadioButtonNamenNormal.setText("einfache Prüfung");

        buttonGroup2.add(jRadioButtonNameUnicode);
        jRadioButtonNameUnicode.setSelected(true);
        jRadioButtonNameUnicode.setText("Unicode-Zeichen \"vereinfachen\"");

        buttonGroup2.add(jRadioButtonNameAscii);
        jRadioButtonNameAscii.setText("nur ASCII-Zeichen erlauben");

        buttonGroup2.add(jRadioButtonNameNix);
        jRadioButtonNameNix.setForeground(new java.awt.Color(153, 0, 51));
        jRadioButtonNameNix.setText("Keine Veränderung vornehmen (dann selbst prüfen!)");

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                        .addComponent(jRadioButtonNamenNormal)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jButtonZielname))
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jRadioButtonNameNix)
                            .addGroup(jPanel3Layout.createSequentialGroup()
                                .addComponent(jRadioButtonNameUnicode)
                                .addGap(18, 18, 18)
                                .addComponent(jRadioButtonNameAscii)))
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonZielname)
                    .addComponent(jRadioButtonNamenNormal))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jRadioButtonNameUnicode)
                    .addComponent(jRadioButtonNameAscii))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jRadioButtonNameNix)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder("Tab Download"));

        jButtonProgrammDateimanager.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N

        jButtonHilfeProgrammDateimanager.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/help_16.png"))); // NOI18N

        jLabel1.setText("Ordner öffnen");

        jLabel2.setText("Film abspielen");

        jButtonHilfeVideoplayer.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/help_16.png"))); // NOI18N

        jButtonProgrammVideoplayer.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel1)
                            .addComponent(jLabel2))
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                            .addComponent(jTextFieldVideoplayer)
                            .addComponent(jTextFieldProgrammDateimanager))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                                .addComponent(jButtonProgrammDateimanager)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonHilfeProgrammDateimanager))
                            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                                .addComponent(jButtonProgrammVideoplayer)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonHilfeVideoplayer)))))
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jTextFieldProgrammDateimanager, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonProgrammDateimanager)
                    .addComponent(jButtonHilfeProgrammDateimanager))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(jLabel2)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldVideoplayer, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(1, 1, 1))
                    .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jButtonHilfeVideoplayer)
                        .addComponent(jButtonProgrammVideoplayer)))
                .addContainerGap(29, Short.MAX_VALUE))
        );

        jPanel2Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonHilfeProgrammDateimanager, jButtonProgrammDateimanager, jTextFieldProgrammDateimanager});

        jPanel2Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonHilfeVideoplayer, jButtonProgrammVideoplayer, jTextFieldVideoplayer});

        jPanel4.setBorder(javax.swing.BorderFactory.createTitledBorder("Programm zum Öffnen von URLs"));

        jButtonProgrammUrl.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N

        jButtonHilfeProgrammUrl.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/help_16.png"))); // NOI18N

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTextFieldProgrammUrl)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonProgrammUrl)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonHilfeProgrammUrl)
                .addContainerGap())
        );
        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jTextFieldProgrammUrl, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonProgrammUrl)
                    .addComponent(jButtonHilfeProgrammUrl))
                .addContainerGap(29, Short.MAX_VALUE))
        );

        jPanel4Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonHilfeProgrammUrl, jButtonProgrammUrl, jTextFieldProgrammUrl});

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel1, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel6, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel6, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(50, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroup2;
    private javax.swing.JButton jButtonHilfe;
    private javax.swing.JButton jButtonHilfeNeuladen;
    private javax.swing.JButton jButtonHilfeProgrammDateimanager;
    private javax.swing.JButton jButtonHilfeProgrammUrl;
    private javax.swing.JButton jButtonHilfeVideoplayer;
    private javax.swing.JButton jButtonProgrammDateimanager;
    private javax.swing.JButton jButtonProgrammUrl;
    private javax.swing.JButton jButtonProgrammVideoplayer;
    private javax.swing.JButton jButtonZielname;
    private javax.swing.JCheckBox jCheckBoxAboSuchen;
    private javax.swing.JCheckBox jCheckBoxDownloadSofortStarten;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JRadioButton jRadioButtonAuto;
    private javax.swing.JRadioButton jRadioButtonManuel;
    private javax.swing.JRadioButton jRadioButtonNameAscii;
    private javax.swing.JRadioButton jRadioButtonNameNix;
    private javax.swing.JRadioButton jRadioButtonNameUnicode;
    private javax.swing.JRadioButton jRadioButtonNamenNormal;
    private javax.swing.JTextField jTextFieldAuto;
    private javax.swing.JTextField jTextFieldProgrammDateimanager;
    private javax.swing.JTextField jTextFieldProgrammUrl;
    private javax.swing.JTextField jTextFieldUserAgent;
    private javax.swing.JTextField jTextFieldVideoplayer;
    // End of variables declaration//GEN-END:variables

    private class BeobUserAgent implements DocumentListener {

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
            Daten.setUserAgentManuel(jTextFieldUserAgent.getText());
        }
    }
}
