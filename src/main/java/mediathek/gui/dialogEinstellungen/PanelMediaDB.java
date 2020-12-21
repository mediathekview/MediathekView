package mediathek.gui.dialogEinstellungen;

import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.daten.DatenMediaDB;
import mediathek.daten.DatenMediaPath;
import mediathek.file.GetFile;
import mediathek.gui.PanelVorlage;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.gui.messages.mediadb.MediaDbStartEvent;
import mediathek.gui.messages.mediadb.MediaDbStopEvent;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.MVMessageDialog;
import mediathek.tool.NoSelectionErrorDialog;
import mediathek.tool.TextCopyPasteHandler;
import mediathek.tool.cellrenderer.CellRendererMediaDB;
import mediathek.tool.models.NonEditableTableModel;
import mediathek.tool.models.TModelMediaDB;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

@SuppressWarnings("serial")
public class PanelMediaDB extends PanelVorlage {
    private final NonEditableTableModel modelPath = new NonEditableTableModel(new Object[][]{}, DatenMediaPath.COLUMN_NAMES);
    private final TModelMediaDB modelMediaDB = new TModelMediaDB(new Object[][]{}, DatenMediaDB.COLUMN_NAMES);
    private static final Logger logger = LogManager.getLogger();

    @Handler
    private void handleMediaDbStartEvent(MediaDbStartEvent e) {
        SwingUtilities.invokeLater(() -> {
            // neue DB suchen
            setIndex(false);
            modelMediaDB.setRowCount(0);
            jToggleButtonLoad.setSelected(false);
        });
    }

    @Handler
    private void handleMediaDbStopEvent(MediaDbStopEvent e) {
        SwingUtilities.invokeLater(() -> {
            // neue DB liegt vor
            jLabelSizeIndex.setText(daten.getListeMediaDB().size() + "");
            setIndex(true);
        });
    }

    public PanelMediaDB(Daten d, JFrame parent) {
        super(d, parent);
        initComponents();
        daten = d;

        daten.getMessageBus().subscribe(this);

        progress.setVisible(false);
        progress.setIndeterminate(true);
        progress.setMaximum(0);
        progress.setMinimum(0);
        progress.setValue(0);
        jTablePath.setModel(modelPath);
        jTablePath.getColumnModel().getColumn(jTablePath.convertColumnIndexToView(DatenMediaPath.MEDIA_PATHE_SAVE)).setMinWidth(0);
        jTablePath.getColumnModel().getColumn(jTablePath.convertColumnIndexToView(DatenMediaPath.MEDIA_PATHE_SAVE)).setPreferredWidth(0);
        jTablePath.getColumnModel().getColumn(jTablePath.convertColumnIndexToView(DatenMediaPath.MEDIA_PATHE_SAVE)).setMaxWidth(0);

        final CellRendererMediaDB cellRenderer = new CellRendererMediaDB();
        jTableMediaDB.setDefaultRenderer(Object.class, cellRenderer);
        jTableMediaDB.setModel(modelMediaDB);

        jTextFieldSuffix.setText(MVConfig.get(MVConfig.Configs.SYSTEM_MEDIA_DB_SUFFIX));
        jTextFieldSuffix.getDocument().addDocumentListener(new DocumentListener() {

            @Override
            public void insertUpdate(DocumentEvent e) {
                MVConfig.add(MVConfig.Configs.SYSTEM_MEDIA_DB_SUFFIX, jTextFieldSuffix.getText());
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                MVConfig.add(MVConfig.Configs.SYSTEM_MEDIA_DB_SUFFIX, jTextFieldSuffix.getText());
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                MVConfig.add(MVConfig.Configs.SYSTEM_MEDIA_DB_SUFFIX, jTextFieldSuffix.getText());
            }
        });

        jTextFieldExportPath.setText(MVConfig.get(MVConfig.Configs.SYSTEM_MEDIA_DB_EXPORT_DATEI));
        jTextFieldExportPath.getDocument().addDocumentListener(new BeobTextFeld());
        var handler = new TextCopyPasteHandler<>(jTextFieldExportPath);
        jTextFieldExportPath.setComponentPopupMenu(handler.getPopupMenu());

        handler = new TextCopyPasteHandler<>(jTextFieldPath);
        jTextFieldPath.setComponentPopupMenu(handler.getPopupMenu());

        handler = new TextCopyPasteHandler<>(jTextFieldSuffix);
        jTextFieldSuffix.setComponentPopupMenu(handler.getPopupMenu());

        jRadioButtonOhneSuffix.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_MEDIA_DB_SUFFIX_OHNE)));
        jRadioButtonMitSuffix.setSelected(!Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_MEDIA_DB_SUFFIX_OHNE)));
        jRadioButtonOhneSuffix.addActionListener((ActionEvent e) -> MVConfig.add(MVConfig.Configs.SYSTEM_MEDIA_DB_SUFFIX_OHNE, Boolean.toString(jRadioButtonOhneSuffix.isSelected())));
        jRadioButtonMitSuffix.addActionListener((ActionEvent e) -> MVConfig.add(MVConfig.Configs.SYSTEM_MEDIA_DB_SUFFIX_OHNE, Boolean.toString(jRadioButtonOhneSuffix.isSelected())));

        jCheckBoxMediaDB.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_MEDIA_DB_ECHTZEITSUCHE)));
        jCheckBoxMediaDB.addActionListener((ActionEvent ae) -> MVConfig.add(MVConfig.Configs.SYSTEM_MEDIA_DB_ECHTZEITSUCHE, Boolean.toString(jCheckBoxMediaDB.isSelected())));

        jButtonMakeIndex.addActionListener((ActionEvent e) -> {
            jLabelSizeIndex.setText("0");
            daten.getListeMediaDB().createMediaDB("");
        });
        btnDel.addActionListener(l -> {
            final int ret = JOptionPane.showConfirmDialog(parentComponent, "Auch die Medien aus externen Laufwerken löschen?", "Löschen", JOptionPane.YES_NO_CANCEL_OPTION);
            if (ret == JOptionPane.YES_OPTION) {
                //alles löschen
                daten.getListeMediaDB().delList(false);
            } else if (ret == JOptionPane.NO_OPTION) {
                //externe nicht löschen
                daten.getListeMediaDB().delList(true);
            }
        });
        jButtonPath.setIcon(Icons.ICON_BUTTON_FILE_OPEN);
        jButtonAdd.setIcon(Icons.ICON_BUTTON_ADD);
        jButtonRemove.setIcon(Icons.ICON_BUTTON_REMOVE);
        jButtonPath.addActionListener(new BeobPath(false/*ext*/));
        jButtonAdd.addActionListener((ActionEvent e) -> addPath());
        jButtonRemove.addActionListener((ActionEvent e) -> removePath());
        jButtonHelp.setIcon(Icons.ICON_BUTTON_HELP);
        jButtonHelp.addActionListener((ActionEvent e) -> new DialogHilfe(MediathekGui.ui(), true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_PANEL_MEDIA_DB)).setVisible(true));
        jButtonExportPath.setIcon(Icons.ICON_BUTTON_FILE_OPEN);
        jButtonExport.addActionListener(l -> filmeExportieren());
        jButtonExportPath.addActionListener(new BeobPfad());
        btnExtAdd.addActionListener(l -> {
            String s = (String) cbxExtMedien.getSelectedItem();
            if (s != null && !s.isEmpty()) {
                daten.getListeMediaDB().createMediaDB(s);
            }
        });
        btnExtPath.addActionListener(new BeobPath(true/*ext*/));
        btnClean.addActionListener(l -> daten.getListeMediaDB().cleanList());

        jToggleButtonLoad.addActionListener((ActionEvent e) -> {
            if (jToggleButtonLoad.isSelected()) {
                daten.getListeMediaDB().getModelMediaDB(modelMediaDB);
            } else {
                modelMediaDB.setRowCount(0);
            }
        });

        setCbkExt("");
        setTablePath();
    }

    private void filmeExportieren() {
        int ret;
        String exporDatei = jTextFieldExportPath.getText();
        if (exporDatei.equals("")) {
            MVMessageDialog.showMessageDialog(parentComponent, "Keine Datei angegeben", "Pfad", JOptionPane.INFORMATION_MESSAGE);
        } else {
            try {
                if (new File(exporDatei).exists()) {
                    ret = JOptionPane.showConfirmDialog(parentComponent, "Datei:  " + "\"" + exporDatei + "\"" + "  existiert bereits", "Überschreiben?",
                            JOptionPane.YES_NO_OPTION);
                } else {
                    ret = JOptionPane.OK_OPTION;
                }
                if (ret == JOptionPane.OK_OPTION) {
                    this.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
                    updateUI();
                    daten.getListeMediaDB().exportListe(exporDatei);
                    if (!new File(exporDatei).exists()) {
                        MVMessageDialog.showMessageDialog(parentComponent, "Datei:  " + "\"" + exporDatei + "\"" + "  Konnte nicht erstellt werden!", "Fehler", JOptionPane.ERROR_MESSAGE);
                    }
                }
            } catch (Exception ex) {
                logger.error("filmeExportieren()", ex);
            }
        }
        this.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
    }

    private void setIndex(boolean noIndex) {
        progress.setVisible(!noIndex);
        jTextFieldPath.setEnabled(noIndex);
        jButtonMakeIndex.setEnabled(noIndex);
        jButtonAdd.setEnabled(noIndex);
        jButtonPath.setEnabled(noIndex);
        jButtonRemove.setEnabled(noIndex);
    }

    private void addPath() {
        String add = jTextFieldPath.getText();
        if (add.isEmpty()) {
            return;
        }
        for (DatenMediaPath mp : daten.getListeMediaPath()) {
            if (mp.arr[DatenMediaPath.MEDIA_PATH_PATH].equals(add)) {
                return; // dann gibts den schon
            }
        }
        daten.getListeMediaPath().add(new DatenMediaPath(add, false));
        setTablePath(); //neu aufbauen
    }

    private void removePath() {
        int row = jTablePath.getSelectedRow();
        if (row < 0) {
            NoSelectionErrorDialog.show();
            return;
        }
        String path = jTablePath.getModel().getValueAt(jTablePath.convertRowIndexToModel(row), 0).toString();
        daten.getListeMediaPath().removeIf(mp -> mp.arr[DatenMediaPath.MEDIA_PATH_PATH].equals(path));
        setTablePath(); //neu aufbauen
    }

    private void setTablePath() {
        daten.getListeMediaPath().addObjectData(modelPath);
    }

    private void setCbkExt(String add) {
        if (!add.isEmpty()) {
            daten.getListeMediaPath().addSave(new DatenMediaPath(add, true));
            cbxExtMedien.setModel(daten.getListeMediaPath().getComboModel());
            cbxExtMedien.setSelectedItem(add);
        } else {
            cbxExtMedien.setModel(daten.getListeMediaPath().getComboModel());
        }
    }

    private class BeobPath implements ActionListener {

        boolean ext;

        public BeobPath(boolean ext) {
            this.ext = ext;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            //we can use native directory chooser on Mac...
            if (SystemUtils.IS_OS_MAC_OSX) {
                //we want to select a directory only, so temporarily change properties
                System.setProperty("apple.awt.fileDialogForDirectories", "true");
                FileDialog chooser = new FileDialog(MediathekGui.ui(), "Pfad zu den Filmen wählen");
                chooser.setVisible(true);
                if (chooser.getFile() != null) {
                    //A directory was selected, that means Cancel was not pressed
                    try {
                        String path = new File(chooser.getDirectory() + chooser.getFile()).getAbsolutePath();
                        if (ext) {
                            setCbkExt(path);
                        } else {
                            jTextFieldPath.setText(path);
                        }
                    } catch (Exception ex) {
                        logger.error("BeobPath.actionPerformed", ex);
                    }
                }
                System.setProperty("apple.awt.fileDialogForDirectories", "false");
            } else {
                //use the cross-platform swing chooser
                int returnVal;
                JFileChooser chooser = new JFileChooser();
                chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                if (!jTextFieldPath.getText().equals("")) {
                    chooser.setCurrentDirectory(new File(jTextFieldPath.getText()));
                }
                returnVal = chooser.showOpenDialog(null);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        String path = chooser.getSelectedFile().getPath();
                        if (ext) {
                            setCbkExt(path);
                        } else {
                            jTextFieldPath.setText(path);
                        }
                    } catch (Exception ex) {
                        logger.error("BeobPath.actionPerformed", ex);
                    }
                }
            }
        }
    }

    private class BeobTextFeld implements DocumentListener {

        @Override
        public void changedUpdate(DocumentEvent e) {
            tusEinfach();
        }

        @Override
        public void insertUpdate(DocumentEvent e) {
            tusEinfach();
        }

        @Override
        public void removeUpdate(DocumentEvent e) {
            tusEinfach();
        }

        void tusEinfach() {
            MVConfig.add(MVConfig.Configs.SYSTEM_MEDIA_DB_EXPORT_DATEI, jTextFieldExportPath.getText());
        }
    }

    private class BeobPfad implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            //we can use native chooser on Mac...
            if (SystemUtils.IS_OS_MAC_OSX) {
                FileDialog chooser = new FileDialog(MediathekGui.ui(), "Filme exportieren");
                chooser.setMode(FileDialog.SAVE);
                chooser.setVisible(true);
                if (chooser.getFile() != null) {
                    try {
                        File destination = new File(chooser.getDirectory() + chooser.getFile());
                        jTextFieldExportPath.setText(destination.getAbsolutePath());
                    } catch (Exception ex) {
                        logger.error(ex);
                    }
                }
            } else {
                int returnVal;
                JFileChooser chooser = new JFileChooser();
                if (!jTextFieldExportPath.getText().equals("")) {
                    chooser.setCurrentDirectory(new File(jTextFieldExportPath.getText()));
                }
                chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                chooser.setFileHidingEnabled(false);
                returnVal = chooser.showOpenDialog(null);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        jTextFieldExportPath.setText(chooser.getSelectedFile().getAbsolutePath());
                    } catch (Exception ex) {
                        logger.error("BeobPfad.actionPerformed", ex);
                    }
                }
            }
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.ButtonGroup buttonGroup1 = new javax.swing.ButtonGroup();
        jButtonHelp = new javax.swing.JButton();
        progress = new javax.swing.JProgressBar();
        jButtonMakeIndex = new javax.swing.JButton();
        javax.swing.JTabbedPane jTabbedPane1 = new javax.swing.JTabbedPane();
        javax.swing.JPanel jPanel6 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel5 = new javax.swing.JLabel();
        javax.swing.JScrollPane jScrollPane1 = new javax.swing.JScrollPane();
        jTablePath = new javax.swing.JTable();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel4 = new javax.swing.JLabel();
        jTextFieldPath = new javax.swing.JTextField();
        jButtonPath = new javax.swing.JButton();
        jButtonAdd = new javax.swing.JButton();
        jButtonRemove = new javax.swing.JButton();
        javax.swing.JScrollPane jScrollPane3 = new javax.swing.JScrollPane();
        javax.swing.JTextArea jTextArea1 = new javax.swing.JTextArea();
        javax.swing.JPanel jPanel9 = new javax.swing.JPanel();
        javax.swing.JPanel jPanel8 = new javax.swing.JPanel();
        cbxExtMedien = new javax.swing.JComboBox<>();
        btnExtAdd = new javax.swing.JButton();
        btnExtPath = new javax.swing.JButton();
        javax.swing.JScrollPane jScrollPane4 = new javax.swing.JScrollPane();
        javax.swing.JTextArea jTextArea2 = new javax.swing.JTextArea();
        javax.swing.JPanel jPanel4 = new javax.swing.JPanel();
        javax.swing.JPanel jPanel3 = new javax.swing.JPanel();
        jCheckBoxMediaDB = new javax.swing.JCheckBox();
        btnClean = new javax.swing.JButton();
        btnDel = new javax.swing.JButton();
        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        jRadioButtonOhneSuffix = new javax.swing.JRadioButton();
        jRadioButtonMitSuffix = new javax.swing.JRadioButton();
        jTextFieldSuffix = new javax.swing.JTextField();
        javax.swing.JLabel jLabel6 = new javax.swing.JLabel();
        javax.swing.JLabel jLabel7 = new javax.swing.JLabel();
        javax.swing.JPanel jPanel7 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        jTextFieldExportPath = new javax.swing.JTextField();
        jButtonExportPath = new javax.swing.JButton();
        jButtonExport = new javax.swing.JButton();
        javax.swing.JPanel jPanel5 = new javax.swing.JPanel();
        jToggleButtonLoad = new javax.swing.JToggleButton();
        javax.swing.JScrollPane jScrollPane2 = new javax.swing.JScrollPane();
        jTableMediaDB = new javax.swing.JTable();
        javax.swing.JLabel jLabel2 = new javax.swing.JLabel();
        jLabelSizeIndex = new javax.swing.JLabel();

        setMinimumSize(new java.awt.Dimension(2, 483));

        jButtonHelp.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-help.png"))); // NOI18N
        jButtonHelp.setToolTipText("Hilfe anzeigen");

        jButtonMakeIndex.setText("Index neu aufbauen");
        jButtonMakeIndex.setToolTipText("");

        jLabel5.setText("In den Pfaden nach Medien suchen");

        jTablePath.setModel(new javax.swing.table.DefaultTableModel(
                new Object[][]{

                },
                new String[]{

                }
        ));
        jScrollPane1.setViewportView(jTablePath);

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)), "Pfad hinzufügen/löschen"));

        jLabel4.setText("Pfad:");

        jButtonPath.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png"))); // NOI18N
        jButtonPath.setToolTipText("Pfad auswählen");

        jButtonAdd.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-add.png"))); // NOI18N
        jButtonAdd.setToolTipText("vorgegebenen Pfad hinzufügen");

        jButtonRemove.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-remove.png"))); // NOI18N
        jButtonRemove.setToolTipText("ausgewählten Pfad entfernen");

        jTextArea1.setEditable(false);
        jTextArea1.setColumns(20);
        jTextArea1.setLineWrap(true);
        jTextArea1.setRows(3);
        jTextArea1.setText("Die Pfade werden beim Programmstart nach Medien durchsucht. Die Mediensammlung wird so aktuell gehalten.");
        jTextArea1.setWrapStyleWord(true);
        jScrollPane3.setViewportView(jTextArea1);

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
                jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel1Layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jScrollPane3)
                                        .addGroup(jPanel1Layout.createSequentialGroup()
                                                .addComponent(jLabel4)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jTextFieldPath)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                                .addComponent(jButtonPath)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jButtonAdd)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jButtonRemove)))
                                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
                jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel1Layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                                        .addComponent(jButtonAdd)
                                        .addComponent(jButtonPath)
                                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                                .addComponent(jLabel4)
                                                .addComponent(jTextFieldPath, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                                        .addComponent(jButtonRemove))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jScrollPane3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel1Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonAdd, jButtonPath, jButtonRemove, jTextFieldPath});

        javax.swing.GroupLayout jPanel6Layout = new javax.swing.GroupLayout(jPanel6);
        jPanel6.setLayout(jPanel6Layout);
        jPanel6Layout.setHorizontalGroup(
                jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel6Layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 499, Short.MAX_VALUE)
                                        .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                        .addGroup(jPanel6Layout.createSequentialGroup()
                                                .addComponent(jLabel5)
                                                .addGap(0, 0, Short.MAX_VALUE)))
                                .addContainerGap())
        );
        jPanel6Layout.setVerticalGroup(
                jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel6Layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jLabel5)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 205, Short.MAX_VALUE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addContainerGap())
        );

        jTabbedPane1.addTab("Pfade", jPanel6);

        jPanel8.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)), "Externe Medien dauerhaft hinzufügen"));

        cbxExtMedien.setModel(new javax.swing.DefaultComboBoxModel<>(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));

        btnExtAdd.setText("Pfad absuchen");
        btnExtAdd.setToolTipText("Ausgewählten Pfad nach Medien absuchen");

        btnExtPath.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png"))); // NOI18N
        btnExtPath.setToolTipText("Pfad auswählen");

        jTextArea2.setEditable(false);
        jTextArea2.setColumns(20);
        jTextArea2.setLineWrap(true);
        jTextArea2.setRows(3);
        jTextArea2.setText("Hier können Medien aus externen Quellen hinzugefügt werden. Die gefundenen Medien werden gespeichert und sind  dann dauerhaft verfügbar.");
        jTextArea2.setWrapStyleWord(true);
        jScrollPane4.setViewportView(jTextArea2);

        javax.swing.GroupLayout jPanel8Layout = new javax.swing.GroupLayout(jPanel8);
        jPanel8.setLayout(jPanel8Layout);
        jPanel8Layout.setHorizontalGroup(
                jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel8Layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jScrollPane4, javax.swing.GroupLayout.DEFAULT_SIZE, 465, Short.MAX_VALUE)
                                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel8Layout.createSequentialGroup()
                                                .addComponent(cbxExtMedien, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                                .addComponent(btnExtPath))
                                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel8Layout.createSequentialGroup()
                                                .addGap(0, 0, Short.MAX_VALUE)
                                                .addComponent(btnExtAdd)))
                                .addContainerGap())
        );
        jPanel8Layout.setVerticalGroup(
                jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel8Layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(btnExtPath)
                                        .addComponent(cbxExtMedien, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(btnExtAdd)
                                .addGap(18, 18, 18)
                                .addComponent(jScrollPane4, javax.swing.GroupLayout.PREFERRED_SIZE, 54, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout jPanel9Layout = new javax.swing.GroupLayout(jPanel9);
        jPanel9.setLayout(jPanel9Layout);
        jPanel9Layout.setHorizontalGroup(
                jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel9Layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jPanel8, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addContainerGap())
        );
        jPanel9Layout.setVerticalGroup(
                jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel9Layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jPanel8, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addContainerGap(201, Short.MAX_VALUE))
        );

        jTabbedPane1.addTab("Externe Medien", jPanel9);

        jPanel3.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));

        jCheckBoxMediaDB.setText("Echtzeitsuche in der Mediensammlung");

        btnClean.setText("Doppelte Einträge löschen");
        btnClean.setToolTipText("Doppelte Einträge in der Mediendatenbak löschen");

        btnDel.setText("Index löschen");
        btnDel.setToolTipText("Komplette Mediendatenbank löschen");

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
                jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel3Layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addGroup(jPanel3Layout.createSequentialGroup()
                                                .addComponent(jCheckBoxMediaDB)
                                                .addGap(0, 0, Short.MAX_VALUE))
                                        .addGroup(jPanel3Layout.createSequentialGroup()
                                                .addComponent(btnClean)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                .addComponent(btnDel, javax.swing.GroupLayout.PREFERRED_SIZE, 222, javax.swing.GroupLayout.PREFERRED_SIZE)))
                                .addContainerGap())
        );
        jPanel3Layout.setVerticalGroup(
                jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel3Layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                        .addComponent(btnClean)
                                        .addComponent(btnDel))
                                .addGap(18, 18, 18)
                                .addComponent(jCheckBoxMediaDB)
                                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel2.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));

        buttonGroup1.add(jRadioButtonOhneSuffix);
        jRadioButtonOhneSuffix.setSelected(true);
        jRadioButtonOhneSuffix.setText("Keine Dateien mit diesem Suffix");

        buttonGroup1.add(jRadioButtonMitSuffix);
        jRadioButtonMitSuffix.setText("Nur Dateien mit diesem Suffix");

        jTextFieldSuffix.setText("txt,xml");

        jLabel6.setText("(z.B.: txt,xml,jpg)");

        jLabel7.setText("(z.B.: mp4,flv,m4v)");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
                jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel2Layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jTextFieldSuffix)
                                        .addGroup(jPanel2Layout.createSequentialGroup()
                                                .addComponent(jRadioButtonMitSuffix)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                .addComponent(jLabel7))
                                        .addGroup(jPanel2Layout.createSequentialGroup()
                                                .addComponent(jRadioButtonOhneSuffix)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 104, Short.MAX_VALUE)
                                                .addComponent(jLabel6)))
                                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
                jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel2Layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                        .addComponent(jRadioButtonOhneSuffix)
                                        .addComponent(jLabel6))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                        .addComponent(jRadioButtonMitSuffix)
                                        .addComponent(jLabel7))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jTextFieldSuffix, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addContainerGap(15, Short.MAX_VALUE))
        );

        jPanel7.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));

        jLabel1.setText("Export Index:");

        jButtonExportPath.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png"))); // NOI18N
        jButtonExportPath.setToolTipText("Datei auswählen");

        jButtonExport.setText("Exportieren");

        javax.swing.GroupLayout jPanel7Layout = new javax.swing.GroupLayout(jPanel7);
        jPanel7.setLayout(jPanel7Layout);
        jPanel7Layout.setHorizontalGroup(
                jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel7Layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addGroup(jPanel7Layout.createSequentialGroup()
                                                .addComponent(jLabel1)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jTextFieldExportPath)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jButtonExportPath))
                                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel7Layout.createSequentialGroup()
                                                .addGap(0, 0, Short.MAX_VALUE)
                                                .addComponent(jButtonExport)))
                                .addContainerGap())
        );
        jPanel7Layout.setVerticalGroup(
                jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel7Layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                                        .addComponent(jLabel1)
                                        .addComponent(jTextFieldExportPath, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addComponent(jButtonExportPath))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jButtonExport)
                                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel7Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonExportPath, jTextFieldExportPath});

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
                jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel4Layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                        .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                        .addComponent(jPanel7, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                                .addContainerGap())
        );
        jPanel4Layout.setVerticalGroup(
                jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel4Layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jPanel7, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addContainerGap(79, Short.MAX_VALUE))
        );

        jTabbedPane1.addTab("Verwalten", jPanel4);

        jToggleButtonLoad.setText("Laden");

        jTableMediaDB.setModel(new javax.swing.table.DefaultTableModel(
                new Object[][]{
                        {null}
                },
                new String[]{
                        "Title 1"
                }
        ));
        jScrollPane2.setViewportView(jTableMediaDB);

        javax.swing.GroupLayout jPanel5Layout = new javax.swing.GroupLayout(jPanel5);
        jPanel5.setLayout(jPanel5Layout);
        jPanel5Layout.setHorizontalGroup(
                jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel5Layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 499, Short.MAX_VALUE)
                                        .addGroup(jPanel5Layout.createSequentialGroup()
                                                .addComponent(jToggleButtonLoad, javax.swing.GroupLayout.PREFERRED_SIZE, 160, javax.swing.GroupLayout.PREFERRED_SIZE)
                                                .addGap(0, 0, Short.MAX_VALUE)))
                                .addContainerGap())
        );
        jPanel5Layout.setVerticalGroup(
                jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel5Layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 333, Short.MAX_VALUE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jToggleButtonLoad)
                                .addContainerGap())
        );

        jTabbedPane1.addTab("Medienliste", jPanel5);

        jLabel2.setText("Anzahl Medien:");

        jLabelSizeIndex.setText("0");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jTabbedPane1, javax.swing.GroupLayout.Alignment.TRAILING)
                                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                                                .addComponent(progress, javax.swing.GroupLayout.PREFERRED_SIZE, 0, Short.MAX_VALUE)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                                .addComponent(jLabel2)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jLabelSizeIndex)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jButtonMakeIndex)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jButtonHelp)))
                                .addContainerGap())
        );
        layout.setVerticalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jTabbedPane1)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                                        .addComponent(progress, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addComponent(jLabel2)
                                        .addComponent(jLabelSizeIndex)
                                        .addComponent(jButtonMakeIndex)
                                        .addComponent(jButtonHelp))
                                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton btnClean;
    private javax.swing.JButton btnDel;
    private javax.swing.JButton btnExtAdd;
    private javax.swing.JButton btnExtPath;
    private javax.swing.JComboBox<String> cbxExtMedien;
    private javax.swing.JButton jButtonAdd;
    private javax.swing.JButton jButtonExport;
    private javax.swing.JButton jButtonExportPath;
    private javax.swing.JButton jButtonHelp;
    private javax.swing.JButton jButtonMakeIndex;
    private javax.swing.JButton jButtonPath;
    private javax.swing.JButton jButtonRemove;
    private javax.swing.JCheckBox jCheckBoxMediaDB;
    private javax.swing.JLabel jLabelSizeIndex;
    private javax.swing.JRadioButton jRadioButtonMitSuffix;
    private javax.swing.JRadioButton jRadioButtonOhneSuffix;
    private javax.swing.JTable jTableMediaDB;
    private javax.swing.JTable jTablePath;
    private javax.swing.JTextField jTextFieldExportPath;
    private javax.swing.JTextField jTextFieldPath;
    private javax.swing.JTextField jTextFieldSuffix;
    private javax.swing.JToggleButton jToggleButtonLoad;
    private javax.swing.JProgressBar progress;
    // End of variables declaration//GEN-END:variables
}
