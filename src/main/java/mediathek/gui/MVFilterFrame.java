/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.gui;

import com.jidesoft.utils.SystemInfo;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Point;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import mSearch.tool.Listener;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.file.GetFile;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.gui.dialog.DialogLeer;
import mediathek.gui.dialogEinstellungen.PanelBlacklist;
import mediathek.res.GetIcon;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.TextCopyPaste;

public class MVFilterFrame extends javax.swing.JFrame implements MVFilter {

    private static final long serialVersionUID = 1L;

    private Daten daten;
    static Point mouseDownCompCoords;
    private JFrame parent;
    private int aktFilter = -1;

    public MVFilterFrame(Daten d) {
        initComponents();
        parent = this;
        daten = d;
        jToggleButtonBlacklist.setText("");
        if (SystemInfo.isWindows()) {
            // zum Abfangen der Win-F4 für comboboxen
            InputMap im = jComboBoxFilterSender.getInputMap();
            im.put(KeyStroke.getKeyStroke(KeyEvent.VK_F4, 0), "einstellungen");
            ActionMap am = jComboBoxFilterSender.getActionMap();
            am.put("einstellungen", new AbstractAction() {
                private static final long serialVersionUID = 1L;
                
                @Override
                public void actionPerformed(ActionEvent e) {
                    Daten.dialogEinstellungen.setVisible(true);
                }
            });
            im = jComboBoxFilterThema.getInputMap();
            im.put(KeyStroke.getKeyStroke(KeyEvent.VK_F4, 0), "einstellungen");
            am = jComboBoxFilterThema.getActionMap();
            am.put("einstellungen", new AbstractAction() {
                private static final long serialVersionUID = 1L;
                
                @Override
                public void actionPerformed(ActionEvent e) {
                    Daten.dialogEinstellungen.setVisible(true);
                }
            });
        }

        mouseDownCompCoords = null;
        setBounds(0, 0, 400, 400);
        this.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        addMouseListener(new MouseListener() {
            @Override
            public void mouseReleased(MouseEvent e) {
                mouseDownCompCoords = null;
            }

            @Override
            public void mousePressed(MouseEvent e) {
                mouseDownCompCoords = e.getPoint();
            }

            @Override
            public void mouseExited(MouseEvent e) {
            }

            @Override
            public void mouseEntered(MouseEvent e) {
            }

            @Override
            public void mouseClicked(MouseEvent e) {
            }
        });

        addMouseMotionListener(new MouseMotionListener() {
            @Override
            public void mouseMoved(MouseEvent e) {
            }

            @Override
            public void mouseDragged(MouseEvent e) {
                Point currCoords = e.getLocationOnScreen();
                parent.setLocation(currCoords.x - mouseDownCompCoords.x, currCoords.y - mouseDownCompCoords.y);
            }
        });
        this.setIconImage(GetIcon.getIcon("MediathekView.png", "/mediathek/res/", 58, 58).getImage());
        this.setTitle("Filter");
        GuiFunktionen.setSize(MVConfig.Configs.SYSTEM_GROESSE_FILTER, this, Daten.mediathekGui);
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent evt) {
                dispose();
            }
        });
        jButtonFilterLoeschen.setIcon(Icons.ICON_BUTTON_CLEAR);
        jButtonOk.addActionListener(e -> dispose());

        setIconBlacklist();
        jToggleButtonBlacklist.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_ON, Boolean.toString(jToggleButtonBlacklist.isSelected()));
            Daten.listeBlacklist.filterListe();
            Listener.notify(Listener.EREIGNIS_BLACKLIST_GEAENDERT, MVFilterFrame.class.getSimpleName());
            setIconBlacklist();
        });
        jToggleButtonBlacklist.addMouseListener(new BeobMausBlacklist());
        Listener.addListener(new Listener(Listener.EREIGNIS_BLACKLIST_GEAENDERT, MVFilterFrame.class.getSimpleName()) {
            @Override
            public void ping() {
                setIconBlacklist();
            }
        });

        jButtonHilfe.setIcon(Icons.ICON_BUTTON_HELP);
        jButtonHilfe.addActionListener(e -> new DialogHilfe(parent, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_FILTER)).setVisible(true));
        setIcon(false); // erst mal alle aus
        jRadioButtonF1.addActionListener(new BeobRadio(0));
        jRadioButtonF2.addActionListener(new BeobRadio(1));
        jRadioButtonF3.addActionListener(new BeobRadio(2));
        jRadioButtonF4.addActionListener(new BeobRadio(3));
        jRadioButtonF5.addActionListener(new BeobRadio(4));
        jRadioButtonF1.addMouseListener(new BeobMaus(0));
        jRadioButtonF2.addMouseListener(new BeobMaus(1));
        jRadioButtonF3.addMouseListener(new BeobMaus(2));
        jRadioButtonF4.addMouseListener(new BeobMaus(3));
        jRadioButtonF5.addMouseListener(new BeobMaus(4));

        setFilterAnzahl();
        Listener.addListener(new Listener(Listener.EREIGNIS_FILTER_ANZAHL, MVFilterFrame.class.getSimpleName()) {
            @Override
            public void ping() {
                setFilterAnzahl();
            }
        });
        jTextFieldFilterTitel.addMouseListener(new TextCopyPaste());
        jTextFieldFilterThemaTitel.addMouseListener(new TextCopyPaste());

        setToolTip();
//        jButtonFilterLoeschen.setVisible(false); // passt einfach nicht rein??
        pack();
    }

    @Override
    public void setVisible(boolean setvisible) {
        super.setVisible(setvisible);
        setToolTip();
    }

    private void setToolTip() {
        if (MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NAME, 0).isEmpty()) {
            jRadioButtonF1.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 1 speichern");
        } else {
            jRadioButtonF1.setToolTipText(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NAME, 0));
        }

        if (MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NAME, 1).isEmpty()) {
            jRadioButtonF2.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 2 speichern");
        } else {
            jRadioButtonF2.setToolTipText(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NAME, 1));
        }

        if (MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NAME, 2).isEmpty()) {
            jRadioButtonF3.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 3 speichern");
        } else {
            jRadioButtonF3.setToolTipText(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NAME, 2));
        }

        if (MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NAME, 3).isEmpty()) {
            jRadioButtonF4.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 4 speichern");
        } else {
            jRadioButtonF4.setToolTipText(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NAME, 3));
        }

        if (MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NAME, 4).isEmpty()) {
            jRadioButtonF5.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 5 speichern");
        } else {
            jRadioButtonF5.setToolTipText(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NAME, 4));
        }
    }

    private void setIconBlacklist() {
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ON))) {
            //ein
            jToggleButtonBlacklist.setIcon(Icons.ICON_BUTTON_BLACKLIST_EIN);
            jToggleButtonBlacklist.setSelected(true);
        } else {
            //aus
            jToggleButtonBlacklist.setIcon(Icons.ICON_BUTTON_BLACKLIST_AUS);
            jToggleButtonBlacklist.setSelected(false);
        }
    }

    private void setFilterAnzahl() {
        int i;
        try {
            i = Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__ANZAHL_FILTER));
        } catch (Exception ex) {
            MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__ANZAHL_FILTER, String.valueOf(3));
            i = 3;
        }
        jRadioButtonF2.setVisible(i >= 2);
        jRadioButtonF3.setVisible(i >= 3);
        jRadioButtonF4.setVisible(i >= 4);
        jRadioButtonF5.setVisible(i == 5);
    }

    private void setIcon(boolean on) {
        setIcon(on, -1);
    }

    private void setIcon(boolean on, int nr) {
        for (int i = 0; i < 5; ++i) {
            if (on && i == nr) {
                setIconOn(i);
            } else if (aktFilter == i) {
                setAktIcon(i);
            } else {
                setIconOff(i);
            }
        }
    }

    private void setIconOn(int filter) {
        switch (filter) {
            case 0:
                jRadioButtonF1.setIcon(Icons.ICON_FILTER_ON_1);
                break;
            case 1:
                jRadioButtonF2.setIcon(Icons.ICON_FILTER_ON_2);
                break;
            case 2:
                jRadioButtonF3.setIcon(Icons.ICON_FILTER_ON_3);
                break;
            case 3:
                jRadioButtonF4.setIcon(Icons.ICON_FILTER_ON_4);
                break;
            case 4:
                jRadioButtonF5.setIcon(Icons.ICON_FILTER_ON_5);
                break;
        }
    }

    private void setAktIcon(int filter) {
        switch (filter) {
            case 0:
                jRadioButtonF1.setIcon(Icons.ICON_FILTER_AKT_1);
                break;
            case 1:
                jRadioButtonF2.setIcon(Icons.ICON_FILTER_AKT_2);
                break;
            case 2:
                jRadioButtonF3.setIcon(Icons.ICON_FILTER_AKT_3);
                break;
            case 3:
                jRadioButtonF4.setIcon(Icons.ICON_FILTER_AKT_4);
                break;
            case 4:
                jRadioButtonF5.setIcon(Icons.ICON_FILTER_AKT_5);
                break;
        }
    }

    private void setIconOff(int filter) {
        switch (filter) {
            case 0:
                jRadioButtonF1.setIcon(Icons.ICON_FILTER_OFF_1);
                break;
            case 1:
                jRadioButtonF2.setIcon(Icons.ICON_FILTER_OFF_2);
                break;
            case 2:
                jRadioButtonF3.setIcon(Icons.ICON_FILTER_OFF_3);
                break;
            case 3:
                jRadioButtonF4.setIcon(Icons.ICON_FILTER_OFF_4);
                break;
            case 4:
                jRadioButtonF5.setIcon(Icons.ICON_FILTER_OFF_5);
                break;
        }
    }

    private class BeobRadio implements ActionListener {

        int filter;

        public BeobRadio(int f) {
            filter = f;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            aktFilter = filter;
            mvFfilter(filter);
        }
    }

    public class BeobMaus extends MouseAdapter {

        int filter;
        JRadioButtonMenuItem r1 = new JRadioButtonMenuItem("Blacklist für Filter einschalten");
        JRadioButtonMenuItem r2 = new JRadioButtonMenuItem("Blacklist für Filter ausschalten");

        public BeobMaus(int f) {
            filter = f;
        }

        @Override
        public void mousePressed(MouseEvent arg0) {
            setIcon(true, filter);
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        @Override
        public void mouseReleased(MouseEvent arg0) {
            setIcon(false);
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        private class BeobRa implements ActionListener {

            int filter;

            public BeobRa(int f) {
                filter = f;
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                if (r1.isSelected()) {
                    MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__BLACKLIST_ON, Boolean.TRUE.toString(), filter);
                } else if (r2.isSelected()) {
                    MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__BLACKLIST_ON, Boolean.FALSE.toString(), filter);
                }
            }
        }

        private void showMenu(MouseEvent evt) {
            final JPopupMenu jPopupMenu = new JPopupMenu();
            final JTextField name = new JTextField("");
            final JSpinner jSpinner;
            JPanel pName = new JPanel(new FlowLayout());
            JLabel lbl = new JLabel("Name: ");
            name.setMinimumSize(new Dimension(100, name.getMinimumSize().height));
            name.setPreferredSize(new Dimension(150, name.getPreferredSize().height));
            name.setText(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NAME, filter));
            name.getDocument().addDocumentListener(new DocumentListener() {

                @Override
                public void insertUpdate(DocumentEvent e) {
                    MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NAME, name.getText(), filter);
                }

                @Override
                public void removeUpdate(DocumentEvent e) {
                    MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NAME, name.getText(), filter);
                }

                @Override
                public void changedUpdate(DocumentEvent e) {
                    MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__NAME, name.getText(), filter);
                }
            });
            name.addActionListener(e -> jPopupMenu.setVisible(false));
            pName.add(lbl);
            pName.add(name);
            jPopupMenu.add(pName);
            //##Trenner##
            jPopupMenu.addSeparator();

            JMenuItem item = new JMenuItem("Filterprofil speichern");
            item.setIcon(Icons.ICON_MENUE_FILTER_SPEICHERN);
            item.addActionListener(e -> mvFsaveFilter(filter));
            jPopupMenu.add(item);
            item = new JMenuItem("Filterprofil löschen");
            item.setIcon(Icons.ICON_MENUE_FILTER_LOESCHEN);
            item.addActionListener(e -> mvFdeleteFilter(filter));
            jPopupMenu.add(item);
            //##Trenner##
            jPopupMenu.addSeparator();

            // Blacklist
            ButtonGroup bG = new ButtonGroup();
            bG.add(r1);
            bG.add(r2);
            jPopupMenu.add(r1);
            jPopupMenu.add(r2);
            if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__BLACKLIST_ON, filter))) {
                r1.setSelected(true);
            } else {
                r2.setSelected(true);
            }
            r1.addActionListener(new BeobRa(filter));
            r2.addActionListener(new BeobRa(filter));
            //##Trenner##
            jPopupMenu.addSeparator();

            JPanel p = new JPanel(new FlowLayout());
            jSpinner = new JSpinner(new SpinnerNumberModel(1, 1, 5, 1));
            int i;
            try {
                i = Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_PROFILE__ANZAHL_FILTER));
            } catch (Exception ex) {
                MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__ANZAHL_FILTER, String.valueOf(3));
                i = 3;
            }
            jSpinner.setValue(i);
            jSpinner.addChangeListener(e -> {
                MVConfig.add(MVConfig.Configs.SYSTEM_FILTER_PROFILE__ANZAHL_FILTER, String.valueOf(((Number) jSpinner.getModel().getValue()).intValue()));
                setFilterAnzahl();
                Listener.notify(Listener.EREIGNIS_FILTER_ANZAHL, MVFilterFrame.class.getSimpleName());
            });
            JLabel label = new JLabel("Anzahl Filter anzeigen:");
            p.add(label);
            p.add(jSpinner);
            jPopupMenu.add(p);

            jPopupMenu.addPopupMenuListener(new PopupMenuListener() {

                @Override
                public void popupMenuCanceled(PopupMenuEvent popupMenuEvent) {
                    setIcon(false);
                    setToolTip();
                }

                @Override
                public void popupMenuWillBecomeInvisible(PopupMenuEvent popupMenuEvent) {
                    setIcon(false);
                    setToolTip();
                }

                @Override
                public void popupMenuWillBecomeVisible(PopupMenuEvent popupMenuEvent) {
                }
            }
            );
            //anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }
    }

    public class BeobMausBlacklist extends MouseAdapter {

        @Override
        public void mousePressed(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        @Override
        public void mouseReleased(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        private void showMenu(MouseEvent evt) {
            final JPopupMenu jPopupMenu = new JPopupMenu();

            JMenuItem item = new JMenuItem("Blacklist bearbeiten");
            item.addActionListener(e -> {
                DialogLeer dialog = new DialogLeer(parent, true);
                dialog.init("Blacklist", new PanelBlacklist(daten, parent, PanelBlacklist.class.getName() + "_4"));
                dialog.setVisible(true);
            });
            jPopupMenu.add(item);

            //anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }
    }

    @Override
    public void dispose() {
        GuiFunktionen.getSize(MVConfig.Configs.SYSTEM_GROESSE_FILTER, this);
        MVConfig.add(MVConfig.Configs.SYSTEM_VIS_FILTER, Boolean.FALSE.toString());
        Listener.notify(Listener.EREIGNIS_PANEL_FILTER_ANZEIGEN, MVFilterFrame.class.getName());
        super.dispose();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.ButtonGroup buttonGroup1 = new javax.swing.ButtonGroup();
        javax.swing.ButtonGroup buttonGroup2 = new javax.swing.ButtonGroup();
        buttonGroup3 = new javax.swing.ButtonGroup();
        javax.swing.JPanel jPanel3 = new javax.swing.JPanel();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        jTextFieldFilterMinuten = new javax.swing.JTextField();
        jSliderMinuten = new javax.swing.JSlider();
        jToggleButtonLivestram = new javax.swing.JToggleButton();
        jCheckBoxKeineGesehenen = new javax.swing.JCheckBox();
        jCheckBoxKeineAbos = new javax.swing.JCheckBox();
        jCheckBoxNurHd = new javax.swing.JCheckBox();
        jToggleButtonHistory = new javax.swing.JToggleButton();
        jCheckBoxNurNeue = new javax.swing.JCheckBox();
        jTextFieldFilterTage = new javax.swing.JTextField();
        jSliderTage = new javax.swing.JSlider();
        chkUt = new javax.swing.JCheckBox();
        rbMin = new javax.swing.JRadioButton();
        rbMax = new javax.swing.JRadioButton();
        jLabel3 = new javax.swing.JLabel();
        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel2 = new javax.swing.JLabel();
        jComboBoxFilterSender = new javax.swing.JComboBox<>();
        javax.swing.JLabel jLabel4 = new javax.swing.JLabel();
        jComboBoxFilterThema = new javax.swing.JComboBox<>();
        javax.swing.JLabel jLabel5 = new javax.swing.JLabel();
        jTextFieldFilterTitel = new javax.swing.JTextField();
        jTextFieldFilterThemaTitel = new javax.swing.JTextField();
        jRadioButtonTT = new javax.swing.JRadioButton();
        jRadioButtonIrgendwo = new javax.swing.JRadioButton();
        jButtonFilterLoeschen = new javax.swing.JButton();
        jButtonHilfe = new javax.swing.JButton();
        jButtonOk = new javax.swing.JButton();
        jRadioButtonF1 = new javax.swing.JRadioButton();
        jRadioButtonF2 = new javax.swing.JRadioButton();
        jRadioButtonF3 = new javax.swing.JRadioButton();
        jRadioButtonF4 = new javax.swing.JRadioButton();
        jRadioButtonF5 = new javax.swing.JRadioButton();
        javax.swing.JLabel jLabel6 = new javax.swing.JLabel();
        jToggleButtonBlacklist = new javax.swing.JToggleButton();
        jButtonClearAll = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
        setUndecorated(true);
        setResizable(false);

        jPanel3.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(102, 102, 102), 2));

        jPanel1.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));

        jLabel1.setText("Zeitraum:");

        jTextFieldFilterMinuten.setEditable(false);

        jToggleButtonLivestram.setText("Livestreams");

        jCheckBoxKeineGesehenen.setText("gesehene ausblenden");

        jCheckBoxKeineAbos.setText("Abos nicht anzeigen");

        jCheckBoxNurHd.setText("HD");
        jCheckBoxNurHd.setToolTipText("nur Filme in HD anzeigen");

        jToggleButtonHistory.setText("aktuelle History");

        jCheckBoxNurNeue.setText("Neue");
        jCheckBoxNurNeue.setToolTipText("nur neue Filme anzeigen");

        jTextFieldFilterTage.setEditable(false);

        jSliderTage.setMaximum(30);
        jSliderTage.setValue(15);

        chkUt.setText("UT");
        chkUt.setToolTipText("nur Filme mit Untertitel anzeigen");

        buttonGroup3.add(rbMin);
        rbMin.setText("Mindestlänge [min]");

        buttonGroup3.add(rbMax);
        rbMax.setText("Maximallänge");

        jLabel3.setText("nur:");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                            .addComponent(jSliderMinuten, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jSliderTage, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addGroup(javax.swing.GroupLayout.Alignment.LEADING, jPanel1Layout.createSequentialGroup()
                                .addComponent(jLabel1)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addComponent(jTextFieldFilterTage, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                            .addGroup(javax.swing.GroupLayout.Alignment.LEADING, jPanel1Layout.createSequentialGroup()
                                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                                    .addComponent(jToggleButtonLivestram, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(jToggleButtonHistory, javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jCheckBoxKeineGesehenen, javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jCheckBoxKeineAbos, javax.swing.GroupLayout.Alignment.LEADING)
                                    .addGroup(javax.swing.GroupLayout.Alignment.LEADING, jPanel1Layout.createSequentialGroup()
                                        .addComponent(jLabel3)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(jCheckBoxNurHd)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(chkUt)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(jCheckBoxNurNeue)))
                                .addGap(0, 0, Short.MAX_VALUE))))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(rbMin)
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(rbMax)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jTextFieldFilterMinuten, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addContainerGap())
        );

        jPanel1Layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jToggleButtonHistory, jToggleButtonLivestram});

        jPanel1Layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jTextFieldFilterMinuten, jTextFieldFilterTage});

        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jTextFieldFilterTage, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jSliderTage, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(rbMin)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(rbMax)
                    .addComponent(jTextFieldFilterMinuten, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jSliderMinuten, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jCheckBoxKeineGesehenen)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jCheckBoxKeineAbos)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jCheckBoxNurHd)
                    .addComponent(chkUt)
                    .addComponent(jCheckBoxNurNeue)
                    .addComponent(jLabel3))
                .addGap(18, 18, 18)
                .addComponent(jToggleButtonLivestram)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jToggleButtonHistory)
                .addContainerGap(14, Short.MAX_VALUE))
        );

        jPanel2.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));

        jLabel2.setText("Sender:");

        jComboBoxFilterSender.setMaximumRowCount(25);

        jLabel4.setText("Thema:");

        jComboBoxFilterThema.setMaximumRowCount(20);

        jLabel5.setText("Titel:");

        buttonGroup2.add(jRadioButtonTT);
        jRadioButtonTT.setSelected(true);
        jRadioButtonTT.setText("Thema / Titel:");

        buttonGroup2.add(jRadioButtonIrgendwo);
        jRadioButtonIrgendwo.setText("irgendwo:");

        jButtonFilterLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-clear.png"))); // NOI18N
        jButtonFilterLoeschen.setToolTipText("Filter löschen");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addGap(16, 16, 16)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                        .addComponent(jLabel5)
                        .addComponent(jLabel4)
                        .addComponent(jRadioButtonTT)
                        .addComponent(jRadioButtonIrgendwo, javax.swing.GroupLayout.Alignment.LEADING)
                        .addComponent(jButtonFilterLoeschen, javax.swing.GroupLayout.Alignment.LEADING))
                    .addComponent(jLabel2))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jComboBoxFilterSender, 0, 315, Short.MAX_VALUE)
                    .addComponent(jComboBoxFilterThema, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jTextFieldFilterTitel)
                    .addComponent(jTextFieldFilterThemaTitel))
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel2)
                    .addComponent(jComboBoxFilterSender, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(14, 14, 14)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel4)
                    .addComponent(jComboBoxFilterThema, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel5)
                    .addComponent(jTextFieldFilterTitel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(18, 18, 18)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jTextFieldFilterThemaTitel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jRadioButtonTT))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jRadioButtonIrgendwo)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(jButtonFilterLoeschen)
                .addContainerGap())
        );

        jPanel2Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jComboBoxFilterSender, jComboBoxFilterThema, jTextFieldFilterThemaTitel, jTextFieldFilterTitel});

        jButtonHilfe.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-help.png"))); // NOI18N
        jButtonHilfe.setToolTipText("Hilfe anzeigen");

        jButtonOk.setText("OK");

        buttonGroup1.add(jRadioButtonF1);
        jRadioButtonF1.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 1 speichern");

        buttonGroup1.add(jRadioButtonF2);
        jRadioButtonF2.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 2 speichern");

        buttonGroup1.add(jRadioButtonF3);
        jRadioButtonF3.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 3 speichern");

        buttonGroup1.add(jRadioButtonF4);
        jRadioButtonF4.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 4 speichern");

        buttonGroup1.add(jRadioButtonF5);
        jRadioButtonF5.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 5 speichern");

        jLabel6.setText("Filterprofile:");

        jToggleButtonBlacklist.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-blacklist-aus.png"))); // NOI18N
        jToggleButtonBlacklist.setToolTipText("Blacklist ein/aus (Rechtsklick um Blacklist zu bearbeiten)");

        jButtonClearAll.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-clear.png"))); // NOI18N
        jButtonClearAll.setToolTipText("Alles löschen");

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addComponent(jLabel6)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jRadioButtonF1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jRadioButtonF2)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jRadioButtonF3)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jRadioButtonF4)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jRadioButtonF5)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jButtonHilfe)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jToggleButtonBlacklist)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonClearAll)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonOk)))
                .addContainerGap())
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel6)
                    .addComponent(jRadioButtonF1)
                    .addComponent(jRadioButtonF2)
                    .addComponent(jRadioButtonF3)
                    .addComponent(jRadioButtonF4)
                    .addComponent(jRadioButtonF5)
                    .addComponent(jButtonOk)
                    .addComponent(jButtonHilfe)
                    .addComponent(jToggleButtonBlacklist)
                    .addComponent(jButtonClearAll))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroup3;
    private javax.swing.JCheckBox chkUt;
    private javax.swing.JButton jButtonClearAll;
    public javax.swing.JButton jButtonFilterLoeschen;
    public javax.swing.JButton jButtonHilfe;
    private javax.swing.JButton jButtonOk;
    public javax.swing.JCheckBox jCheckBoxKeineAbos;
    public javax.swing.JCheckBox jCheckBoxKeineGesehenen;
    public javax.swing.JCheckBox jCheckBoxNurHd;
    private javax.swing.JCheckBox jCheckBoxNurNeue;
    public javax.swing.JComboBox<String> jComboBoxFilterSender;
    public javax.swing.JComboBox<String> jComboBoxFilterThema;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JRadioButton jRadioButtonF1;
    private javax.swing.JRadioButton jRadioButtonF2;
    private javax.swing.JRadioButton jRadioButtonF3;
    private javax.swing.JRadioButton jRadioButtonF4;
    private javax.swing.JRadioButton jRadioButtonF5;
    private javax.swing.JRadioButton jRadioButtonIrgendwo;
    private javax.swing.JRadioButton jRadioButtonTT;
    public javax.swing.JSlider jSliderMinuten;
    private javax.swing.JSlider jSliderTage;
    public javax.swing.JTextField jTextFieldFilterMinuten;
    private javax.swing.JTextField jTextFieldFilterTage;
    public javax.swing.JTextField jTextFieldFilterThemaTitel;
    public javax.swing.JTextField jTextFieldFilterTitel;
    private javax.swing.JToggleButton jToggleButtonBlacklist;
    private javax.swing.JToggleButton jToggleButtonHistory;
    public javax.swing.JToggleButton jToggleButtonLivestram;
    private javax.swing.JRadioButton rbMax;
    private javax.swing.JRadioButton rbMin;
    // End of variables declaration//GEN-END:variables

    @Override
    public void mvFdeleteFilter(int i) {
    }

    @Override
    public void mvFsaveFilter(int i) {
    }

    @Override
    public void mvFfilter(int i) {
    }

    @Override
    public void enableFilter(boolean enable) {
        jRadioButtonF1.setEnabled(enable);
        jRadioButtonF2.setEnabled(enable);
        jRadioButtonF3.setEnabled(enable);
        jRadioButtonF4.setEnabled(enable);
        jRadioButtonF5.setEnabled(enable);
    }

    @Override
    public JButton get_jButtonFilterLoeschen() {
        return jButtonFilterLoeschen;
    }

    @Override
    public JButton get_jButtonClearAll() {
        return jButtonClearAll;
    }

    @Override
    public JCheckBox get_jCheckBoxKeineAbos() {
        return jCheckBoxKeineAbos;
    }

    @Override
    public JCheckBox get_jCheckBoxKeineGesehenen() {
        return jCheckBoxKeineGesehenen;
    }

    @Override
    public JCheckBox get_jCheckBoxNurHd() {
        return jCheckBoxNurHd;
    }

    @Override
    public JCheckBox get_jCheckBoxNurUt() {
        return chkUt;
    }

    @Override
    public JComboBox<String> get_jComboBoxFilterSender() {
        return jComboBoxFilterSender;
    }

    @Override
    public boolean getThemaTitel() {
        return jRadioButtonTT.isSelected();
    }

    @Override
    public void setThemaTitel(boolean set) {
        jRadioButtonTT.setSelected(set);
        jRadioButtonIrgendwo.setSelected(!set);
    }

    @Override
    public JRadioButton get_jRadioButtonTT() {
        return jRadioButtonTT;
    }

    @Override
    public JRadioButton get_JRadioButtonIrgendwo() {
        return jRadioButtonIrgendwo;
    }

    @Override
    public JComboBox<String> get_jComboBoxFilterThema() {
        return jComboBoxFilterThema;
    }

    @Override
    public JSlider get_jSliderTage() {
        return jSliderTage;
    }

    @Override
    public JTextField get_jTextFieldFilterTage() {
        return jTextFieldFilterTage;
    }

    @Override
    public JSlider get_jSliderMinuten() {
        return jSliderMinuten;
    }

    @Override
    public JRadioButton get_rbMin() {
        return rbMin;
    }
    @Override
    public JRadioButton get_rbMax() {
        return rbMax;
    }

    @Override
    public JTextField get_jTextFieldFilterMinuten() {
        return jTextFieldFilterMinuten;
    }

    @Override
    public JTextField get_jTextFieldFilterThemaTitel() {
        return jTextFieldFilterThemaTitel;
    }

    @Override
    public JTextField get_jTextFieldFilterTitel() {
        return jTextFieldFilterTitel;
    }

    @Override
    public JToggleButton get_jToggleButtonLivestram() {
        return jToggleButtonLivestram;
    }

    @Override
    public JCheckBox get_jCheckBoxNeue() {
        return jCheckBoxNurNeue;
    }

    @Override
    public JToggleButton get_jToggleButtonHistory() {
        return jToggleButtonHistory;
    }

    @Override
    public void removeAllListener() {
        aktFilter = -1;
        setIcon(false); // erst mal alle aus
        for (ActionListener a : jButtonFilterLoeschen.getActionListeners()) {
            jButtonFilterLoeschen.removeActionListener(a);
        }
        for (ActionListener a : jButtonClearAll.getActionListeners()) {
            jButtonClearAll.removeActionListener(a);
        }
        for (ActionListener a : jCheckBoxKeineAbos.getActionListeners()) {
            jCheckBoxKeineAbos.removeActionListener(a);
        }
        for (ActionListener a : jCheckBoxKeineGesehenen.getActionListeners()) {
            jCheckBoxKeineGesehenen.removeActionListener(a);
        }
        for (ActionListener a : jCheckBoxNurHd.getActionListeners()) {
            jCheckBoxNurHd.removeActionListener(a);
        }
        for (ActionListener a : jComboBoxFilterSender.getActionListeners()) {
            jComboBoxFilterSender.removeActionListener(a);
        }
        for (ActionListener a : jComboBoxFilterThema.getActionListeners()) {
            jComboBoxFilterThema.removeActionListener(a);
        }
        for (ActionListener a : jTextFieldFilterTage.getActionListeners()) {
            jTextFieldFilterTage.removeActionListener(a);
        }
        for (ChangeListener a : jSliderTage.getChangeListeners()) {
            jSliderTage.removeChangeListener(a);
        }
        for (ActionListener a : rbMax.getActionListeners()) {
            rbMax.removeActionListener(a);
        }
        for (ActionListener a : rbMin.getActionListeners()) {
            rbMin.removeActionListener(a);
        }
        for (ChangeListener a : jSliderMinuten.getChangeListeners()) {
            jSliderMinuten.removeChangeListener(a);
        }
        for (ActionListener a : jTextFieldFilterMinuten.getActionListeners()) {
            jTextFieldFilterMinuten.removeActionListener(a);
        }
        for (ActionListener a : jTextFieldFilterThemaTitel.getActionListeners()) {
            jTextFieldFilterThemaTitel.removeActionListener(a);
        }
        for (ActionListener a : jTextFieldFilterTitel.getActionListeners()) {
            jTextFieldFilterTitel.removeActionListener(a);
        }
        for (ActionListener a : jToggleButtonLivestram.getActionListeners()) {
            jToggleButtonLivestram.removeActionListener(a);
        }
        for (ActionListener a : jCheckBoxNurNeue.getActionListeners()) {
            jCheckBoxNurNeue.removeActionListener(a);
        }
        for (ActionListener a : jToggleButtonHistory.getActionListeners()) {
            jToggleButtonHistory.removeActionListener(a);
        }
    }

}
