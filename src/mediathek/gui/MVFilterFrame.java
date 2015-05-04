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
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.ButtonGroup;
import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButton;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JSlider;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.KeyStroke;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import mediathek.MediathekGui;
import mediathek.daten.Daten;
import mediathek.file.GetFile;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.gui.dialog.DialogLeer;
import mediathek.gui.dialogEinstellungen.PanelBlacklist;
import mediathek.res.GetIcon;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVConfig;
import mediathek.tool.MVListeFilme;

public class MVFilterFrame extends javax.swing.JFrame implements MVFilter {

    Daten daten;
    static Point mouseDownCompCoords;
    JFrame parent;
    private int aktFilter = -1;

    public MVFilterFrame(Daten d) {
        initComponents();
        parent = this;
        daten = d;

        if (SystemInfo.isWindows()) {
            // zum Abfangen der Win-F4 für comboboxen
            InputMap im = jComboBoxFilterSender.getInputMap();
            im.put(KeyStroke.getKeyStroke(KeyEvent.VK_F4, 0), "einstellungen");
            ActionMap am = jComboBoxFilterSender.getActionMap();
            am.put("einstellungen", new AbstractAction() {

                @Override
                public void actionPerformed(ActionEvent e) {
                    daten.mediathekGui.showDialogPreferences();
                }
            });
            im = jComboBoxFilterThema.getInputMap();
            im.put(KeyStroke.getKeyStroke(KeyEvent.VK_F4, 0), "einstellungen");
            am = jComboBoxFilterThema.getActionMap();
            am.put("einstellungen", new AbstractAction() {

                @Override
                public void actionPerformed(ActionEvent e) {
                    daten.mediathekGui.showDialogPreferences();
                }
            });
            im = jComboBoxZeitraum.getInputMap();
            im.put(KeyStroke.getKeyStroke(KeyEvent.VK_F4, 0), "einstellungen");
            am = jComboBoxZeitraum.getActionMap();
            am.put("einstellungen", new AbstractAction() {

                @Override
                public void actionPerformed(ActionEvent e) {
                    daten.mediathekGui.showDialogPreferences();
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
        this.setIconImage(Toolkit.getDefaultToolkit().getImage(MediathekGui.class.getResource("/mediathek/res/MediathekView_k.gif")));
        this.setTitle("Filter");
        GuiFunktionen.setSize(MVConfig.SYSTEM_GROESSE_FILTER, this, daten.mediathekGui);
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent evt) {
                dispose();
            }
        });
        jButtonFilterLoeschen.setIcon(GetIcon.getProgramIcon("clear_16.png"));
        jButtonOk.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                dispose();
            }
        });

        setIconBlacklist();
        jToggleButtonBlacklist.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_BLACKLIST_ON, Boolean.toString(jToggleButtonBlacklist.isSelected()));
                MVListeFilme.checkBlacklist();
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_BLACKLIST_GEAENDERT, MVFilterFrame.class.getSimpleName());
                setIconBlacklist();
            }
        });
        jToggleButtonBlacklist.addMouseListener(new BeobMausBlacklist());
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_BLACKLIST_GEAENDERT, MVFilterFrame.class.getSimpleName()) {
            @Override
            public void ping() {
                setIconBlacklist();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_BLACKLIST_GEAENDERT, MVFilterFrame.class.getSimpleName()) {
            @Override
            public void ping() {
                setIconBlacklist();
            }
        });

        jButtonHilfe.setIcon(GetIcon.getProgramIcon("help_16.png"));
        jButtonHilfe.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                new DialogHilfe(parent, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_FILTER)).setVisible(true);
            }
        });
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
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_FILTER_ANZAHL, MVFilterFrame.class.getSimpleName()) {
            @Override
            public void ping() {
                setFilterAnzahl();
            }
        });

        setToolTip();
        pack();
    }

    @Override
    public void setVisible(boolean setvisible) {
        super.setVisible(setvisible);
        setToolTip();
    }

    private void setToolTip() {
        if (Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__NAME, 0).isEmpty()) {
            jRadioButtonF1.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 1 speichern");
        } else {
            jRadioButtonF1.setToolTipText(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__NAME, 0));
        }

        if (Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__NAME, 1).isEmpty()) {
            jRadioButtonF2.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 2 speichern");
        } else {
            jRadioButtonF2.setToolTipText(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__NAME, 1));
        }

        if (Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__NAME, 2).isEmpty()) {
            jRadioButtonF3.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 3 speichern");
        } else {
            jRadioButtonF3.setToolTipText(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__NAME, 2));
        }

        if (Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__NAME, 3).isEmpty()) {
            jRadioButtonF4.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 4 speichern");
        } else {
            jRadioButtonF4.setToolTipText(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__NAME, 3));
        }

        if (Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__NAME, 4).isEmpty()) {
            jRadioButtonF5.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 5 speichern");
        } else {
            jRadioButtonF5.setToolTipText(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__NAME, 4));
        }
    }

    private void setIconBlacklist() {
        if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_BLACKLIST_ON))) {
            //ein
            jToggleButtonBlacklist.setIcon(GetIcon.getProgramIcon("blacklist_ein_16.png"));
            jToggleButtonBlacklist.setSelected(true);
        } else {
            //aus
            jToggleButtonBlacklist.setIcon(GetIcon.getProgramIcon("blacklist_aus_16.png"));
            jToggleButtonBlacklist.setSelected(false);
        }
    }

    private void setFilterAnzahl() {
        int i;
        try {
            i = Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__ANZAHL_FILTER));
        } catch (Exception ex) {
            Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__ANZAHL_FILTER, String.valueOf(3));
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
                jRadioButtonF1.setIcon(GetIcon.getProgramIcon("filter_on_1.png"));
                break;
            case 1:
                jRadioButtonF2.setIcon(GetIcon.getProgramIcon("filter_on_2.png"));
                break;
            case 2:
                jRadioButtonF3.setIcon(GetIcon.getProgramIcon("filter_on_3.png"));
                break;
            case 3:
                jRadioButtonF4.setIcon(GetIcon.getProgramIcon("filter_on_4.png"));
                break;
            case 4:
                jRadioButtonF5.setIcon(GetIcon.getProgramIcon("filter_on_5.png"));
                break;
        }
    }

    private void setAktIcon(int filter) {
        switch (filter) {
            case 0:
                jRadioButtonF1.setIcon(GetIcon.getProgramIcon("filter_akt_1.png"));
                break;
            case 1:
                jRadioButtonF2.setIcon(GetIcon.getProgramIcon("filter_akt_2.png"));
                break;
            case 2:
                jRadioButtonF3.setIcon(GetIcon.getProgramIcon("filter_akt_3.png"));
                break;
            case 3:
                jRadioButtonF4.setIcon(GetIcon.getProgramIcon("filter_akt_4.png"));
                break;
            case 4:
                jRadioButtonF5.setIcon(GetIcon.getProgramIcon("filter_akt_5.png"));
                break;
        }
    }

    private void setIconOff(int filter) {
        switch (filter) {
            case 0:
                jRadioButtonF1.setIcon(GetIcon.getProgramIcon("filter_off_1.png"));
                break;
            case 1:
                jRadioButtonF2.setIcon(GetIcon.getProgramIcon("filter_off_2.png"));
                break;
            case 2:
                jRadioButtonF3.setIcon(GetIcon.getProgramIcon("filter_off_3.png"));
                break;
            case 3:
                jRadioButtonF4.setIcon(GetIcon.getProgramIcon("filter_off_4.png"));
                break;
            case 4:
                jRadioButtonF5.setIcon(GetIcon.getProgramIcon("filter_off_5.png"));
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
                    Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__BLACKLIST_ON, Boolean.TRUE.toString(), filter, MVFilter.MAX_FILTER);
                } else if (r2.isSelected()) {
                    Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__BLACKLIST_ON, Boolean.FALSE.toString(), filter, MVFilter.MAX_FILTER);
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
            name.setText(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__NAME, filter));
            name.getDocument().addDocumentListener(new DocumentListener() {

                @Override
                public void insertUpdate(DocumentEvent e) {
                    Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__NAME, name.getText(), filter, MVFilter.MAX_FILTER);
                }

                @Override
                public void removeUpdate(DocumentEvent e) {
                    Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__NAME, name.getText(), filter, MVFilter.MAX_FILTER);
                }

                @Override
                public void changedUpdate(DocumentEvent e) {
                    Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__NAME, name.getText(), filter, MVFilter.MAX_FILTER);
                }
            });
            name.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent e) {
                    jPopupMenu.setVisible(false);
                }
            });
            pName.add(lbl);
            pName.add(name);
            jPopupMenu.add(pName);
            //##Trenner##
            jPopupMenu.addSeparator();

            JMenuItem item = new JMenuItem("Filterprofil speichern");
            item.setIcon(GetIcon.getProgramIcon("filter_speichern_16.png"));
            item.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    mvFsaveFilter(filter);
                }
            });
            jPopupMenu.add(item);
            item = new JMenuItem("Filterprofil löschen");
            item.setIcon(GetIcon.getProgramIcon("filter_loeschen_16.png"));
            item.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    mvFdeleteFilter(filter);
                }
            });
            jPopupMenu.add(item);
            //##Trenner##
            jPopupMenu.addSeparator();

            // Blacklist
            ButtonGroup bG = new ButtonGroup();
            bG.add(r1);
            bG.add(r2);
            jPopupMenu.add(r1);
            jPopupMenu.add(r2);
            if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__BLACKLIST_ON, filter))) {
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
                i = Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_FILTER_PROFILE__ANZAHL_FILTER));
            } catch (Exception ex) {
                Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__ANZAHL_FILTER, String.valueOf(3));
                i = 3;
            }
            jSpinner.setValue(i);
            jSpinner.addChangeListener(new ChangeListener() {

                @Override
                public void stateChanged(ChangeEvent e) {
                    Daten.mVConfig.add(MVConfig.SYSTEM_FILTER_PROFILE__ANZAHL_FILTER, String.valueOf(((Number) jSpinner.getModel().getValue()).intValue()));
                    setFilterAnzahl();
                    ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_FILTER_ANZAHL, MVFilterFrame.class.getSimpleName());
                }
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
            item.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    DialogLeer dialog = new DialogLeer(parent, true);
                    dialog.init("Blacklist", new PanelBlacklist(daten, parent, PanelBlacklist.class.getName() + "_4"));
                    dialog.setVisible(true);
                }
            });
            jPopupMenu.add(item);

            //anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }
    }

    @Override
    public void dispose() {
        GuiFunktionen.getSize(MVConfig.SYSTEM_GROESSE_FILTER, this);
        Daten.mVConfig.add(MVConfig.SYSTEM_VIS_FILTER, Boolean.FALSE.toString());
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_PANEL_FILTER_ANZEIGEN, MVFilterFrame.class.getName());
        super.dispose();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.ButtonGroup buttonGroup1 = new javax.swing.ButtonGroup();
        javax.swing.ButtonGroup buttonGroup2 = new javax.swing.ButtonGroup();
        javax.swing.JPanel jPanel3 = new javax.swing.JPanel();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        jComboBoxZeitraum = new javax.swing.JComboBox<String>();
        javax.swing.JLabel jLabel3 = new javax.swing.JLabel();
        jTextFieldFilterMinuten = new javax.swing.JTextField();
        jSliderMinuten = new javax.swing.JSlider();
        jToggleButtonLivestram = new javax.swing.JToggleButton();
        jCheckBoxKeineGesehenen = new javax.swing.JCheckBox();
        jCheckBoxKeineAbos = new javax.swing.JCheckBox();
        jCheckBoxNurHd = new javax.swing.JCheckBox();
        jToggleButtonHistory = new javax.swing.JToggleButton();
        jCheckBoxNurNeue = new javax.swing.JCheckBox();
        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel2 = new javax.swing.JLabel();
        jComboBoxFilterSender = new javax.swing.JComboBox<String>();
        javax.swing.JLabel jLabel4 = new javax.swing.JLabel();
        jComboBoxFilterThema = new javax.swing.JComboBox<String>();
        javax.swing.JLabel jLabel5 = new javax.swing.JLabel();
        jTextFieldFilterTitel = new javax.swing.JTextField();
        jTextFieldFilterThemaTitel = new javax.swing.JTextField();
        jRadioButtonTT = new javax.swing.JRadioButton();
        jRadioButtonIrgendwo = new javax.swing.JRadioButton();
        jButtonHilfe = new javax.swing.JButton();
        jButtonOk = new javax.swing.JButton();
        jRadioButtonF1 = new javax.swing.JRadioButton();
        jRadioButtonF2 = new javax.swing.JRadioButton();
        jRadioButtonF3 = new javax.swing.JRadioButton();
        jRadioButtonF4 = new javax.swing.JRadioButton();
        jRadioButtonF5 = new javax.swing.JRadioButton();
        javax.swing.JLabel jLabel6 = new javax.swing.JLabel();
        jButtonFilterLoeschen = new javax.swing.JButton();
        jToggleButtonBlacklist = new javax.swing.JToggleButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
        setUndecorated(true);
        setResizable(false);

        jPanel3.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(102, 102, 102), 2));

        jPanel1.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));

        jLabel1.setText("Zeitraum:");

        jLabel3.setText("Mindestlänge [min]:");

        jTextFieldFilterMinuten.setEditable(false);

        jToggleButtonLivestram.setText("Livestreams");

        jCheckBoxKeineGesehenen.setText("gesehene ausblenden");

        jCheckBoxKeineAbos.setText("Abos nicht anzeigen");

        jCheckBoxNurHd.setText("nur HD anzeigen");

        jToggleButtonHistory.setText("aktuelle History");

        jCheckBoxNurNeue.setText("nur neue");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jComboBoxZeitraum, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jLabel3)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jTextFieldFilterMinuten, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jCheckBoxNurNeue)
                            .addComponent(jCheckBoxNurHd)
                            .addComponent(jCheckBoxKeineAbos)
                            .addComponent(jCheckBoxKeineGesehenen)
                            .addComponent(jLabel1)
                            .addComponent(jSliderMinuten, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jToggleButtonLivestram, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jToggleButtonHistory))
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );

        jPanel1Layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jToggleButtonHistory, jToggleButtonLivestram});

        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jComboBoxZeitraum, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel3)
                    .addComponent(jTextFieldFilterMinuten, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jSliderMinuten, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jToggleButtonLivestram)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jToggleButtonHistory)
                .addGap(14, 14, 14)
                .addComponent(jCheckBoxKeineGesehenen)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jCheckBoxKeineAbos)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jCheckBoxNurHd)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jCheckBoxNurNeue)
                .addContainerGap(12, Short.MAX_VALUE))
        );

        jPanel1Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jComboBoxZeitraum, jTextFieldFilterMinuten});

        jPanel2.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));

        jLabel2.setText("Sender:");

        jComboBoxFilterSender.setMaximumRowCount(20);

        jLabel4.setText("Thema:");

        jComboBoxFilterThema.setMaximumRowCount(20);

        jLabel5.setText("Titel:");

        buttonGroup2.add(jRadioButtonTT);
        jRadioButtonTT.setSelected(true);
        jRadioButtonTT.setText("Thema / Titel:");

        buttonGroup2.add(jRadioButtonIrgendwo);
        jRadioButtonIrgendwo.setText("irgendwo:");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addGap(16, 16, 16)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jLabel5)
                    .addComponent(jLabel4)
                    .addComponent(jLabel2)
                    .addComponent(jRadioButtonTT)
                    .addComponent(jRadioButtonIrgendwo, javax.swing.GroupLayout.Alignment.LEADING))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jComboBoxFilterSender, 0, 360, Short.MAX_VALUE)
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
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
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
                .addContainerGap(144, Short.MAX_VALUE))
        );

        jPanel2Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jComboBoxFilterSender, jComboBoxFilterThema, jTextFieldFilterThemaTitel, jTextFieldFilterTitel});

        jButtonHilfe.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/help_16.png"))); // NOI18N
        jButtonHilfe.setToolTipText("Hilfe");

        jButtonOk.setText("OK");

        buttonGroup1.add(jRadioButtonF1);
        jRadioButtonF1.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 1 speichern");
        jRadioButtonF1.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/filter_off_1.png"))); // NOI18N

        buttonGroup1.add(jRadioButtonF2);
        jRadioButtonF2.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 2 speichern");
        jRadioButtonF2.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/filter_off_2.png"))); // NOI18N

        buttonGroup1.add(jRadioButtonF3);
        jRadioButtonF3.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 3 speichern");
        jRadioButtonF3.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/filter_off_3.png"))); // NOI18N

        buttonGroup1.add(jRadioButtonF4);
        jRadioButtonF4.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 4 speichern");
        jRadioButtonF4.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/filter_off_4.png"))); // NOI18N

        buttonGroup1.add(jRadioButtonF5);
        jRadioButtonF5.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 5 speichern");
        jRadioButtonF5.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/filter_off_5.png"))); // NOI18N

        jLabel6.setText("Filterprofile:");

        jButtonFilterLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/clear_16.png"))); // NOI18N
        jButtonFilterLoeschen.setToolTipText("Filter löschen");

        jToggleButtonBlacklist.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/blacklist_aus_16.png"))); // NOI18N

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
                        .addComponent(jButtonFilterLoeschen)
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
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel6)
                    .addComponent(jRadioButtonF1)
                    .addComponent(jRadioButtonF2)
                    .addComponent(jRadioButtonF3)
                    .addComponent(jRadioButtonF4)
                    .addComponent(jRadioButtonF5)
                    .addComponent(jButtonOk)
                    .addComponent(jButtonFilterLoeschen)
                    .addComponent(jButtonHilfe)
                    .addComponent(jToggleButtonBlacklist))
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
    public javax.swing.JButton jButtonFilterLoeschen;
    public javax.swing.JButton jButtonHilfe;
    private javax.swing.JButton jButtonOk;
    public javax.swing.JCheckBox jCheckBoxKeineAbos;
    public javax.swing.JCheckBox jCheckBoxKeineGesehenen;
    public javax.swing.JCheckBox jCheckBoxNurHd;
    private javax.swing.JCheckBox jCheckBoxNurNeue;
    public javax.swing.JComboBox<String> jComboBoxFilterSender;
    public javax.swing.JComboBox<String> jComboBoxFilterThema;
    public javax.swing.JComboBox<String> jComboBoxZeitraum;
    private javax.swing.JRadioButton jRadioButtonF1;
    private javax.swing.JRadioButton jRadioButtonF2;
    private javax.swing.JRadioButton jRadioButtonF3;
    private javax.swing.JRadioButton jRadioButtonF4;
    private javax.swing.JRadioButton jRadioButtonF5;
    private javax.swing.JRadioButton jRadioButtonIrgendwo;
    private javax.swing.JRadioButton jRadioButtonTT;
    public javax.swing.JSlider jSliderMinuten;
    public javax.swing.JTextField jTextFieldFilterMinuten;
    public javax.swing.JTextField jTextFieldFilterThemaTitel;
    public javax.swing.JTextField jTextFieldFilterTitel;
    private javax.swing.JToggleButton jToggleButtonBlacklist;
    private javax.swing.JToggleButton jToggleButtonHistory;
    public javax.swing.JToggleButton jToggleButtonLivestram;
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
    public JComboBox<String> get_jComboBoxZeitraum() {
        return jComboBoxZeitraum;
    }

    @Override
    public JSlider get_jSliderMinuten() {
        return jSliderMinuten;
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
        for (ActionListener a : jComboBoxZeitraum.getActionListeners()) {
            jComboBoxZeitraum.removeActionListener(a);
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
