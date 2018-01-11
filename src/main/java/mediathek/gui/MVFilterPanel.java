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
import mSearch.tool.Listener;
import mediathek.MediathekGui;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.res.GetIcon;
import mediathek.tool.TextCopyPaste;

import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;

@SuppressWarnings("serial")
public class MVFilterPanel extends JPanel implements MVFilter {
    private int aktFilter = -1;

    public MVFilterPanel(MediathekGui aMediathekGui) {
        initComponents();

        if (SystemInfo.isWindows()) {
            // zum Abfangen der Win-F4 für comboboxen
            InputMap im = jComboBoxFilterSender.getInputMap();
            im.put(KeyStroke.getKeyStroke(KeyEvent.VK_F4, 0), "einstellungen");
            ActionMap am = jComboBoxFilterSender.getActionMap();
            am.put("einstellungen", new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    aMediathekGui.showSettingsDialog();
                }
            });
            im = jComboBoxFilterThema.getInputMap();
            im.put(KeyStroke.getKeyStroke(KeyEvent.VK_F4, 0), "einstellungen");
            am = jComboBoxFilterThema.getActionMap();
            am.put("einstellungen", new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    aMediathekGui.showSettingsDialog();
                }
            });
        }
        jButtonClearAll.setIcon(Icons.ICON_BUTTON_CLEAR);
        jButtonFilterLoeschen.setIcon(Icons.ICON_BUTTON_CLEAR);
        jButtonFilterLoeschen.setMnemonic(KeyEvent.VK_F8);

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
        Listener.addListener(new Listener(Listener.EREIGNIS_FILTER_ANZAHL, MVFilterPanel.class.getSimpleName()) {
            @Override
            public void ping() {
                setFilterAnzahl();
            }
        });
        jTextFieldFilterTitel.addMouseListener(new TextCopyPaste());

        setToolTip();
        super.setVisible(true);
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
                jRadioButtonF1.setIcon(GetIcon.getProgramIcon("filter-on-1.png"));
                break;
            case 1:
                jRadioButtonF2.setIcon(GetIcon.getProgramIcon("filter-on-2.png"));
                break;
            case 2:
                jRadioButtonF3.setIcon(GetIcon.getProgramIcon("filter-on-3.png"));
                break;
            case 3:
                jRadioButtonF4.setIcon(GetIcon.getProgramIcon("filter-on-4.png"));
                break;
            case 4:
                jRadioButtonF5.setIcon(GetIcon.getProgramIcon("filter-on-5.png"));
                break;
        }
    }

    private void setAktIcon(int filter) {
        switch (filter) {
            case 0:
                jRadioButtonF1.setIcon(GetIcon.getProgramIcon("filter-akt-1.png"));
                break;
            case 1:
                jRadioButtonF2.setIcon(GetIcon.getProgramIcon("filter-akt-2.png"));
                break;
            case 2:
                jRadioButtonF3.setIcon(GetIcon.getProgramIcon("filter-akt-3.png"));
                break;
            case 3:
                jRadioButtonF4.setIcon(GetIcon.getProgramIcon("filter-akt-4.png"));
                break;
            case 4:
                jRadioButtonF5.setIcon(GetIcon.getProgramIcon("filter-akt-5.png"));
                break;
        }
    }

    private void setIconOff(int filter) {
        switch (filter) {
            case 0:
                jRadioButtonF1.setIcon(GetIcon.getProgramIcon("filter-off-1.png"));
                break;
            case 1:
                jRadioButtonF2.setIcon(GetIcon.getProgramIcon("filter-off-2.png"));
                break;
            case 2:
                jRadioButtonF3.setIcon(GetIcon.getProgramIcon("filter-off-3.png"));
                break;
            case 3:
                jRadioButtonF4.setIcon(GetIcon.getProgramIcon("filter-off-4.png"));
                break;
            case 4:
                jRadioButtonF5.setIcon(GetIcon.getProgramIcon("filter-off-5.png"));
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
                Listener.notify(Listener.EREIGNIS_FILTER_ANZAHL, MVFilterPanel.class.getSimpleName());
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

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.ButtonGroup buttonGroup1 = new javax.swing.ButtonGroup();
        javax.swing.ButtonGroup buttonGroup3 = new javax.swing.ButtonGroup();
        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        jTextFieldFilterMinuten = new javax.swing.JTextField();
        jSliderMinuten = new javax.swing.JSlider();
        jSliderTage = new javax.swing.JSlider();
        jTextFieldFilterTage = new javax.swing.JTextField();
        rbMin = new javax.swing.JRadioButton();
        rbMax = new javax.swing.JRadioButton();
        javax.swing.JLabel jLabel6 = new javax.swing.JLabel();
        javax.swing.JPanel jPanel3 = new javax.swing.JPanel();
        jToggleButtonLivestram = new javax.swing.JToggleButton();
        jButtonClearAll = new javax.swing.JButton();
        javax.swing.JPanel jPanel6 = new javax.swing.JPanel();
        javax.swing.JPanel jPanel5 = new javax.swing.JPanel();
        javax.swing.JPanel jPanel7 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel2 = new javax.swing.JLabel();
        jComboBoxFilterSender = new javax.swing.JComboBox<>();
        javax.swing.JLabel jLabel4 = new javax.swing.JLabel();
        jComboBoxFilterThema = new javax.swing.JComboBox<>();
        javax.swing.JLabel jLabel5 = new javax.swing.JLabel();
        jTextFieldFilterTitel = new javax.swing.JTextField();
        jButtonFilterLoeschen = new javax.swing.JButton();
        javax.swing.JPanel jPanel4 = new javax.swing.JPanel();
        jRadioButtonF1 = new javax.swing.JRadioButton();
        jRadioButtonF2 = new javax.swing.JRadioButton();
        jRadioButtonF3 = new javax.swing.JRadioButton();
        jRadioButtonF4 = new javax.swing.JRadioButton();
        jRadioButtonF5 = new javax.swing.JRadioButton();

        jLabel1.setText("Zeitraum [Tage]:");

        jTextFieldFilterMinuten.setEditable(false);

        jSliderTage.setMaximum(30);
        jSliderTage.setValue(15);

        jTextFieldFilterTage.setEditable(false);

        buttonGroup3.add(rbMin);
        rbMin.setSelected(true);
        rbMin.setText("Mindestlänge [min]");

        buttonGroup3.add(rbMax);
        rbMax.setText("Maximallänge");

        jLabel6.setText("Filterprofile:");

        jPanel3.setLayout(new org.jdesktop.swingx.VerticalLayout());

        jToggleButtonLivestram.setText("Livestreams");
        jPanel3.add(jToggleButtonLivestram);

        jButtonClearAll.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-clear.png"))); // NOI18N
        jButtonClearAll.setToolTipText("Alles löschen");
        jPanel3.add(jButtonClearAll);

        jPanel6.setLayout(new org.jdesktop.swingx.VerticalLayout());

        jPanel5.setLayout(new org.jdesktop.swingx.HorizontalLayout());
        jPanel6.add(jPanel5);

        jPanel7.setLayout(new org.jdesktop.swingx.VerticalLayout());

        jLabel2.setText("Sender:");
        jPanel7.add(jLabel2);

        jComboBoxFilterSender.setMaximumRowCount(25);
        jComboBoxFilterSender.setPreferredSize(new java.awt.Dimension(184, 24));
        jPanel7.add(jComboBoxFilterSender);

        jLabel4.setText("Thema:");
        jPanel7.add(jLabel4);

        jComboBoxFilterThema.setMaximumRowCount(20);
        jComboBoxFilterThema.setPreferredSize(new java.awt.Dimension(184, 24));
        jPanel7.add(jComboBoxFilterThema);

        jLabel5.setText("Titel:");
        jPanel7.add(jLabel5);

        jTextFieldFilterTitel.setPreferredSize(new java.awt.Dimension(184, 24));
        jPanel7.add(jTextFieldFilterTitel);

        jButtonFilterLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-clear.png"))); // NOI18N
        jButtonFilterLoeschen.setToolTipText("Filter löschen");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jSliderTage, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jSliderMinuten, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jTextFieldFilterTage, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(rbMax)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jTextFieldFilterMinuten, javax.swing.GroupLayout.PREFERRED_SIZE, 64, javax.swing.GroupLayout.PREFERRED_SIZE))
                        .addGroup(jPanel2Layout.createSequentialGroup()
                                .addComponent(jButtonFilterLoeschen)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jPanel6, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        .addComponent(jPanel7, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(rbMin)
                                .addComponent(jLabel6)
                                .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                            .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );

        jPanel2Layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jTextFieldFilterMinuten, jTextFieldFilterTage});

        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jPanel7, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel2Layout.createSequentialGroup()
                                    .addGap(12, 12, 12)
                                    .addComponent(jPanel6, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                            .addGroup(jPanel2Layout.createSequentialGroup()
                                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(jButtonFilterLoeschen)))
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jTextFieldFilterTage, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jSliderTage, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(rbMin)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(rbMax)
                    .addComponent(jTextFieldFilterMinuten, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jSliderMinuten, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jLabel6)
                    .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel2Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jLabel1, jTextFieldFilterMinuten, jTextFieldFilterTage});

        jPanel4.setLayout(new org.jdesktop.swingx.HorizontalLayout());

        buttonGroup1.add(jRadioButtonF1);
        jRadioButtonF1.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 1 speichern");
        jRadioButtonF1.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/filter-off-1.png"))); // NOI18N
        jPanel4.add(jRadioButtonF1);

        buttonGroup1.add(jRadioButtonF2);
        jRadioButtonF2.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 2 speichern");
        jRadioButtonF2.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/filter-off-2.png"))); // NOI18N
        jPanel4.add(jRadioButtonF2);

        buttonGroup1.add(jRadioButtonF3);
        jRadioButtonF3.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 3 speichern");
        jRadioButtonF3.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/filter-off-3.png"))); // NOI18N
        jPanel4.add(jRadioButtonF3);

        buttonGroup1.add(jRadioButtonF4);
        jRadioButtonF4.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 4 speichern");
        jRadioButtonF4.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/filter-off-4.png"))); // NOI18N
        jPanel4.add(jRadioButtonF4);

        buttonGroup1.add(jRadioButtonF5);
        jRadioButtonF5.setToolTipText("Filter-Einstellungen vornehmen und mit Rechtsklick als Profil 5 speichern");
        jRadioButtonF5.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/filter-off-5.png"))); // NOI18N
        jPanel4.add(jRadioButtonF5);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                    .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jPanel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonClearAll;
    public javax.swing.JButton jButtonFilterLoeschen;
    public javax.swing.JComboBox<String> jComboBoxFilterSender;
    public javax.swing.JComboBox<String> jComboBoxFilterThema;
    private javax.swing.JRadioButton jRadioButtonF1;
    private javax.swing.JRadioButton jRadioButtonF2;
    private javax.swing.JRadioButton jRadioButtonF3;
    private javax.swing.JRadioButton jRadioButtonF4;
    private javax.swing.JRadioButton jRadioButtonF5;
    public javax.swing.JSlider jSliderMinuten;
    private javax.swing.JSlider jSliderTage;
    public javax.swing.JTextField jTextFieldFilterMinuten;
    private javax.swing.JTextField jTextFieldFilterTage;
    public javax.swing.JTextField jTextFieldFilterTitel;
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
    public JButton get_jButtonFilterLoeschen() {
        return jButtonFilterLoeschen;
    }

    @Override
    public JButton get_jButtonClearAll() {
        return jButtonClearAll;
    }

    @Override
    public JComboBox<String> get_jComboBoxFilterSender() {
        return jComboBoxFilterSender;
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
    public JTextField get_jTextFieldFilterTitel() {
        return jTextFieldFilterTitel;
    }

    @Override
    public JToggleButton get_jToggleButtonLivestram() {
        return jToggleButtonLivestram;
    }

    @Override
    public void removeAllListener() {
        aktFilter = -1;
        setIcon(false); // erst mal alle aus
        for (ActionListener a : jButtonFilterLoeschen.getActionListeners()) {
            jButtonFilterLoeschen.removeActionListener(a);
        }
        for (ActionListener a : jButtonClearAll.getActionListeners()) {
            jButtonFilterLoeschen.removeActionListener(a);
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
        for (ActionListener a : jTextFieldFilterTitel.getActionListeners()) {
            jTextFieldFilterTitel.removeActionListener(a);
        }
        for (ActionListener a : jToggleButtonLivestram.getActionListeners()) {
            jToggleButtonLivestram.removeActionListener(a);
        }
    }

}
