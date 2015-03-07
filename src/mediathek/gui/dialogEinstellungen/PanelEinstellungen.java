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

import java.awt.Frame;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.LookAndFeel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import mediathek.controller.Log;
import mediathek.controller.ProgrammUpdateSuchen;
import mediathek.daten.Daten;
import mediathek.gui.PanelVorlage;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.res.GetIcon;
import mediathek.tool.Funktionen;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVConfig;
import mediathek.tool.MVMessageDialog;

public class PanelEinstellungen extends PanelVorlage {

    private final static String ICONSET_STANDARD = "Standard";

    public PanelEinstellungen(Daten d, JFrame parent) {
        super(d, parent);
        initComponents();
        jButtonInfos.setIcon(GetIcon.getProgramIcon("icons_refresh_16.png"));
        daten = d;
        initSpinner();
        jSpinnerDays.addChangeListener(new BeobSpinnerDays());
        jButtonLoad.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent ae) {
                Daten.listeFilme.clear(); // sonst wird evtl. nur eine Diff geladen
                Daten.filmeLaden.importFilmliste("");
            }
        });
        setupLookAndFeelComboBox();
        jButtonHelpDays.setIcon(GetIcon.getProgramIcon("help_16.png"));
        jButtonHelpDays.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                new DialogHilfe(parentComponent, true, "\n"
                        + "Es werden nur Filme der letzten\n"
                        + "xx Tage geladen."
                        + "\n"
                        + "Bei \"0\" werden alle Filme geladen.\n"
                        + "\n"
                        + "(Eine kleinere Filmliste\n"
                        + "kann bei Rechnern mit wenig\n"
                        + "Speicher hilfreich sein.)"
                        + "\n\n"
                        + "Auswirkung hat das erst nach dem\n"
                        + "Neuladen der kompletten Filmliste.").setVisible(true);
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_ANZAHL_DOWNLOADS, PanelEinstellungen.class.getSimpleName()) {
            @Override
            public void ping() {
                initSpinner();
            }
        });
        jCheckBoxEchtzeit.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_ECHTZEITSUCHE)));
        jCheckBoxEchtzeit.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent ae) {
                Daten.mVConfig.add(MVConfig.SYSTEM_ECHTZEITSUCHE, Boolean.toString(jCheckBoxEchtzeit.isSelected()));
            }
        });
        jCheckBoxSuchen.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_UPDATE_SUCHEN)));
        jCheckBoxSuchen.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent ae) {
                Daten.mVConfig.add(MVConfig.SYSTEM_UPDATE_SUCHEN, Boolean.toString(jCheckBoxSuchen.isSelected()));
            }
        });
        jButtonSuchen.addActionListener(new BeobSuchen(false));
        jButtonInfos.addActionListener(new BeobSuchen(true));
        jButtonRefresh.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                fillIconList();
            }
        });
        fillIconList();
        jComboBoxIcons.addItemListener(new java.awt.event.ItemListener() {
            @Override
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                if (evt.getStateChange() == 1) {
                    cbxIconPackagesItemStateChanged(evt);
                }
            }
        });
    }

    private void cbxIconPackagesItemStateChanged(java.awt.event.ItemEvent evt) {
        MVMessageDialog.showMessageDialog(this, "Sie müssen die Applikation neu starten damit die Icons genutzt werden können.", "MediathekView", JOptionPane.WARNING_MESSAGE);
        String iconName = jComboBoxIcons.getModel().getElementAt(jComboBoxIcons.getSelectedIndex());
        if (iconName.equals(ICONSET_STANDARD)) {
            Daten.mVConfig.add(MVConfig.SYSTEM_ICON_STANDARD, Boolean.TRUE.toString());
            Daten.mVConfig.add(MVConfig.SYSTEM_ICON_PFAD, "");
        } else {
            Daten.mVConfig.add(MVConfig.SYSTEM_ICON_STANDARD, Boolean.FALSE.toString());
        }
        try {
            File[] files = new File(Funktionen.pathProgramIcons()).listFiles();
            if (files != null) {
                for (File file : files) {
                    if (file.isDirectory() && file.getName().equals(iconName)) {
                        Daten.mVConfig.add(MVConfig.SYSTEM_ICON_PFAD, file.getAbsolutePath());
                        break;
                    }
                }
            }
        } catch (Exception ex) {
            Daten.mVConfig.add(MVConfig.SYSTEM_ICON_STANDARD, Boolean.TRUE.toString());
            Daten.mVConfig.add(MVConfig.SYSTEM_ICON_PFAD, "");
            Log.fehlerMeldung(829304789, ex);
        }
    }

    private void initSpinner() {
        if (Daten.mVConfig.get(MVConfig.SYSTEM_ANZ_TAGE_FILMLISTE).equals("")) {
            Daten.mVConfig.add(MVConfig.SYSTEM_ANZ_TAGE_FILMLISTE, "0");
        }
        jSpinnerDays.setValue(Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_ANZ_TAGE_FILMLISTE)));
    }

    @SuppressWarnings("unchecked")
    private void setupLookAndFeelComboBox() {
        try {
            //query all installed LAFs
            final UIManager.LookAndFeelInfo info[];
            info = UIManager.getInstalledLookAndFeels();

            //fill in the combobox model
            ArrayList<String> themeList = new ArrayList<>();
            for (UIManager.LookAndFeelInfo i : info) {
                themeList.add(i.getName());
            }

            DefaultComboBoxModel model = new DefaultComboBoxModel(themeList.toArray());
            jComboBoxLookAndFeel.setModel(model);
            //select the current
            LookAndFeel laf = UIManager.getLookAndFeel();
            int index = model.getIndexOf(laf.getName());
            jComboBoxLookAndFeel.setSelectedIndex(index);
            ActionListener lst = new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {
                    String lafName = jComboBoxLookAndFeel.getModel().getElementAt(jComboBoxLookAndFeel.getSelectedIndex());
                    String lafClass = "";
                    //retrieve class name for selected LAF
                    for (UIManager.LookAndFeelInfo i : info) {
                        if (i.getName().equals(lafName)) {
                            lafClass = i.getClassName();
                            break;
                        }
                    }
                    //and now switch it...
                    try {
                        UIManager.setLookAndFeel(lafClass);
                        SwingUtilities.updateComponentTreeUI(daten.mediathekGui);
                        for (Frame f : Frame.getFrames()) {
                            SwingUtilities.updateComponentTreeUI(f);
                            for (Window w : f.getOwnedWindows()) {
                                SwingUtilities.updateComponentTreeUI(w);
                            }
                        }
                    } catch (Exception ex) {
                        ex.printStackTrace();
                    }
                    Daten.mVConfig.add(MVConfig.SYSTEM_LOOK, lafClass);  //
                }
            };
            jComboBoxLookAndFeel.addActionListener(lst);

        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    private void fillIconList() {
        ArrayList<String> iconList = new ArrayList<>();
        iconList.add(ICONSET_STANDARD);
        try {
            File[] files = new File(Funktionen.pathProgramIcons()).listFiles();
            if (files != null) {
                for (File file : files) {
                    // System.out.print(files[i].getAbsolutePath());
                    if (file.isDirectory()) {
                        // iconList.add(files[i].getAbsolutePath());
                        iconList.add(file.getName());
                    }
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(636875409, ex);
        }
        DefaultComboBoxModel model = new DefaultComboBoxModel(iconList.toArray());
        jComboBoxIcons.setModel(model);
        if (!Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_ICON_STANDARD))) {
            if (!Daten.mVConfig.get(MVConfig.SYSTEM_ICON_PFAD).equals("")) {
                File f = new File(Daten.mVConfig.get(MVConfig.SYSTEM_ICON_PFAD));
                jComboBoxIcons.setSelectedItem(f.getName());
            }
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JPanel pnlProgramUpdate = new javax.swing.JPanel();
        jCheckBoxSuchen = new javax.swing.JCheckBox();
        jButtonSuchen = new javax.swing.JButton();
        jButtonInfos = new javax.swing.JButton();
        jPanel1 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel2 = new javax.swing.JLabel();
        jComboBoxLookAndFeel = new javax.swing.JComboBox<String>();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        jComboBoxIcons = new javax.swing.JComboBox<String>();
        jButtonRefresh = new javax.swing.JButton();
        jPanel2 = new javax.swing.JPanel();
        jCheckBoxEchtzeit = new javax.swing.JCheckBox();
        jLabel6 = new javax.swing.JLabel();
        jSpinnerDays = new javax.swing.JSpinner();
        jButtonHelpDays = new javax.swing.JButton();
        jButtonLoad = new javax.swing.JButton();

        setMinimumSize(getPreferredSize());

        pnlProgramUpdate.setBorder(javax.swing.BorderFactory.createTitledBorder("Programmupdate"));

        jCheckBoxSuchen.setText("einmal am Tag nach einer neuen Programmversion suchen");

        jButtonSuchen.setText("Jetzt suchen");

        jButtonInfos.setText("Programminfos anzeigen");

        javax.swing.GroupLayout pnlProgramUpdateLayout = new javax.swing.GroupLayout(pnlProgramUpdate);
        pnlProgramUpdate.setLayout(pnlProgramUpdateLayout);
        pnlProgramUpdateLayout.setHorizontalGroup(
            pnlProgramUpdateLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnlProgramUpdateLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(pnlProgramUpdateLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jCheckBoxSuchen)
                    .addGroup(pnlProgramUpdateLayout.createSequentialGroup()
                        .addComponent(jButtonSuchen, javax.swing.GroupLayout.PREFERRED_SIZE, 173, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonInfos)))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        pnlProgramUpdateLayout.setVerticalGroup(
            pnlProgramUpdateLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnlProgramUpdateLayout.createSequentialGroup()
                .addComponent(jCheckBoxSuchen)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(pnlProgramUpdateLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonSuchen)
                    .addComponent(jButtonInfos))
                .addContainerGap(20, Short.MAX_VALUE))
        );

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder(""));

        jLabel2.setText("Look&Feel:");

        jLabel1.setText("Icon-Pack:");

        jButtonRefresh.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/icons_refresh_16.png"))); // NOI18N
        jButtonRefresh.setToolTipText("neue Icons suchen");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel2)
                    .addComponent(jLabel1))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jComboBoxIcons, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jComboBoxLookAndFeel, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonRefresh)
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jComboBoxLookAndFeel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel2))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jComboBoxIcons, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addComponent(jLabel1))
                    .addComponent(jButtonRefresh))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder(""));

        jCheckBoxEchtzeit.setText("Echtzeitsuche im Filter");

        jLabel6.setText("nur die Filme der letzten Tage laden:");

        jSpinnerDays.setModel(new javax.swing.SpinnerNumberModel(0, 0, 30, 1));

        jButtonHelpDays.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/help_16.png"))); // NOI18N

        jButtonLoad.setText("Filmliste jetzt neu laden");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(jCheckBoxEchtzeit, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addGap(64, 64, 64))
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(jLabel6)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jSpinnerDays, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonHelpDays)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonLoad)
                        .addGap(0, 16, Short.MAX_VALUE))))
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addGap(15, 15, 15)
                .addComponent(jCheckBoxEchtzeit)
                .addGap(18, 18, 18)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel6)
                    .addComponent(jSpinnerDays, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonHelpDays)
                    .addComponent(jButtonLoad))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(pnlProgramUpdate, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel1, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(pnlProgramUpdate, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonHelpDays;
    private javax.swing.JButton jButtonInfos;
    private javax.swing.JButton jButtonLoad;
    private javax.swing.JButton jButtonRefresh;
    private javax.swing.JButton jButtonSuchen;
    private javax.swing.JCheckBox jCheckBoxEchtzeit;
    private javax.swing.JCheckBox jCheckBoxSuchen;
    private javax.swing.JComboBox<String> jComboBoxIcons;
    private javax.swing.JComboBox<String> jComboBoxLookAndFeel;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JSpinner jSpinnerDays;
    // End of variables declaration//GEN-END:variables

    private class BeobSpinnerDays implements ChangeListener {

        @Override
        public void stateChanged(ChangeEvent arg0) {
            Daten.mVConfig.add(MVConfig.SYSTEM_ANZ_TAGE_FILMLISTE,
                    String.valueOf(((Number) jSpinnerDays.getModel().getValue()).intValue()));
        }
    }

    private class BeobSuchen implements ActionListener {

        private boolean infos = false;

        public BeobSuchen(boolean iinfos) {
            infos = iinfos;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            new ProgrammUpdateSuchen().checkVersion(daten, !infos /* bei aktuell anzeigen */, infos /* Hinweis */, true /* hinweiseAlleAnzeigen */);
        }
    }

}
