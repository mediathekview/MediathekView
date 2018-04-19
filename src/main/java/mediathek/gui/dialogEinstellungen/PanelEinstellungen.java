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
import mSearch.tool.Listener;
import mSearch.tool.Log;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.gui.PanelVorlage;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.tool.MVFunctionSys;
import mediathek.tool.MVMessageDialog;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.*;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;

@SuppressWarnings("serial")
public class PanelEinstellungen extends PanelVorlage {
    private final static String ICONSET_STANDARD = "Standard";
    private final static String ALLE = " Alle ";

    public PanelEinstellungen(Daten d, JFrame parent) {
        super(d, parent);
        initComponents();

        daten = d;
        SpinnerListModel lm = new SpinnerListModel(new Object[]{ALLE, "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
            "12", "14", "16", "18", "20", "25", "30"});
        jSpinnerDays.setModel(lm);
        ((JSpinner.DefaultEditor) jSpinnerDays.getEditor()).getTextField().setEditable(false);
        initSpinner();
        jSpinnerDays.addChangeListener(new BeobSpinnerDays());

        jButtonLoad.addActionListener(ae -> {
            daten.getListeFilme().clear(); // sonst wird evtl. nur eine Diff geladen
            daten.getFilmeLaden().loadFilmlist("");
        });
        setupLookAndFeelComboBox();
        jButtonHelpDays.setIcon(Icons.ICON_BUTTON_HELP);
        jButtonHelpDays.addActionListener(e -> new DialogHilfe(parentComponent, true, '\n'
                + "Es werden nur Filme der letzten\n"
                + "xx Tage geladen."
                + '\n'
                + "Bei \"Alle\" werden alle Filme geladen.\n"
                + '\n'
                + "(Eine kleinere Filmliste\n"
                + "kann bei Rechnern mit wenig\n"
                + "Speicher hilfreich sein.)"
                + "\n\n"
                + "Auswirkung hat das erst nach dem\n"
                + "Neuladen der kompletten Filmliste.").setVisible(true));

        Listener.addListener(new Listener(Listener.EREIGNIS_ANZAHL_DOWNLOADS, PanelEinstellungen.class.getSimpleName()) {
            @Override
            public void ping() {
                initSpinner();
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_TRAYICON, PanelEinstellungen.class.getSimpleName()) {
            @Override
            public void ping() {
                jCheckBoxTray.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_TRAY)));
            }
        });

        jCheckBoxTabsTop.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TABS_TOP)));
        jCheckBoxTabsTop.addActionListener(ae -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_TABS_TOP, Boolean.toString(jCheckBoxTabsTop.isSelected()));
            Listener.notify(Listener.EREIGNIS_TABS_TOP, PanelEinstellungen.class.getSimpleName());
        });
        jCheckBoxTabIcon.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TABS_ICON)));
        jCheckBoxTabIcon.addActionListener(ae -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_TABS_ICON, Boolean.toString(jCheckBoxTabIcon.isSelected()));
            Listener.notify(Listener.EREIGNIS_TABS_TOP, PanelEinstellungen.class.getSimpleName());
        });

        if (SystemInfo.isMacOSX()) {
            jCheckBoxTray.setSelected(false);
            jCheckBoxTray.setEnabled(false);
        } else {
            jCheckBoxTray.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_TRAY)));
            jCheckBoxTray.addActionListener(ae -> {
                MVConfig.add(MVConfig.Configs.SYSTEM_USE_TRAY, Boolean.toString(jCheckBoxTray.isSelected()));
                daten.getMediathekGui().setTray();
            });
        }

        jButtonRefresh.addActionListener(e -> fillIconList());

        fillIconList();
        jComboBoxIcons.addItemListener(evt -> {
            if (evt.getStateChange() == 1) {
                cbxIconPackagesItemStateChanged(evt);
            }
        });
    }

    private void cbxIconPackagesItemStateChanged(java.awt.event.ItemEvent evt) {
        MVMessageDialog.showMessageDialog(this, "Sie müssen die Applikation neu starten damit die Icons genutzt werden können.", "MediathekView", JOptionPane.WARNING_MESSAGE);
        String iconName = jComboBoxIcons.getModel().getElementAt(jComboBoxIcons.getSelectedIndex());
        if (iconName.equals(ICONSET_STANDARD)) {
            MVConfig.add(MVConfig.Configs.SYSTEM_ICON_STANDARD, Boolean.TRUE.toString());
            MVConfig.add(MVConfig.Configs.SYSTEM_ICON_PFAD, "");
        } else {
            MVConfig.add(MVConfig.Configs.SYSTEM_ICON_STANDARD, Boolean.FALSE.toString());
        }
        try {
            File[] files = new File(MVFunctionSys.pathProgramIcons()).listFiles();
            if (files != null) {
                for (File file : files) {
                    if (file.isDirectory() && file.getName().equals(iconName)) {
                        MVConfig.add(MVConfig.Configs.SYSTEM_ICON_PFAD, file.getAbsolutePath());
                        break;
                    }
                }
            }
        } catch (Exception ex) {
            MVConfig.add(MVConfig.Configs.SYSTEM_ICON_STANDARD, Boolean.TRUE.toString());
            MVConfig.add(MVConfig.Configs.SYSTEM_ICON_PFAD, "");
            Log.errorLog(829304789, ex);
        }
    }

    private void initSpinner() {
        if (MVConfig.get(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE).equals("")) {
            MVConfig.add(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE, "0");
        }
        String s = MVConfig.get(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE);
        if (s.equals("0")) {
            s = ALLE;
        }
        jSpinnerDays.setValue(s);
    }

    private void setupLookAndFeelComboBox() {
        try {
            //query all installed LAFs
            final UIManager.LookAndFeelInfo info[];
            info = UIManager.getInstalledLookAndFeels();
            LookAndFeel aktLaf = UIManager.getLookAndFeel();
            String classNameAktLaf = aktLaf.getClass().getName();
            int idx = 0;

            //fill in the combobox model
            ArrayList<String> themeList = new ArrayList<>(); // list of "UIManager.LookAndFeelInfo" names
            for (int i = 0; i < info.length; ++i) {
                themeList.add(info[i].getName());
                // LookAndFeelInfo.getName und LookAndFeel.getName sind beim GTK LF nicht gleich??
                if (info[i].getClassName().equals(classNameAktLaf)) {
                    idx = i;
                }
            }
            DefaultComboBoxModel<String> model = new DefaultComboBoxModel<>(themeList.toArray(new String[0]));
            jComboBoxLookAndFeel.setModel(model);
            jComboBoxLookAndFeel.setSelectedIndex(idx);

            ActionListener lst = actionEvent -> {
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
                    SwingUtilities.updateComponentTreeUI(daten.getMediathekGui());
                    for (Frame f : Frame.getFrames()) {
                        SwingUtilities.updateComponentTreeUI(f);
                        for (Window w : f.getOwnedWindows()) {
                            SwingUtilities.updateComponentTreeUI(w);
                        }
                    }
                } catch (Exception ex) {
                    ex.printStackTrace();
                }
                MVConfig.add(MVConfig.Configs.SYSTEM_LOOK, lafClass);  //
            };
            jComboBoxLookAndFeel.addActionListener(lst);

        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    private void fillIconList() {
        final DefaultComboBoxModel<String> model = new DefaultComboBoxModel<>();
        model.addElement(ICONSET_STANDARD);
        try {
            File[] files = new File(MVFunctionSys.pathProgramIcons()).listFiles();
            if (files != null) {
                for (File file : files) {
                    if (file.isDirectory()) {
                        model.addElement(file.getName());
                    }
                }
            }
        } catch (Exception ex) {
            Log.errorLog(636875409, ex);
        }

        jComboBoxIcons.setModel(model);

        if (!Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ICON_STANDARD))) {
            if (!MVConfig.get(MVConfig.Configs.SYSTEM_ICON_PFAD).isEmpty()) {
                File f = new File(MVConfig.get(MVConfig.Configs.SYSTEM_ICON_PFAD));
                jComboBoxIcons.setSelectedItem(f.getName());
            }
        }
    }


    private class BeobSpinnerDays implements ChangeListener {

        @Override
        public void stateChanged(ChangeEvent arg0) {
            String s = jSpinnerDays.getModel().getValue().toString();
            if (s.equals(ALLE)) {
                s = "0";
            }
            MVConfig.add(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE, s);
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel2 = new javax.swing.JLabel();
        jComboBoxLookAndFeel = new javax.swing.JComboBox<>();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        jComboBoxIcons = new javax.swing.JComboBox<>();
        jButtonRefresh = new javax.swing.JButton();
        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel6 = new javax.swing.JLabel();
        jSpinnerDays = new javax.swing.JSpinner();
        jButtonHelpDays = new javax.swing.JButton();
        jButtonLoad = new javax.swing.JButton();
        jCheckBoxTray = new javax.swing.JCheckBox();
        jCheckBoxTabsTop = new javax.swing.JCheckBox();
        jCheckBoxTabIcon = new javax.swing.JCheckBox();

        setMinimumSize(getPreferredSize());

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder(""));

        jLabel2.setText("Look&Feel:");

        jLabel1.setText("Icon-Pack:");

        jButtonRefresh.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-aktualisieren.png"))); // NOI18N
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

        jLabel6.setText("nur die Filme der letzten Tage laden:");

        jSpinnerDays.setModel(new javax.swing.SpinnerListModel(new String[] {"Alles", "1", "2", "10", "15"}));

        jButtonHelpDays.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-help.png"))); // NOI18N
        jButtonHelpDays.setToolTipText("Hilfe anzeigen");

        jButtonLoad.setText("Filmliste jetzt neu laden");

        jCheckBoxTray.setText("Programm ins Tray minimieren");

        jCheckBoxTabsTop.setText("Tabs oben anzeigen");

        jCheckBoxTabIcon.setText("Icons im Tab anzeigen");
        jCheckBoxTabIcon.setToolTipText("Im Tab keine Icons anzeigen");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                            .addComponent(jCheckBoxTabsTop)
                            .addGap(18, 18, 18)
                            .addComponent(jCheckBoxTabIcon))
                        .addGroup(jPanel2Layout.createSequentialGroup()
                                .addComponent(jLabel6)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jSpinnerDays, javax.swing.GroupLayout.PREFERRED_SIZE, 78, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonHelpDays)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonLoad))
                        .addComponent(jCheckBoxTray))
                    .addGap(0, 42, Short.MAX_VALUE))
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jCheckBoxTabsTop)
                    .addComponent(jCheckBoxTabIcon))
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jCheckBoxTray)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
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
                    .addComponent(jPanel1, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addContainerGap(299, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonHelpDays;
    private javax.swing.JButton jButtonLoad;
    private javax.swing.JButton jButtonRefresh;
    private javax.swing.JCheckBox jCheckBoxTabIcon;
    private javax.swing.JCheckBox jCheckBoxTabsTop;
    private javax.swing.JCheckBox jCheckBoxTray;
    private javax.swing.JComboBox<String> jComboBoxIcons;
    private javax.swing.JComboBox<String> jComboBoxLookAndFeel;
    private javax.swing.JSpinner jSpinnerDays;
    // End of variables declaration//GEN-END:variables
}
