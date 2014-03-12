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

public class PanelEinstellungen extends PanelVorlage {

    private final String ICONSET_STANDARD = "Standard";

    public PanelEinstellungen(Daten d, JFrame parent) {
        super(d, parent);
        initComponents();
        jButtonInfos.setIcon(GetIcon.getIcon("icons_refresh_16.png"));
        daten = d;
        init();
        jCheckBoxEchtzeit.addActionListener(new BeobCheckBox());
        jSpinnerDownload.addChangeListener(new BeobSpinnerDownload());
        setupLookAndFeelComboBox();
        jButtonHilfeAnzahl.setIcon(GetIcon.getIcon("help_16.png"));
        jButtonHilfeAnzahl.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                new DialogHilfe(parentComponent, true, "\n"
                        + "Hier kann angegeben werden, wie viele\n"
                        + "Downloads gleichzeitig gestartet werden können.\n"
                        + "Es gibt jedoch noch eine Begrenzung \n"
                        + "auf 2 Downloads pro Server\n"
                        + "die trotzdem nicht überschritten wird.").setVisible(true);
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_ANZAHL_DOWNLOADS, PanelEinstellungen.class.getSimpleName()) {
            @Override
            public void ping() {
                init();
            }
        });
        jCheckBoxNotification.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_NOTIFICATION, Boolean.toString(jCheckBoxNotification.isSelected()));
            }
        });
        jCheckBoxSuchen.addActionListener(new BeobCheckBoxSuchen());
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
        int bandbreite;
        try {
            bandbreite = Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_BANDBREITE_KBYTE));
        } catch (Exception ex) {
            bandbreite = 0;
            Daten.mVConfig.add(MVConfig.SYSTEM_BANDBREITE_KBYTE, "0");
        }
        jSliderBandbreite.setValue(bandbreite / 10);
        jLabelBandwidth.setText(bandbreite + " kByte/s");
        jSliderBandbreite.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                int b = jSliderBandbreite.getValue() * 10;
                jLabelBandwidth.setText(b + " kByte/s");
                Daten.mVConfig.add(MVConfig.SYSTEM_BANDBREITE_KBYTE, String.valueOf(b));
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_BANDBREITE, PanelEinstellungen.class.getName());
            }
        });
    }

    private void cbxIconPackagesItemStateChanged(java.awt.event.ItemEvent evt) {
        JOptionPane.showMessageDialog(this, "Sie müssen die Applikation neu starten damit die Icons genutzt werden können.", "MediathekView", JOptionPane.WARNING_MESSAGE);
        String iconName = jComboBoxIcons.getModel().getElementAt(jComboBoxIcons.getSelectedIndex());
        if (iconName.equals(ICONSET_STANDARD)) {
            Daten.mVConfig.add(MVConfig.SYSTEM_ICON_STANDARD, Boolean.TRUE.toString());
            Daten.mVConfig.add(MVConfig.SYSTEM_ICON_PFAD, "");
        } else {
            Daten.mVConfig.add(MVConfig.SYSTEM_ICON_STANDARD, Boolean.FALSE.toString());
        }
        try {
            File[] files = new File(Funktionen.getPfadIcons()).listFiles();
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
            Log.fehlerMeldung(829304789, Log.FEHLER_ART_PROG, "PanelEinstellungen", ex);
        }
    }

    private void init() {
        jCheckBoxNotification.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_NOTIFICATION)));
        jCheckBoxSuchen.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_UPDATE_SUCHEN)));
        jCheckBoxEchtzeit.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_ECHTZEITSUCHE)));
        // UserAgent
        // Rest
        if (Daten.mVConfig.get(MVConfig.SYSTEM_MAX_DOWNLOAD).equals("")) {
            jSpinnerDownload.setValue(1);
            Daten.mVConfig.add(MVConfig.SYSTEM_MAX_DOWNLOAD, "1");
        } else {
            jSpinnerDownload.setValue(Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_MAX_DOWNLOAD)));
        }
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
            File[] files = new File(Funktionen.getPfadIcons()).listFiles();
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
            Log.fehlerMeldung(636875409, Log.FEHLER_ART_PROG, "PanelEinstellungen", ex);
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
        jCheckBoxNotification = new javax.swing.JCheckBox();
        jLabelBandwidth = new javax.swing.JLabel();
        javax.swing.JLabel jLabel3 = new javax.swing.JLabel();
        jSpinnerDownload = new javax.swing.JSpinner();
        jButtonHilfeAnzahl = new javax.swing.JButton();
        jSliderBandbreite = new javax.swing.JSlider();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();

        setMinimumSize(getPreferredSize());

        pnlProgramUpdate.setBorder(javax.swing.BorderFactory.createTitledBorder("Programmupdate"));

        jCheckBoxSuchen.setText("Einmal am Tag nach einer neuen Programmversion suchen");

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

        jButtonRefresh.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/icons_refresh_16.png"))); // NOI18N
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

        jCheckBoxNotification.setText("Benachrichtigungen anzeigen");

        jLabelBandwidth.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        jLabelBandwidth.setText("1000 kByte/s");
        jLabelBandwidth.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));

        jLabel3.setText("gleichzeitige Downloads laden:");

        jSpinnerDownload.setModel(new javax.swing.SpinnerNumberModel(1, 1, 9, 1));

        jButtonHilfeAnzahl.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/help_16.png"))); // NOI18N

        jSliderBandbreite.setMajorTickSpacing(10);
        jSliderBandbreite.setMinorTickSpacing(5);
        jSliderBandbreite.setToolTipText("");
        jSliderBandbreite.setValue(0);

        jLabel4.setText("Downloadgeschwindigkeit begrenzen:");

        jLabel5.setText("(nur für direkte Downloads, bei \"0\" gibt es keine Begrenzung)");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jCheckBoxEchtzeit, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jCheckBoxNotification, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        .addGap(64, 64, 64))
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(jLabel3)
                        .addGap(6, 6, 6)
                        .addComponent(jSpinnerDownload, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jButtonHilfeAnzahl)
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel2Layout.createSequentialGroup()
                                .addGap(12, 12, 12)
                                .addComponent(jLabel5)
                                .addGap(181, 181, 181))
                            .addGroup(jPanel2Layout.createSequentialGroup()
                                .addComponent(jLabel4)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabelBandwidth, javax.swing.GroupLayout.PREFERRED_SIZE, 98, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jSliderBandbreite, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                        .addContainerGap())))
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addGap(15, 15, 15)
                .addComponent(jCheckBoxEchtzeit)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jCheckBoxNotification)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel3)
                    .addComponent(jSpinnerDownload, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonHilfeAnzahl))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabelBandwidth)
                    .addComponent(jSliderBandbreite, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel4))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel5)
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
                .addContainerGap(138, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonHilfeAnzahl;
    private javax.swing.JButton jButtonInfos;
    private javax.swing.JButton jButtonRefresh;
    private javax.swing.JButton jButtonSuchen;
    private javax.swing.JCheckBox jCheckBoxEchtzeit;
    private javax.swing.JCheckBox jCheckBoxNotification;
    private javax.swing.JCheckBox jCheckBoxSuchen;
    private javax.swing.JComboBox<String> jComboBoxIcons;
    private javax.swing.JComboBox<String> jComboBoxLookAndFeel;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabelBandwidth;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JSlider jSliderBandbreite;
    private javax.swing.JSpinner jSpinnerDownload;
    // End of variables declaration//GEN-END:variables

    private class BeobSpinnerDownload implements ChangeListener {

        @Override
        public void stateChanged(ChangeEvent arg0) {
            Daten.mVConfig.add(MVConfig.SYSTEM_MAX_DOWNLOAD,
                    String.valueOf(((Number) jSpinnerDownload.getModel().getValue()).intValue()));
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_ANZAHL_DOWNLOADS, PanelEinstellungen.class.getSimpleName());
        }
    }

    private class BeobCheckBox implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            Daten.mVConfig.add(MVConfig.SYSTEM_ECHTZEITSUCHE, Boolean.toString(jCheckBoxEchtzeit.isSelected()));
        }
    }

    private class BeobCheckBoxSuchen implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            Daten.mVConfig.add(MVConfig.SYSTEM_UPDATE_SUCHEN, Boolean.toString(jCheckBoxSuchen.isSelected()));
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
