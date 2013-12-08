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
import javax.swing.LookAndFeel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import mediathek.controller.ProgrammUpdateSuchen;
import mediathek.daten.Daten;
import mediathek.gui.PanelVorlage;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.res.GetIcon;
import mediathek.tool.Funktionen;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;

public class PanelEinstellungen extends PanelVorlage {

    private final String ICONSET_STANDARD = "Standard";

    public PanelEinstellungen(Daten d, JFrame parentComponent) {
        super(d, parentComponent);
        initComponents();
        jButtonInfos.setIcon(GetIcon.getIcon("icons_refresh_16.png"));
        daten = d;
        init();
        jCheckBoxEchtzeit.addActionListener(new BeobCheckBox());
        jSpinnerDownload.addChangeListener(new BeobSpinnerDownload());
        setupLookAndFeelComboBox();

        jButtonHilfeAnzahl.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                new DialogHilfe(null, true, "\n"
                        + "Hier kann angegeben werden, wie viele\n"
                        + "Downloads gleichzeitig gestartet werden.\n"
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
                Daten.system[Konstanten.SYSTEM_NOTIFICATION_NR] = Boolean.toString(jCheckBoxNotification.isSelected());
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
        setUpIconList();
    }

    private void init() {
        jCheckBoxNotification.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_NOTIFICATION_NR]));
        jCheckBoxSuchen.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_UPDATE_SUCHEN_NR]));
        jCheckBoxEchtzeit.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_ECHTZEITSUCHE_NR]));
        // UserAgent
        // Rest
        if (Daten.system[Konstanten.SYSTEM_MAX_DOWNLOAD_NR].equals("")) {
            jSpinnerDownload.setValue(1);
            Daten.system[Konstanten.SYSTEM_MAX_DOWNLOAD_NR] = "1";
        } else {
            jSpinnerDownload.setValue(Integer.parseInt(Daten.system[Konstanten.SYSTEM_MAX_DOWNLOAD_NR]));
        }
        int bandbreite;
        try {
            bandbreite = Integer.parseInt(Daten.system[Konstanten.SYSTEM_BANDBREITE_KBYTE_NR]);
        } catch (Exception ex) {
            bandbreite = 0;
            Daten.system[Konstanten.SYSTEM_BANDBREITE_KBYTE_NR] = "0";
        }
        jSliderBandbreite.setValue(bandbreite);
        jLabelBandbreite.setText(bandbreite + " kByte/s");
        jSliderBandbreite.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                int b = jSliderBandbreite.getValue();
                jLabelBandbreite.setText(b + " kByte/s");
                Daten.system[Konstanten.SYSTEM_BANDBREITE_KBYTE_NR] = String.valueOf(b);
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_BANDBREITE, PanelEinstellungen.class.getName());

            }
        });
    }

    private void setupLookAndFeelComboBox() {
        try {
            //query all installed LAFs
            final UIManager.LookAndFeelInfo info[];
            info = UIManager.getInstalledLookAndFeels();

            //fill in the combobox model
            ArrayList<String> themeList = new ArrayList<String>();
            for (UIManager.LookAndFeelInfo i : info) {
                themeList.add(i.getName());
            }

            DefaultComboBoxModel model = new DefaultComboBoxModel(themeList.toArray());
            jComboBoxLook.setModel(model);
            //select the current
            LookAndFeel laf = UIManager.getLookAndFeel();
            int index = model.getIndexOf(laf.getName());
            jComboBoxLook.setSelectedIndex(index);
            ActionListener lst = new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {
                    String lafName = (String) jComboBoxLook.getModel().getElementAt(jComboBoxLook.getSelectedIndex());
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
                    Daten.system[Konstanten.SYSTEM_LOOK_NR] = lafClass;  //
                }
            };
            jComboBoxLook.addActionListener(lst);

        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    private void setUpIconList() {
        fillIconList();
        jComboBoxIcons.addActionListener(new BeobIcon());
    }

    private void fillIconList() {
        ArrayList<String> iconList = new ArrayList<String>();
        iconList.add(ICONSET_STANDARD);
        try {
            File[] files = new File(Funktionen.getPfadIcons()).listFiles();
            if (files != null) {
                for (int i = 0; i < files.length; i++) {
                    // System.out.print(files[i].getAbsolutePath());
                    if (files[i].isDirectory()) {
                        // iconList.add(files[i].getAbsolutePath());
                        iconList.add(files[i].getName());
                    }
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(636875409, Log.FEHLER_ART_PROG, "PanelEinstellungen", ex);
        }
        DefaultComboBoxModel model = new DefaultComboBoxModel(iconList.toArray());
        jComboBoxIcons.setModel(model);
        if (!Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_ICON_STANDARD_NR])) {
            if (!Daten.system[Konstanten.SYSTEM_ICON_PFAD_NR].equals("")) {
                File f = new File(Daten.system[Konstanten.SYSTEM_ICON_PFAD_NR]);
                jComboBoxIcons.setSelectedItem(f.getName());
            }
        }
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
        jCheckBox1 = new javax.swing.JCheckBox();
        jCheckBox2 = new javax.swing.JCheckBox();
        javax.swing.JPanel jPanel6 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel3 = new javax.swing.JLabel();
        jSpinnerDownload = new javax.swing.JSpinner();
        jCheckBoxEchtzeit = new javax.swing.JCheckBox();
        jButtonHilfeAnzahl = new javax.swing.JButton();
        jCheckBoxNotification = new javax.swing.JCheckBox();
        javax.swing.JPanel jPanel7 = new javax.swing.JPanel();
        jCheckBoxSuchen = new javax.swing.JCheckBox();
        jButtonSuchen = new javax.swing.JButton();
        jButtonInfos = new javax.swing.JButton();
        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        jComboBoxLook = new javax.swing.JComboBox<String>();
        jPanel1 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jComboBoxIcons = new javax.swing.JComboBox<String>();
        jButtonRefresh = new javax.swing.JButton();
        jPanel3 = new javax.swing.JPanel();
        jSliderBandbreite = new javax.swing.JSlider();
        jLabelBandbreite = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();

        jCheckBox1.setText("jCheckBox1");

        jCheckBox2.setText("jCheckBox2");

        jPanel6.setBorder(javax.swing.BorderFactory.createTitledBorder(""));

        jLabel3.setText("Download(s) gleichzeitig Laden");

        jSpinnerDownload.setModel(new javax.swing.SpinnerNumberModel(1, 1, 9, 1));

        jCheckBoxEchtzeit.setText("Echtzeitsuche im Filter");

        jButtonHilfeAnzahl.setText("Hilfe");

        jCheckBoxNotification.setText("Benachrichtigungen anzeigen");

        javax.swing.GroupLayout jPanel6Layout = new javax.swing.GroupLayout(jPanel6);
        jPanel6.setLayout(jPanel6Layout);
        jPanel6Layout.setHorizontalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jCheckBoxNotification)
                    .addGroup(jPanel6Layout.createSequentialGroup()
                        .addComponent(jSpinnerDownload, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel3)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonHilfeAnzahl))
                    .addComponent(jCheckBoxEchtzeit))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel6Layout.setVerticalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jCheckBoxEchtzeit)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jCheckBoxNotification)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel3)
                    .addComponent(jSpinnerDownload, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonHilfeAnzahl))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel7.setBorder(javax.swing.BorderFactory.createTitledBorder("Programmupdate"));

        jCheckBoxSuchen.setText("Einmal am Tag nach einer neuen Programmversion suchen");

        jButtonSuchen.setText("Jetzt suchen");

        jButtonInfos.setText("Programminfos anzeigen");

        javax.swing.GroupLayout jPanel7Layout = new javax.swing.GroupLayout(jPanel7);
        jPanel7.setLayout(jPanel7Layout);
        jPanel7Layout.setHorizontalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jCheckBoxSuchen)
                    .addGroup(jPanel7Layout.createSequentialGroup()
                        .addGap(21, 21, 21)
                        .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jButtonInfos)
                            .addComponent(jButtonSuchen))))
                .addContainerGap(48, Short.MAX_VALUE))
        );

        jPanel7Layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonInfos, jButtonSuchen});

        jPanel7Layout.setVerticalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addComponent(jCheckBoxSuchen)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonSuchen)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonInfos)
                .addContainerGap(26, Short.MAX_VALUE))
        );

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder("Look and Feel:"));

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jComboBoxLook, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jComboBoxLook, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(26, Short.MAX_VALUE))
        );

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("Icons"));

        jLabel1.setText("Um alle Icons richtig anzuzeigen, Programm neu starten");

        jButtonRefresh.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/icons_refresh_16.png"))); // NOI18N
        jButtonRefresh.setToolTipText("neue Icons suchen");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jComboBoxIcons, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonRefresh))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jComboBoxIcons, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonRefresh))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel1)
                .addContainerGap(25, Short.MAX_VALUE))
        );

        jPanel3.setBorder(javax.swing.BorderFactory.createTitledBorder("Bandbreite für Downloads"));

        jSliderBandbreite.setMaximum(1000);
        jSliderBandbreite.setMinorTickSpacing(10);
        jSliderBandbreite.setValue(0);

        jLabelBandbreite.setText("250");

        jLabel2.setText("Die Bandbreite direkter Downloads wird auf den Wert begrenzt");

        jLabel4.setText("bei \"0\" gibt es keine Begrenzung");

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addComponent(jSliderBandbreite, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabelBandbreite))
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel2)
                            .addComponent(jLabel4))
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jLabelBandbreite)
                    .addComponent(jSliderBandbreite, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jLabel2)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel4)
                .addContainerGap(27, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel2, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel7, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel6, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel6, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel7, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(16, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroup2;
    private javax.swing.JButton jButtonHilfeAnzahl;
    private javax.swing.JButton jButtonInfos;
    private javax.swing.JButton jButtonRefresh;
    private javax.swing.JButton jButtonSuchen;
    private javax.swing.JCheckBox jCheckBox1;
    private javax.swing.JCheckBox jCheckBox2;
    private javax.swing.JCheckBox jCheckBoxEchtzeit;
    private javax.swing.JCheckBox jCheckBoxNotification;
    private javax.swing.JCheckBox jCheckBoxSuchen;
    private javax.swing.JComboBox<String> jComboBoxIcons;
    private javax.swing.JComboBox<String> jComboBoxLook;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabelBandbreite;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JSlider jSliderBandbreite;
    private javax.swing.JSpinner jSpinnerDownload;
    // End of variables declaration//GEN-END:variables

    private class BeobSpinnerDownload implements ChangeListener {

        @Override
        public void stateChanged(ChangeEvent arg0) {
            Daten.system[Konstanten.SYSTEM_MAX_DOWNLOAD_NR] =
                    String.valueOf(((Number) jSpinnerDownload.getModel().getValue()).intValue());
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_ANZAHL_DOWNLOADS, PanelEinstellungen.class.getSimpleName());
        }
    }

    private class BeobCheckBox implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            Daten.system[Konstanten.SYSTEM_ECHTZEITSUCHE_NR] = Boolean.toString(jCheckBoxEchtzeit.isSelected());
        }
    }

    private class BeobCheckBoxSuchen implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            Daten.system[Konstanten.SYSTEM_UPDATE_SUCHEN_NR] = Boolean.toString(jCheckBoxSuchen.isSelected());
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

    private class BeobIcon implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            String iconName = jComboBoxIcons.getModel().getElementAt(jComboBoxIcons.getSelectedIndex()).toString();
            if (iconName.equals(ICONSET_STANDARD)) {
                Daten.system[Konstanten.SYSTEM_ICON_STANDARD_NR] = Boolean.TRUE.toString();
                Daten.system[Konstanten.SYSTEM_ICON_PFAD_NR] = "";
            } else {
                Daten.system[Konstanten.SYSTEM_ICON_STANDARD_NR] = Boolean.FALSE.toString();
            }
            try {
                File[] files = new File(Funktionen.getPfadIcons()).listFiles();
                if (files != null) {
                    for (int i = 0; i < files.length; i++) {
                        if (files[i].isDirectory() && files[i].getName().equals(iconName)) {
                            Daten.system[Konstanten.SYSTEM_ICON_PFAD_NR] = files[i].getAbsolutePath();
                            break;
                        }
                    }
                }
            } catch (Exception ex) {
                Daten.system[Konstanten.SYSTEM_ICON_STANDARD_NR] = Boolean.TRUE.toString();
                Daten.system[Konstanten.SYSTEM_ICON_PFAD_NR] = "";
                Log.fehlerMeldung(829304789, Log.FEHLER_ART_PROG, "PanelEinstellungen", ex);
            }
        }
    }
}
