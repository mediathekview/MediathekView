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

import java.awt.Component;
import java.awt.Frame;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import javax.swing.DefaultComboBoxModel;
import javax.swing.LookAndFeel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import mediathek.controller.io.ProgrammUpdateSuchen;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.gui.PanelVorlage;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;

public class PanelEinstellungen extends PanelVorlage {

    public PanelEinstellungen(DDaten d, Component parentComponent) {
        super(d, parentComponent);
        initComponents();
        ddaten = d;
        init();
        jCheckBoxEchtzeit.addActionListener(new BeobCheckBox());
        jSpinnerDownload.addChangeListener(new BeobSpinnerDownload());
//        String[] theme = new String[GuiFunktionen.THEME.length];
//        for (int i = 0; i < GuiFunktionen.THEME.length; ++i) {
//            theme[i] = GuiFunktionen.THEME[i][0];
//        }
//        jComboBoxLook.setModel(new DefaultComboBoxModel(theme));
//        if (Daten.system[Konstanten.SYSTEM_LOOK_NR].equals("")) {
//            Daten.system[Konstanten.SYSTEM_LOOK_NR] = "0";
//        }
//        jComboBoxLook.setSelectedIndex(Integer.parseInt(Daten.system[Konstanten.SYSTEM_LOOK_NR]));
//        jComboBoxLook.addActionListener(new BeobLook());

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
        jCheckBoxSuchen.addActionListener(new BeobCheckBoxSuchen());
        jButtonSuchen.addActionListener(new BeobSuchen(false));
        jButtonInfos.addActionListener(new BeobSuchen(true));
    }

    private void init() {
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
                        SwingUtilities.updateComponentTreeUI(ddaten.mediathekGui);
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

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.ButtonGroup buttonGroup1 = new javax.swing.ButtonGroup();
        javax.swing.JPanel jPanel6 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel3 = new javax.swing.JLabel();
        jSpinnerDownload = new javax.swing.JSpinner();
        jCheckBoxEchtzeit = new javax.swing.JCheckBox();
        jButtonHilfeAnzahl = new javax.swing.JButton();
        javax.swing.JPanel jPanel7 = new javax.swing.JPanel();
        jCheckBoxSuchen = new javax.swing.JCheckBox();
        jButtonSuchen = new javax.swing.JButton();
        jButtonInfos = new javax.swing.JButton();
        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        jComboBoxLook = new javax.swing.JComboBox();

        jPanel6.setBorder(javax.swing.BorderFactory.createTitledBorder(""));

        jLabel3.setText("Download(s) gleichzeitig Laden");

        jSpinnerDownload.setModel(new javax.swing.SpinnerNumberModel(1, 1, 9, 1));

        jCheckBoxEchtzeit.setText("Echtzeitsuche im Suchfeld \"Titel/Thema\"");

        jButtonHilfeAnzahl.setText("Hilfe");

        javax.swing.GroupLayout jPanel6Layout = new javax.swing.GroupLayout(jPanel6);
        jPanel6.setLayout(jPanel6Layout);
        jPanel6Layout.setHorizontalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
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
                .addGap(18, 18, 18)
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
                .addContainerGap(44, Short.MAX_VALUE))
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
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder("Look and Feel:"));

        jComboBoxLook.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));

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
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel7, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel6, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel6, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jPanel7, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(82, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonHilfeAnzahl;
    private javax.swing.JButton jButtonInfos;
    private javax.swing.JButton jButtonSuchen;
    private javax.swing.JCheckBox jCheckBoxEchtzeit;
    private javax.swing.JCheckBox jCheckBoxSuchen;
    private javax.swing.JComboBox jComboBoxLook;
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

//    private class BeobLook implements ActionListener {
//
//        @Override
//        public void actionPerformed(ActionEvent e) {
//            if (GuiFunktionen.setLook(ddaten.mediathekGui, Integer.parseInt(String.valueOf(jComboBoxLook.getSelectedIndex())))) {
//                Daten.system[Konstanten.SYSTEM_LOOK_NR] = String.valueOf(jComboBoxLook.getSelectedIndex());
//            } else {
//                Daten.system[Konstanten.SYSTEM_LOOK_NR] = "0";
//                jComboBoxLook.setSelectedIndex(0);
//                GuiFunktionen.setLook(ddaten.mediathekGui, 0);
//            }
//
//        }
//    }

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
            new ProgrammUpdateSuchen().checkVersion(ddaten, !infos /* bei aktuell anzeigen */, infos /* Hinweis */, true /* hinweiseAlleAnzeigen */);
        }
    }
}
