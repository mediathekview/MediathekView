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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JPanel;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeSelectionModel;
import mediathek.Log;
import mediathek.daten.DDaten;
import mediathek.gui.beobachter.EscBeenden;

public class DialogEinstellungen extends javax.swing.JDialog {

    DDaten ddaten;
    public boolean ok = false;
    private PanelEinstellungen panelEinstellungen;
    private PanelEinstellungenNetz panelEinstellungenNetz;
    private PanelUpdate panelUpdate;
    private PanelFilmlisteLaden panelImportFilme;
    private PanelExportFilmliste panelExportFilmliste;
    private PanelSenderLaden panelSenderLaden;
    private PanelBlacklist panelBlacklist;
    private PanelHistory panelHistory;
    private PanelLogfile panelLogfile;
    private PanelPgruppen panelPgruppen;
    private PanelImportStandardPgruppe panelImportStandardPgruppe;
    private PanelImportPgruppe panelImportProgramme;
    // Infos
    private PanelInfo panelInfo;
    private PanelInfoStarts panelStarts;
    private PanelMeldungen panelMeldungenFehler;
    private PanelMeldungen panelMeldungenSystem;
    private PanelMeldungen panelMeldungenPlayer;
    private JPanel panelLeer = new JPanel();

    /**
     *
     * @param parent
     * @param modal
     * @param d
     * @param gguiFilme
     */
    public DialogEinstellungen(java.awt.Frame parent, boolean modal, DDaten d) {
        super(parent, modal);
        initComponents();
        setTitle("Programmeinstellungen");
        ddaten = d;
        init();
        initTree();
        this.pack();
        jButtonBeenden.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                beenden();
            }
        });
        new EscBeenden(this) {

            @Override
            public void beenden_() {
                beenden();
            }
        };
    }

    private void init() {
        panelEinstellungen = new PanelEinstellungen(ddaten);
        panelEinstellungenNetz = new PanelEinstellungenNetz(ddaten);
        panelUpdate = new PanelUpdate(ddaten);
        panelImportFilme = new PanelFilmlisteLaden(ddaten);
        panelExportFilmliste = new PanelExportFilmliste(ddaten);
        panelSenderLaden = new PanelSenderLaden(ddaten);
        panelBlacklist = new PanelBlacklist(ddaten);
        panelHistory = new PanelHistory(ddaten);
        panelLogfile = new PanelLogfile(ddaten);
        panelPgruppen = new PanelPgruppen(ddaten);
        panelImportStandardPgruppe = new PanelImportStandardPgruppe(ddaten);
        panelImportProgramme = new PanelImportPgruppe(ddaten);
        // Infos
        panelInfo = new PanelInfo(ddaten);
        panelStarts = new PanelInfoStarts(ddaten);
        panelMeldungenFehler = new PanelMeldungen(ddaten, Log.LOG_FEHLER, "Fehlermeldungen");
        panelMeldungenSystem = new PanelMeldungen(ddaten, Log.LOG_SYSTEM, "Systemmeldungen");
        panelMeldungenPlayer = new PanelMeldungen(ddaten, Log.LOG_PLAYER, "Meldungen Videoplayer");
    }

    private void initTree() {
        final String NAME_allgemeineEinstellungen = "Allgemein";
        final String NAME_netzwerk = "Netzwerk";
        final String NAME_update = "Update";
        final String NAME_filmListeLaden = "Filmliste laden";
        final String NAME_senderLaden = "Sender aktualisieren";
        final String NAME_filmListeExportieren = "Filmliste exportieren";
        final String NAME_blacklist = "Blacklist";
        final String NAME_programme = "Programmgruppen";
        final String NAME_programmeStandard = "Standardgruppen importieren";
        final String NAME_programmeImportieren = "Importieren";
        // Infos
        final String NAME_allgemeineInfos = "Pfade";
        final String NAME_infosStarts = "laufende Programme";
        final String NAME_history = "History";
        final String NAME_logfile = "erledigte Abos";
        final String NAME_systemmeldungen = "Systemmeldungen";
        final String NAME_fehlermeldungen = "Fehlermeldungen";
        final String NAME_meldungenProgramme = "Programmeldungen";
        //
        DefaultMutableTreeNode treeNodeStart = new DefaultMutableTreeNode("Programm");
        // ######## Einstellulngen ############
        DefaultMutableTreeNode treeNodeEinstellungen = new DefaultMutableTreeNode("Einstellungen");
        // allgemeine Einstellungen
        DefaultMutableTreeNode treeNodeAllgemeineEinstellungen = new DefaultMutableTreeNode(NAME_allgemeineEinstellungen);
        treeNodeEinstellungen.add(treeNodeAllgemeineEinstellungen);
        DefaultMutableTreeNode treeNodeNetzwerk = new DefaultMutableTreeNode(NAME_netzwerk);
        treeNodeEinstellungen.add(treeNodeNetzwerk);
        DefaultMutableTreeNode treeNodeUpdate = new DefaultMutableTreeNode(NAME_update);
        treeNodeEinstellungen.add(treeNodeUpdate);
        treeNodeStart.add(treeNodeEinstellungen);
        // ######## Filme ###############
        DefaultMutableTreeNode treeNodeFilme = new DefaultMutableTreeNode("Filme");
        DefaultMutableTreeNode treeNodeFilmliste = new DefaultMutableTreeNode(NAME_filmListeLaden);
        treeNodeFilme.add(treeNodeFilmliste);
        DefaultMutableTreeNode treeNodeSenderLaden = new DefaultMutableTreeNode(NAME_senderLaden);
        treeNodeFilme.add(treeNodeSenderLaden);
        DefaultMutableTreeNode treeNodeFilmlisteExport = new DefaultMutableTreeNode(NAME_filmListeExportieren);
        treeNodeFilme.add(treeNodeFilmlisteExport);
        DefaultMutableTreeNode treeNodeBlacklist = new DefaultMutableTreeNode(NAME_blacklist);
        treeNodeFilme.add(treeNodeBlacklist);
        treeNodeStart.add(treeNodeFilme);
        // ########### Programme ##############
        DefaultMutableTreeNode treeNodeDownloads = new DefaultMutableTreeNode("Videoplayer");
        DefaultMutableTreeNode treeNodeProgramme = new DefaultMutableTreeNode(NAME_programme);
        treeNodeDownloads.add(treeNodeProgramme);
        DefaultMutableTreeNode treeNodeImportProgramme = new DefaultMutableTreeNode(NAME_programmeImportieren);
        treeNodeDownloads.add(treeNodeImportProgramme);
        DefaultMutableTreeNode treeNodeImportStandard = new DefaultMutableTreeNode(NAME_programmeStandard);
        treeNodeDownloads.add(treeNodeImportStandard);
        treeNodeStart.add(treeNodeDownloads);
        // ####### Infos #########
        DefaultMutableTreeNode treeNodeInfos = new DefaultMutableTreeNode("Infos");
        DefaultMutableTreeNode treeNodeAllgemeineInfos = new DefaultMutableTreeNode(NAME_allgemeineInfos);
        treeNodeInfos.add(treeNodeAllgemeineInfos);
        DefaultMutableTreeNode treeNodeInfosStarts = new DefaultMutableTreeNode(NAME_infosStarts);
        treeNodeInfos.add(treeNodeInfosStarts);
        DefaultMutableTreeNode treeNodeHistory = new DefaultMutableTreeNode(NAME_history);
        treeNodeInfos.add(treeNodeHistory);
        DefaultMutableTreeNode treeNodeLogfile = new DefaultMutableTreeNode(NAME_logfile);
        treeNodeInfos.add(treeNodeLogfile);
        treeNodeStart.add(treeNodeInfos);
        // ############ Systemmeldungen ###############
        DefaultMutableTreeNode treeNodeSystem = new DefaultMutableTreeNode("Meldungen");
        DefaultMutableTreeNode treeNodeSystemmeldungen = new DefaultMutableTreeNode(NAME_systemmeldungen);
        treeNodeSystem.add(treeNodeSystemmeldungen);
        DefaultMutableTreeNode treeNodeFehlermeldungen = new DefaultMutableTreeNode(NAME_fehlermeldungen);
        treeNodeSystem.add(treeNodeFehlermeldungen);
        DefaultMutableTreeNode treeNodeProgrammmeldungen = new DefaultMutableTreeNode(NAME_meldungenProgramme);
        treeNodeSystem.add(treeNodeProgrammmeldungen);
        treeNodeStart.add(treeNodeSystem);
        // Aufbauen
        jTree1.setModel(new javax.swing.tree.DefaultTreeModel(treeNodeStart));
        jTree1.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);


        jTree1.addTreeSelectionListener(new TreeSelectionListener() {

            @Override
            public void valueChanged(TreeSelectionEvent e) {
                DefaultMutableTreeNode node = (DefaultMutableTreeNode) jTree1.getLastSelectedPathComponent();
                if (node == null) {
                    // nix markiert
                    jPanelExtra.removeAll();
                    jPanelExtra.add(panelLeer);
                } else {
                    String name = node.getUserObject().toString();
                    setTitle(name);
                    if (name.equals(NAME_allgemeineEinstellungen)) {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelEinstellungen);
                    } else if (name.equals(NAME_netzwerk)) {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelEinstellungenNetz);
                    } else if (name.equals(NAME_update)) {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelUpdate);
                    } else if (name.equals(NAME_filmListeLaden)) {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelImportFilme);
                    } else if (name.equals(NAME_senderLaden)) {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelSenderLaden);
                    } else if (name.equals(NAME_filmListeExportieren)) {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelExportFilmliste);
                    } else if (name.equals(NAME_blacklist)) {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelBlacklist);
                    } else if (name.equals(NAME_history)) {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelHistory);
                    } else if (name.equals(NAME_logfile)) {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelLogfile);
                    } else if (name.equals(NAME_programme)) {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelPgruppen);
                    } else if (name.equals(NAME_programmeImportieren)) {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelImportProgramme);
                    } else if (name.equals(NAME_programmeStandard)) {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelImportStandardPgruppe);
                    } else if (name.equals(NAME_allgemeineInfos)) {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelInfo);
                    } else if (name.equals(NAME_infosStarts)) {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelStarts);
                    } else if (name.equals(NAME_systemmeldungen)) {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelMeldungenSystem);
                    } else if (name.equals(NAME_fehlermeldungen)) {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelMeldungenFehler);
                    } else if (name.equals(NAME_meldungenProgramme)) {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelMeldungenPlayer);
                    } else {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelLeer);
                        setTitle("Programmeinstellungen");
                    }
                }
                jPanelExtra.updateUI();
            }
        });


    }

    private void beenden() {
        this.dispose();
        ddaten.setGeaendertPanelSofort();
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        buttonGroup1 = new javax.swing.ButtonGroup();
        jButtonBeenden = new javax.swing.JButton();
        jSplitPane1 = new javax.swing.JSplitPane();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTree1 = new javax.swing.JTree();
        jScrollPane2 = new javax.swing.JScrollPane();
        jPanelExtra = new javax.swing.JPanel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        jButtonBeenden.setText("Schließen");

        jSplitPane1.setBorder(javax.swing.BorderFactory.createEtchedBorder());
        jSplitPane1.setDividerLocation(200);
        jSplitPane1.setContinuousLayout(true);
        jSplitPane1.setOneTouchExpandable(true);

        jScrollPane1.setViewportView(jTree1);

        jSplitPane1.setLeftComponent(jScrollPane1);

        jPanelExtra.setLayout(new java.awt.BorderLayout());
        jScrollPane2.setViewportView(jPanelExtra);

        jSplitPane1.setRightComponent(jScrollPane2);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jSplitPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 975, Short.MAX_VALUE)
                    .addGroup(layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jButtonBeenden)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jSplitPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 554, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonBeenden)
                .addGap(6, 6, 6))
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents
    /**
     * @param args the command line arguments
     */
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JButton jButtonBeenden;
    private javax.swing.JPanel jPanelExtra;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JTree jTree1;
    // End of variables declaration//GEN-END:variables
}
