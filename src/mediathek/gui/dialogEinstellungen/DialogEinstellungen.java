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

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JPanel;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeSelectionModel;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.tool.EscBeenden;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;

public class DialogEinstellungen extends javax.swing.JDialog {

    DDaten ddaten;
    public boolean ok = false;
    private PanelEinstellungen panelEinstellungen;
    private PanelEinstellungenNetz panelEinstellungenNetz;
    private PanelFilmlisteLaden panelImportFilme;
    private PanelExportFilmliste panelExportFilmliste;
    private PanelSenderLaden panelSenderLaden;
    private PanelBlacklist panelBlacklist;
    private PanelErledigteUrls panelErledigteAbos;
    private PanelErledigteUrls panelHistory;
    private PanelPsetLang panelPset;
    private PanelPsetImport panelPsetVorlagen;
    // Infos
    private PanelInfo panelInfo;
    //private PanelInfoStarts panelStarts;
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
        setSize();
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

    private void setSize() {
        int breite, hoehe, posX, posY;
        try {
            breite = Integer.parseInt(Daten.system[Konstanten.SYSTEM_GROESSE_EINSTELLUNEN_X_NR]);
            hoehe = Integer.parseInt(Daten.system[Konstanten.SYSTEM_GROESSE_EINSTELLUNEN_Y_NR]);
            posX = Integer.parseInt(Daten.system[Konstanten.SYSTEM_POS_EINSTELLUNEN_X_NR]);
            posY = Integer.parseInt(Daten.system[Konstanten.SYSTEM_POS_EINSTELLUNEN_Y_NR]);
        } catch (Exception ex) {
            breite = 0;
            hoehe = 0;
            posX = 0;
            posY = 0;
        }
        if (breite > 0 && hoehe > 0) {
            this.setSize(new Dimension(breite, hoehe));
        }
        if (posX > 0 && posY > 0) {
            this.setLocation(posX, posY);
        } else {
            int x = ddaten.mediathekGui.getLocation().x;
            setLocationRelativeTo(ddaten.mediathekGui);
        }
    }

    private void init() {
        panelEinstellungen = new PanelEinstellungen(ddaten);
        panelEinstellungenNetz = new PanelEinstellungenNetz(ddaten);
        panelImportFilme = new PanelFilmlisteLaden(ddaten);
        panelExportFilmliste = new PanelExportFilmliste(ddaten);
        panelSenderLaden = new PanelSenderLaden(ddaten);
        panelBlacklist = new PanelBlacklist(ddaten);
        panelHistory = new PanelErledigteUrls(ddaten);
        panelHistory.initHistory();
        panelErledigteAbos = new PanelErledigteUrls(ddaten);
        panelErledigteAbos.initAbo();
        panelPset = new PanelPsetLang(ddaten);
        panelPsetVorlagen = new PanelPsetImport(ddaten);
        // Infos
        panelInfo = new PanelInfo(ddaten);
        //panelStarts = new PanelInfoStarts(ddaten);
        panelMeldungenFehler = new PanelMeldungen(ddaten, Log.textFehler, Log.LOG_FEHLER, "Fehlermeldungen");
        panelMeldungenSystem = new PanelMeldungen(ddaten, Log.textSystem, Log.LOG_SYSTEM, "Systemmeldungen");
        panelMeldungenPlayer = new PanelMeldungen(ddaten, Log.textProgramm, Log.LOG_PLAYER, "Meldungen Videoplayer");
    }

    private void initTree() {
        final String NAME_allgemeineEinstellungen = "Allgemein";
        final String NAME_netzwerk = "Netzwerk";
        final String NAME_filmListeLaden = "Filmliste laden";
        final String NAME_senderLaden = "Sender aktualisieren";
        final String NAME_filmListeExportieren = "Filmliste exportieren";
        final String NAME_blacklist = "Blacklist";
        final String NAME_programmset = "Programmsets";
        final String NAME_programmsetImportieren = "Set importieren";
        // Infos
        final String NAME_allgemeineInfos = "Pfade";
        //final String NAME_infosStarts = "laufende Programme";
        final String NAME_history = "History";
        final String NAME_logfile = "erledigte Abos";
        final String NAME_systemmeldungen = "Systemmeldungen";
        final String NAME_fehlermeldungen = "Fehlermeldungen";
        final String NAME_meldungenProgramme = "Programmausgabe";
        //
        DefaultMutableTreeNode treeNodeStart = new DefaultMutableTreeNode("Programm");
        // ######## Einstellulngen ############
        DefaultMutableTreeNode treeNodeEinstellungen = new DefaultMutableTreeNode("Einstellungen");
        // allgemeine Einstellungen
        DefaultMutableTreeNode treeNodeAllgemeineEinstellungen = new DefaultMutableTreeNode(NAME_allgemeineEinstellungen);
        treeNodeEinstellungen.add(treeNodeAllgemeineEinstellungen);
        DefaultMutableTreeNode treeNodeNetzwerk = new DefaultMutableTreeNode(NAME_netzwerk);
        treeNodeEinstellungen.add(treeNodeNetzwerk);
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
        DefaultMutableTreeNode treeNodeProgramme = new DefaultMutableTreeNode(NAME_programmset);
        treeNodeDownloads.add(treeNodeProgramme);
        DefaultMutableTreeNode treeNodeImportProgramme = new DefaultMutableTreeNode(NAME_programmsetImportieren);
        treeNodeDownloads.add(treeNodeImportProgramme);
        treeNodeStart.add(treeNodeDownloads);
        // ####### Infos #########
        DefaultMutableTreeNode treeNodeInfos = new DefaultMutableTreeNode("Infos");
        DefaultMutableTreeNode treeNodeAllgemeineInfos = new DefaultMutableTreeNode(NAME_allgemeineInfos);
        treeNodeInfos.add(treeNodeAllgemeineInfos);
        //DefaultMutableTreeNode treeNodeInfosStarts = new DefaultMutableTreeNode(NAME_infosStarts);
        //treeNodeInfos.add(treeNodeInfosStarts);
        DefaultMutableTreeNode treeNodeHistory = new DefaultMutableTreeNode(NAME_history);
        treeNodeInfos.add(treeNodeHistory);
        DefaultMutableTreeNode treeNodeLogfile = new DefaultMutableTreeNode(NAME_logfile);
        treeNodeInfos.add(treeNodeLogfile);
        treeNodeStart.add(treeNodeInfos);
        // ############ Systemmeldungen ###############
        DefaultMutableTreeNode treeNodeSystem = new DefaultMutableTreeNode("Meldungen");
        DefaultMutableTreeNode treeNodeSystemmeldungen = new DefaultMutableTreeNode(NAME_systemmeldungen);
        treeNodeSystem.add(treeNodeSystemmeldungen);
        if (DDaten.debug) {
            DefaultMutableTreeNode treeNodeFehlermeldungen = new DefaultMutableTreeNode(NAME_fehlermeldungen);
            treeNodeSystem.add(treeNodeFehlermeldungen);
        }
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
                        jPanelExtra.add(panelErledigteAbos);
                    } else if (name.equals(NAME_programmset)) {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelPset);
                    } else if (name.equals(NAME_programmsetImportieren)) {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelPsetVorlagen);
                    } else if (name.equals(NAME_allgemeineInfos)) {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelInfo);
                        //} else if (name.equals(NAME_infosStarts)) {
                        //jPanelExtra.removeAll();
                        //jPanelExtra.add(panelStarts);
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
        // und jetzt noch aufklappen
        for (int i = 0; i < jTree1.getRowCount(); ++i) {
            jTree1.expandRow(i);
        }
    }

    private void beenden() {
        ddaten.allesSpeichern();
        this.dispose();
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jButtonBeenden = new javax.swing.JButton();
        jSplitPane1 = new javax.swing.JSplitPane();
        jScrollPane2 = new javax.swing.JScrollPane();
        jPanelExtra = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTree1 = new javax.swing.JTree();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        jButtonBeenden.setText("Schließen");

        jSplitPane1.setBorder(javax.swing.BorderFactory.createEtchedBorder());
        jSplitPane1.setDividerLocation(200);
        jSplitPane1.setContinuousLayout(true);
        jSplitPane1.setOneTouchExpandable(true);

        jPanelExtra.setLayout(new java.awt.BorderLayout());
        jScrollPane2.setViewportView(jPanelExtra);

        jSplitPane1.setRightComponent(jScrollPane2);

        jScrollPane1.setViewportView(jTree1);

        jSplitPane1.setLeftComponent(jScrollPane1);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jSplitPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 1068, Short.MAX_VALUE)
                    .addGroup(layout.createSequentialGroup()
                        .addGap(12, 965, Short.MAX_VALUE)
                        .addComponent(jButtonBeenden)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jSplitPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 620, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonBeenden)
                .addGap(6, 6, 6))
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents
    /**
     * @param args the command line arguments
     */
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonBeenden;
    private javax.swing.JPanel jPanelExtra;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JTree jTree1;
    // End of variables declaration//GEN-END:variables
}
