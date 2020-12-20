package mediathek.gui.dialogEinstellungen;

import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.gui.PanelVorlage;
import mediathek.gui.dialogEinstellungen.allgemein.PanelEinstellungen;
import mediathek.mainwindow.MediathekGui;
import mediathek.res.GetIcon;
import mediathek.tool.EscapeKeyHandler;
import mediathek.tool.GuiFunktionen;

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

public class DialogEinstellungen extends JFrame {
    private final Daten daten;
    public boolean ok;
    private PanelEinstellungen panelEinstellungen;
    private PanelDownload panelDownload;
    private PanelMediaDB panelMediaDB;
    private PanelEinstellungenErweitert panelEinstellungenErweitert;
    private PanelEinstellungenGeo panelEinstellungenGeo;
    private PanelEinstellungenColor panelEinstellungenColor;
    private PanelFilmlisteLaden panelImportFilme;
    private PanelBlacklist panelBlacklist;
    private PanelDateinamen panelDateinamen;
    private PanelVorlage panelPset;
    private PanelPsetImport panelPsetVorlagen;
    private JPanel panelNotifications;
    private final JPanel panelLeer = new JPanel();

    private static final String NAME_einstellungen = "Einstellungen";
    private static final String NAME_allgemeineEinstellungen = "Allgemein";
    private static final String NAME_notifications = "Benachrichtigungen";
    private static final String NAME_bandwidth = "Download";
    private static final String NAME_mediaDB = "Mediensammlung";
    private static final String NAME_allgemeineEinstellungenErweitert = "Erweitert";
    private static final String NAME_allgemeineEinstellungenGeo = "Standort & Geoblocking";
    private static final String NAME_allgemeineEinstellungenColor = "Farben";
    private static final String NAME_filmListe = "Filmliste";
    private static final String NAME_filmListeLaden = "Filmliste laden";
    private static final String NAME_blacklist = "Blacklist";
    private static final String NAME_aufzeichnen = "Aufzeichnen und Abspielen";
    private static final String NAME_dateiname = "Datei- und Pfadnamen";
    private static final String NAME_programmset = "Set bearbeiten";
    private static final String NAME_programmsetImportieren = "Set importieren";

    // ######## Einstellungen ############
    private final DefaultMutableTreeNode treeNodeAllgemeineEinstellungen = new DefaultMutableTreeNode(NAME_allgemeineEinstellungen);
    private final DefaultMutableTreeNode treeNodeNotifications = new DefaultMutableTreeNode(NAME_notifications);
    private final DefaultMutableTreeNode treeNodeAllgemeineEinstellungenEreweitert = new DefaultMutableTreeNode(NAME_allgemeineEinstellungenErweitert);
    private final DefaultMutableTreeNode treeNodeAllgemeineEinstellungenGeo = new DefaultMutableTreeNode(NAME_allgemeineEinstellungenGeo);
    private final DefaultMutableTreeNode treeNodeAllgemeineEinstellungenColor = new DefaultMutableTreeNode(NAME_allgemeineEinstellungenColor);
    // ######## Filme ###############
    private final DefaultMutableTreeNode treeNodeFilmliste = new DefaultMutableTreeNode(NAME_filmListeLaden);
    private final DefaultMutableTreeNode treeNodeBlacklist = new DefaultMutableTreeNode(NAME_blacklist);
    // ########### Programme ##############
    private final DefaultMutableTreeNode treeNodeBandwidth = new DefaultMutableTreeNode(NAME_bandwidth);
    private final DefaultMutableTreeNode treeNodeMediaDB = new DefaultMutableTreeNode(NAME_mediaDB);

    private final DefaultMutableTreeNode treeNodeDateinamen = new DefaultMutableTreeNode(NAME_dateiname);
    private final DefaultMutableTreeNode treeNodeProgramme = new DefaultMutableTreeNode(NAME_programmset);
    private final DefaultMutableTreeNode treeNodeImportProgramme = new DefaultMutableTreeNode(NAME_programmsetImportieren);

    public DialogEinstellungen() {
        initComponents();
        daten = Daten.getInstance();

        initPanels();
        initTree();

        restoreSizeFromConfig();

        setIconImage(GetIcon.getIcon("MediathekView.png", "/mediathek/res/", 58, 58).getImage());
        jButtonBeenden.addActionListener(e -> beenden());

        EscapeKeyHandler.installHandler(this, this::beenden);
    }

    private void restoreSizeFromConfig() {
        int breite = 0,
                hoehe = 0,
                posX = 0,
                posY = 0;

        String[] arr = MVConfig.get(MVConfig.Configs.SYSTEM_GROESSE_EINSTELLUNGEN).split(":");
        try {
            if (arr.length == 4) {
                breite = Integer.parseInt(arr[0]);
                hoehe = Integer.parseInt(arr[1]);
                posX = Integer.parseInt(arr[2]);
                posY = Integer.parseInt(arr[3]);
            }
        } catch (Exception ex) {
            breite = 0;
            hoehe = 0;
            posX = 0;
            posY = 0;
        }

        if (breite > 0 && hoehe > 0) {
            setSize(breite, hoehe);
        }

        if (posX > 0 && posY > 0) {
            setLocation(posX, posY);
        } else {
            final var parentFrame = MediathekGui.ui();
            if (parentFrame != null)
                setLocationRelativeTo(parentFrame);
        }
    }

    private void initPanels() {
        panelEinstellungen = new PanelEinstellungen(daten);
        panelDownload = new PanelDownload();
        panelMediaDB = new PanelMediaDB(daten, this);
        panelEinstellungenErweitert = new PanelEinstellungenErweitert();
        panelEinstellungenGeo = new PanelEinstellungenGeo(this);
        panelEinstellungenColor = new PanelEinstellungenColor();
        panelImportFilme = new PanelFilmlisteLaden(true);
        panelBlacklist = new PanelBlacklist(daten, this, PanelBlacklist.class.getName());
        panelDateinamen = new PanelDateinamen(daten, this);
        panelPset = new PanelPset(daten, this);
        panelPsetVorlagen = new PanelPsetImport(daten, this);

        panelNotifications = new PanelNotifications();
    }

    private void initTree() {
        DefaultMutableTreeNode treeNodeStart = new DefaultMutableTreeNode(Konstanten.PROGRAMMNAME);
        // ######## Einstellungen ############
        final DefaultMutableTreeNode treeNodeEinstellungen = new DefaultMutableTreeNode("Einstellungen");
        treeNodeEinstellungen.add(treeNodeAllgemeineEinstellungen);
        treeNodeEinstellungen.add(treeNodeNotifications);
        treeNodeEinstellungen.add(treeNodeAllgemeineEinstellungenEreweitert);
        treeNodeEinstellungen.add(treeNodeAllgemeineEinstellungenGeo);
        treeNodeEinstellungen.add(treeNodeAllgemeineEinstellungenColor);
        treeNodeEinstellungen.add(treeNodeMediaDB);
        treeNodeStart.add(treeNodeEinstellungen);

        // ######## Filme ###############
        final DefaultMutableTreeNode treeNodeFilme = new DefaultMutableTreeNode("Filmliste");
        treeNodeFilme.add(treeNodeFilmliste);
        treeNodeFilme.add(treeNodeBlacklist);
        treeNodeStart.add(treeNodeFilme);

        // ########### Programme ##############
        final DefaultMutableTreeNode treeNodeDownload = new DefaultMutableTreeNode("Aufzeichnen und Abspielen");
        treeNodeDownload.add(treeNodeDateinamen);
        treeNodeDownload.add(treeNodeBandwidth);
        treeNodeDownload.add(treeNodeProgramme);
        treeNodeDownload.add(treeNodeImportProgramme);
        treeNodeStart.add(treeNodeDownload);

        // Aufbauen
        jTree1.setModel(new DefaultTreeModel(treeNodeStart));
        jTree1.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        jTree1.setRootVisible(false);
        jTree1.addTreeSelectionListener(e -> {
            DefaultMutableTreeNode node = (DefaultMutableTreeNode) jTree1.getLastSelectedPathComponent();
            if (node == null) {
                // nix markiert
                jPanelExtra.removeAll();
                jPanelExtra.add(panelLeer);
            } else {
                String name1 = node.getUserObject().toString();
                setTitle(name1);
                switch (name1) {
                    case NAME_einstellungen -> jTree1.setSelectionPath(new TreePath(treeNodeAllgemeineEinstellungen.getPath()));
                    case NAME_notifications -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelNotifications);
                    }
                    case NAME_bandwidth -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelDownload);
                    }
                    case NAME_mediaDB -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelMediaDB);
                    }
                    case NAME_allgemeineEinstellungen -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelEinstellungen);
                    }
                    case NAME_allgemeineEinstellungenErweitert -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelEinstellungenErweitert);
                    }
                    case NAME_allgemeineEinstellungenGeo -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelEinstellungenGeo);
                    }
                    case NAME_allgemeineEinstellungenColor -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelEinstellungenColor);
                    }
                    case NAME_filmListe -> jTree1.setSelectionPath(new TreePath(treeNodeFilmliste.getPath()));
                    case NAME_filmListeLaden -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelImportFilme);
                    }
                    case NAME_blacklist -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelBlacklist);
                    }
                    case NAME_aufzeichnen -> jTree1.setSelectionPath(new TreePath(treeNodeDateinamen.getPath()));
                    case NAME_dateiname -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelDateinamen);
                    }
                    case NAME_programmset -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelPset);
                    }
                    case NAME_programmsetImportieren -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelPsetVorlagen);
                    }
                    default -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(panelLeer);
                        setTitle("Programmeinstellungen");
                    }
                }
            }
            jPanelExtra.updateUI();
        });
        // und jetzt noch aufklappen
        for (int i = 0; i < jTree1.getRowCount(); ++i)
            jTree1.expandRow(i);

        // und den Start setzen
        TreePath tp = new TreePath(treeNodeAllgemeineEinstellungen.getPath());
        jTree1.setSelectionPath(tp);
    }

    private void beenden() {
        //save the dialog size when we are closing...
        GuiFunktionen.getSize(MVConfig.Configs.SYSTEM_GROESSE_EINSTELLUNGEN, this);

        daten.allesSpeichern();
        dispose();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jButtonBeenden = new javax.swing.JButton();
        javax.swing.JSplitPane jSplitPane1 = new javax.swing.JSplitPane();
        javax.swing.JScrollPane jScrollPane2 = new javax.swing.JScrollPane();
        jPanelExtra = new javax.swing.JPanel();
        javax.swing.JScrollPane jScrollPane1 = new javax.swing.JScrollPane();
        jTree1 = new javax.swing.JTree();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Einstellungen");

        jButtonBeenden.setText("Schließen");

        jSplitPane1.setBorder(javax.swing.BorderFactory.createEtchedBorder());
        jSplitPane1.setDividerLocation(250);
        jSplitPane1.setContinuousLayout(true);
        jSplitPane1.setOneTouchExpandable(true);

        jPanelExtra.setLayout(new java.awt.BorderLayout());
        jScrollPane2.setViewportView(jPanelExtra);

        jSplitPane1.setRightComponent(jScrollPane2);

        jTree1.setBorder(javax.swing.BorderFactory.createEmptyBorder(5, 5, 5, 5));
        jTree1.setRootVisible(false);
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
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonBeenden;
    private javax.swing.JPanel jPanelExtra;
    private javax.swing.JTree jTree1;
    // End of variables declaration//GEN-END:variables
}
