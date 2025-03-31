package mediathek.gui.dialogEinstellungen;

import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.gui.dialogEinstellungen.allgemein.PanelEinstellungen;
import mediathek.mainwindow.MediathekGui;
import mediathek.res.GetIcon;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.EscapeKeyHandler;
import org.apache.commons.configuration2.sync.LockMode;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
import java.awt.*;

public class DialogEinstellungen extends JFrame {
    private final Daten daten;
    public boolean ok;

    private static final String NAME_einstellungen = "Einstellungen";
    private static final String NAME_allgemeineEinstellungen = "Allgemein";
    private static final String NAME_notifications = "Benachrichtigungen";
    private static final String NAME_bandwidth = "Download";
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

    private final DefaultMutableTreeNode treeNodeDateinamen = new DefaultMutableTreeNode(NAME_dateiname);
    private final DefaultMutableTreeNode treeNodeProgramme = new DefaultMutableTreeNode(NAME_programmset);
    private final DefaultMutableTreeNode treeNodeImportProgramme = new DefaultMutableTreeNode(NAME_programmsetImportieren);

    public DialogEinstellungen() {
        initComponents();
        daten = Daten.getInstance();

        initTree();

        restoreSizeFromConfig();

        setIconImage(GetIcon.getIcon("MediathekView.png", "/mediathek/res/", 58, 58).getImage());
        jButtonBeenden.addActionListener(e -> beenden());

        EscapeKeyHandler.installHandler(this, this::beenden);
    }

    private void restoreSizeFromConfig() {
        int width, height, x, y;
        final var config = ApplicationConfiguration.getConfiguration();

        config.lock(LockMode.READ);
        try {
            width = config.getInt(ApplicationConfiguration.SettingsDialog.WIDTH);
            height = config.getInt(ApplicationConfiguration.SettingsDialog.HEIGHT);
            x = config.getInt(ApplicationConfiguration.SettingsDialog.X);
            y = config.getInt(ApplicationConfiguration.SettingsDialog.Y);
        }
        catch (Exception e) {
            width = 0;
            height = 0;
            x = 0;
            y = 0;
        }
        finally {
            config.unlock(LockMode.READ);
        }

        if (width > 0 && height > 0) {
            setSize(width, height);
        }

        if (x > 0 && y > 0) {
            setLocation(x, y);
        } else {
            final var parentFrame = MediathekGui.ui();
            if (parentFrame != null)
                setLocationRelativeTo(parentFrame);
        }
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
                jPanelExtra.add(new JPanel());
            } else {
                String name1 = node.getUserObject().toString();
                setTitle(name1);
                switch (name1) {
                    case NAME_einstellungen -> jTree1.setSelectionPath(new TreePath(treeNodeAllgemeineEinstellungen.getPath()));
                    case NAME_notifications -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(new PanelNotifications());
                    }
                    case NAME_bandwidth -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(new PanelDownload());
                    }
                    case NAME_allgemeineEinstellungen -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(new PanelEinstellungen());
                    }
                    case NAME_allgemeineEinstellungenErweitert -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(new PanelEinstellungenErweitert());
                    }
                    case NAME_allgemeineEinstellungenGeo -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(new PanelEinstellungenGeo(this));
                    }
                    case NAME_allgemeineEinstellungenColor -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(new PanelEinstellungenColor());
                    }
                    case NAME_filmListe -> jTree1.setSelectionPath(new TreePath(treeNodeFilmliste.getPath()));
                    case NAME_filmListeLaden -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(new PanelFilmlisteLaden(true));
                    }
                    case NAME_blacklist -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(new PanelBlacklist(daten, this, PanelBlacklist.class.getName()));
                    }
                    case NAME_aufzeichnen -> jTree1.setSelectionPath(new TreePath(treeNodeDateinamen.getPath()));
                    case NAME_dateiname -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(new PanelDateinamen(daten, this));
                    }
                    case NAME_programmset -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(new PanelPset(this));
                    }
                    case NAME_programmsetImportieren -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(new PanelPsetImport(daten, this));
                    }
                    default -> {
                        jPanelExtra.removeAll();
                        jPanelExtra.add(new JPanel());
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

    private void storeSizeInConfig() {
        final var size = getSize();
        final var location = getLocation();
        final var config = ApplicationConfiguration.getConfiguration();

        config.lock(LockMode.WRITE);
        try {
            config.setProperty(ApplicationConfiguration.SettingsDialog.WIDTH, size.width);
            config.setProperty(ApplicationConfiguration.SettingsDialog.HEIGHT, size.height);
            config.setProperty(ApplicationConfiguration.SettingsDialog.X, location.x);
            config.setProperty(ApplicationConfiguration.SettingsDialog.Y, location.y);
        }
        finally {
            config.unlock(LockMode.WRITE);
        }
    }

    private void beenden() {
        storeSizeInConfig();

        daten.allesSpeichern();
        dispose();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        jButtonBeenden = new JButton();
        var jSplitPane1 = new JSplitPane();
        var jScrollPane2 = new JScrollPane();
        jPanelExtra = new JPanel();
        var jScrollPane1 = new JScrollPane();
        jTree1 = new JTree();

        //======== this ========
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Einstellungen"); //NON-NLS
        var contentPane = getContentPane();

        //---- jButtonBeenden ----
        jButtonBeenden.setText("Schlie\u00dfen"); //NON-NLS

        //======== jSplitPane1 ========
        {
            jSplitPane1.setBorder(new EtchedBorder());
            jSplitPane1.setDividerLocation(250);
            jSplitPane1.setContinuousLayout(true);
            jSplitPane1.setOneTouchExpandable(true);

            //======== jScrollPane2 ========
            {

                //======== jPanelExtra ========
                {
                    jPanelExtra.setLayout(new BorderLayout());
                }
                jScrollPane2.setViewportView(jPanelExtra);
            }
            jSplitPane1.setRightComponent(jScrollPane2);

            //======== jScrollPane1 ========
            {

                //---- jTree1 ----
                jTree1.setBorder(new EmptyBorder(5, 5, 5, 5));
                jTree1.setRootVisible(false);
                jScrollPane1.setViewportView(jTree1);
            }
            jSplitPane1.setLeftComponent(jScrollPane1);
        }

        GroupLayout contentPaneLayout = new GroupLayout(contentPane);
        contentPane.setLayout(contentPaneLayout);
        contentPaneLayout.setHorizontalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(contentPaneLayout.createSequentialGroup()
                    .addContainerGap()
                    .addGroup(contentPaneLayout.createParallelGroup(GroupLayout.Alignment.TRAILING)
                        .addComponent(jSplitPane1, GroupLayout.DEFAULT_SIZE, 1068, Short.MAX_VALUE)
                        .addGroup(contentPaneLayout.createSequentialGroup()
                            .addGap(12, 965, Short.MAX_VALUE)
                            .addComponent(jButtonBeenden)))
                    .addContainerGap())
        );
        contentPaneLayout.setVerticalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(contentPaneLayout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jSplitPane1, GroupLayout.DEFAULT_SIZE, 620, Short.MAX_VALUE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                    .addComponent(jButtonBeenden)
                    .addGap(6, 6, 6))
        );
        pack();
        setLocationRelativeTo(getOwner());
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JButton jButtonBeenden;
    private JPanel jPanelExtra;
    private JTree jTree1;
    // End of variables declaration//GEN-END:variables
}
