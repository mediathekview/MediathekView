package mediathek.gui.abo;

import ca.odell.glazedlists.swing.GlazedListsSwing;
import mediathek.config.Daten;
import mediathek.daten.abo.AboTags;
import mediathek.daten.abo.DatenAbo;
import mediathek.gui.actions.CreateNewAboAction;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.gui.messages.AboListChangedEvent;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.*;
import mediathek.tool.cellrenderer.CellRendererAbo;
import mediathek.tool.listener.BeobTableHeader;
import mediathek.tool.models.TModelAbo;
import mediathek.tool.table.MVAbosTable;
import mediathek.tool.table.MVTable;
import net.engio.mbassy.listener.Handler;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jdesktop.swingx.JXStatusBar;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

public class ManageAboPanel extends JPanel {
    private static final String ACTION_MAP_KEY_EDIT_ABO = "edit_abo";
    private static final String ACTION_MAP_KEY_DELETE_ABO = "delete_abo";
    private static final Logger logger = LogManager.getLogger();
    private final MVTable tabelle = new MVAbosTable();
    private final Daten daten;
    private final CreateNewAboAction createAboAction = new CreateNewAboAction(Daten.getInstance().getListeAbo());
    private final JXStatusBar infoPanel = new JXStatusBar();
    private final JLabel totalAbos = new JLabel("totalAbos");
    private final JLabel activeAbos = new JLabel("activeAbos");
    private final JLabel inactiveAbos = new JLabel("inactiveAbos");
    private final JToolBar swingToolBar = new JToolBar();
    private final JComboBox<String> senderCombo = new JComboBox<>();
    private final InfiniteProgressPanel infiniteProgressPanel = new InfiniteProgressPanel();
    private JScrollPane jScrollPane1;

    public ManageAboPanel(@NotNull JDialog dialog) {
        daten = Daten.getInstance();

        initComponents();
        jScrollPane1.setViewportView(tabelle);

        setupToolBar();
        setupInfoPanel();
        updateInfoText();

        MessageBus.getMessageBus().subscribe(this);

        initListeners();

        initializeTable();

        dialog.setGlassPane(infiniteProgressPanel);
    }

    public void addObjectData(TModelAbo model, String sender) {
        model.setRowCount(0);
        Object[] object = new Object[DatenAbo.MAX_ELEM];
        for (DatenAbo abo : daten.getListeAbo()) {
            if (sender.isEmpty() || sender.equals(abo.getSender())) {
                object[DatenAbo.ABO_NR] = null;
                object[DatenAbo.ABO_EINGESCHALTET] = abo.isActive();
                object[DatenAbo.ABO_NAME] = null;
                object[DatenAbo.ABO_SENDER] = null;
                object[DatenAbo.ABO_THEMA] = null;
                object[DatenAbo.ABO_TITEL] = null;
                object[DatenAbo.ABO_THEMA_TITEL] = null;
                object[DatenAbo.ABO_IRGENDWO] = null;
                object[DatenAbo.ABO_MINDESTDAUER] = null;
                object[DatenAbo.ABO_MIN] = null;
                object[DatenAbo.ABO_ZIELPFAD] = null;
                object[DatenAbo.ABO_DOWN_DATUM] = null;
                object[DatenAbo.ABO_PSET] = null;
                object[DatenAbo.ABO_REF] = abo;
                model.addRow(object);
            }
        }
    }

    private void setupInfoPanel() {
        infoPanel.add(totalAbos);
        infoPanel.add(activeAbos);
        infoPanel.add(inactiveAbos);
    }

    private void initializeTable() {
        tabelleLaden();
        tabelle.readColumnConfigurationData();
        if (tabelle.getRowCount() > 0) {
            tabelle.setRowSelectionInterval(0, 0);
        }
    }

    private void setupToolBar() {
        JButton button = new JButton();
        button.setToolTipText("Abos einschalten");
        button.addActionListener(l -> changeAboActiveState(true));
        button.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/check.svg"));
        swingToolBar.add(button);

        button = new JButton();
        button.setToolTipText("Abos ausschalten");
        button.addActionListener(l -> changeAboActiveState(false));
        button.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/xmark.svg", 16f));
        swingToolBar.add(button);
        swingToolBar.addSeparator();

        button = new JButton(createAboAction);
        button.setText("");
        swingToolBar.add(button);

        button = new JButton();
        button.setToolTipText("Abos löschen");
        button.addActionListener(l -> aboLoeschen());
        button.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/trash-can.svg"));
        swingToolBar.add(button);

        button = new JButton();
        button.setToolTipText("Abo ändern");
        button.addActionListener(l -> editAbo());
        button.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/pen-to-square.svg"));
        swingToolBar.add(button);
        swingToolBar.addSeparator();

        swingToolBar.add(new JLabel("Abos für Sender:"));
        senderCombo.setMaximumSize(new Dimension(150, Integer.MAX_VALUE));
        senderCombo.setModel(GlazedListsSwing.eventComboBoxModel(new SenderListModel()));
        senderCombo.setSelectedIndex(0);
        senderCombo.addActionListener(l -> tabelleLaden());
        swingToolBar.add(senderCombo);
    }

    public void tabelleSpeichern() {
        if (tabelle != null) {
            tabelle.writeTableConfigurationData();
        }
    }

    @Handler
    private void handleAboListChanged(AboListChangedEvent e) {
        SwingUtilities.invokeLater(() -> {
            tabelleLaden();
            updateInfoText();
        });
    }

    /**
     * Get the number of abos which are active and used.
     *
     * @return num of used abos
     */
    private long numActiveAbos() {
        return Daten.getInstance().getListeAbo().stream().filter(DatenAbo::isActive).count();
    }

    /**
     * Get the number of abos which are created but offline.
     *
     * @return number of abos which are offline
     */
    private long numInactiveAbos() {
        return Daten.getInstance().getListeAbo().stream().filter(abo -> !abo.isActive()).count();
    }

    private void updateInfoText() {
        var listeAbo = Daten.getInstance().getListeAbo();
        var numAbos = listeAbo.size();

        if (numAbos == 1)
            totalAbos.setText("Gesamt: 1 Abo");
        else
            totalAbos.setText(String.format("Gesamt: %d Abos", numAbos));

        activeAbos.setText(String.format("%d eingeschaltet", numActiveAbos()));
        inactiveAbos.setText(String.format("%d ausgeschaltet", numInactiveAbos()));
    }

    private void setupKeyMap() {
        final InputMap im = tabelle.getInputMap();
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), ACTION_MAP_KEY_EDIT_ABO);
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0), ACTION_MAP_KEY_DELETE_ABO);

        final ActionMap am = tabelle.getActionMap();
        am.put(ACTION_MAP_KEY_EDIT_ABO, new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                editAbo();
            }
        });
        am.put(ACTION_MAP_KEY_DELETE_ABO, new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                aboLoeschen();
            }
        });
    }

    private JPopupMenu createContextMenu() {
        JMenuItem itemEinschalten = new JMenuItem("Abo einschalten");
        itemEinschalten.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/check.svg"));
        itemEinschalten.addActionListener(e -> changeAboActiveState(true));

        JMenuItem itemDeaktivieren = new JMenuItem("Abo ausschalten");
        itemDeaktivieren.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/xmark.svg"));
        itemDeaktivieren.addActionListener(e -> changeAboActiveState(false));

        JMenuItem itemLoeschen = new JMenuItem("Abo löschen");
        itemLoeschen.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/minus.svg"));
        itemLoeschen.addActionListener(e -> aboLoeschen());

        JMenuItem itemAendern = new JMenuItem("Abo ändern");
        itemAendern.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/pen-to-square.svg"));
        itemAendern.addActionListener(e -> editAbo());

        JMenuItem itemNeu = new JMenuItem();
        itemNeu.setAction(createAboAction);

        JPopupMenu jPopupMenu = new JPopupMenu();
        jPopupMenu.add(itemEinschalten);
        jPopupMenu.add(itemDeaktivieren);
        jPopupMenu.addSeparator();
        jPopupMenu.add(itemNeu);
        jPopupMenu.add(itemLoeschen);
        jPopupMenu.add(itemAendern);

        return jPopupMenu;
    }

    private void initListeners() {
        tabelle.setComponentPopupMenu(createContextMenu());

        tabelle.setDefaultRenderer(Object.class, new CellRendererAbo());

        tabelle.setModel(new TModelAbo(new Object[][]{}));
        tabelle.setLineBreak(false);
        tabelle.getTableHeader().addMouseListener(new BeobTableHeader(tabelle,
                DatenAbo.spaltenAnzeigen,
                new int[]{DatenAbo.ABO_EINGESCHALTET, DatenAbo.ABO_REF},
                new int[]{},
                true,
                null));

        setupKeyMap();
    }

    private void tabelleLaden() {
        tabelle.getSpalten();

        String selectedItem = null;
        var item = senderCombo.getSelectedItem();
        if (item != null)
            selectedItem = item.toString();
        if (selectedItem != null) {
            addObjectData((TModelAbo) tabelle.getModel(), selectedItem);
            tabelle.setSpalten();
        }
    }

    private void aboLoeschen() {
        final int[] rows = tabelle.getSelectedRows();
        if (rows.length > 0) {
            String text;
            if (rows.length == 1) {
                final int delRow = tabelle.convertRowIndexToModel(rows[0]);
                var abo = (DatenAbo) tabelle.getModel().getValueAt(delRow, DatenAbo.ABO_REF);
                text = '"' + abo.getName() + "\" löschen?";
            } else {
                text = "Möchten Sie wirklich " + rows.length + " Abos löschen?";
            }

            final int ret = JOptionPane.showConfirmDialog(this, text, "Abo löschen", JOptionPane.YES_NO_OPTION);
            if (ret == JOptionPane.OK_OPTION) {
                try {
                    final var listeAbo = daten.getListeAbo();
                    for (var row : rows) {
                        final int modelRow = tabelle.convertRowIndexToModel(row);
                        var abo = (DatenAbo) tabelle.getModel().getValueAt(modelRow, DatenAbo.ABO_REF);
                        listeAbo.remove(abo);
                    }
                } catch (Exception e) {
                    logger.error("aboLoeschen", e);
                }
            }
            tabelleLaden();

            selectFirstRow();

            var worker = new ChangeWorker();
            worker.execute();
        } else {
            NoSelectionErrorDialog.show(this);
        }
    }

    private void selectFirstRow() {
        if (tabelle.getRowCount() > 0) {
            // sonst ist schon eine Zeile markiert
            if (tabelle.getSelectedRow() == -1) {
                tabelle.requestFocus();
                tabelle.setRowSelectionInterval(0, 0);
            }
        }
    }

    public void editAbo() {
        // nichts selektiert
        if (tabelle.getSelectedRowCount() == 0) {
            NoSelectionErrorDialog.show(this);
            return;
        }

        final int[] rows = tabelle.getSelectedRows();
        int modelRow = tabelle.convertRowIndexToModel(tabelle.getSelectedRow());
        var editedAbo = (DatenAbo) tabelle.getModel().getValueAt(modelRow, DatenAbo.ABO_REF);

        DialogEditAbo dialog = new DialogEditAbo(MediathekGui.ui(), editedAbo, tabelle.getSelectedRowCount() > 1);
        dialog.setTitle("Abo ändern");
        dialog.setVisible(true);
        if (!dialog.successful()) {
            return;
        }

        if (tabelle.getSelectedRowCount() > 1) {
            // bei mehreren selektierten Zeilen
            for (int row : rows) {
                for (int b = 0; b < dialog.multiEditCbIndices.length; ++b) {
                    if (!dialog.multiEditCbIndices[b]) {
                        //skip over tags we should not change
                        continue;
                    }

                    modelRow = tabelle.convertRowIndexToModel(row);
                    var curSelAbo = (DatenAbo) tabelle.getModel().getValueAt(modelRow, DatenAbo.ABO_REF);

                    AboTags.fromIndex(b).ifPresent(tag -> {
                        switch (tag) {
                            case EINGESCHALTET -> curSelAbo.setActive(editedAbo.isActive());
                            case MINDESTDAUER -> curSelAbo.setMindestDauerMinuten(editedAbo.getMindestDauerMinuten());
                            case MIN -> curSelAbo.setFilmLengthState(editedAbo.getFilmLengthState());
                            case ZIELPFAD -> curSelAbo.setZielpfad(editedAbo.getZielpfad());
                            case PSET -> curSelAbo.setPsetName(editedAbo.getPsetName());
                            default -> logger.error("Unhandled tag called {}", tag);
                        }
                    });
                }
            }

        }

        tabelleLaden();
        var worker = new ChangeWorker();
        worker.execute();
    }

    private void changeAboActiveState(boolean ein) {
        final int[] rows = tabelle.getSelectedRows();
        if (rows.length > 0) {
            for (int row : rows) {
                int modelRow = tabelle.convertRowIndexToModel(row);
                var akt = (DatenAbo) tabelle.getModel().getValueAt(modelRow, DatenAbo.ABO_REF);
                akt.setActive(ein);
            }
            tabelleLaden();
            tabelle.clearSelection();
            tabelle.requestFocus();
            for (int row : rows) {
                tabelle.addRowSelectionInterval(row, row);
            }

            var worker = new ChangeWorker();
            worker.execute();
        } else {
            NoSelectionErrorDialog.show(this);
        }
    }

    private void initComponents() {
        jScrollPane1 = new JScrollPane();
        var jTable1 = new JTable();

        setLayout(new BorderLayout());

        jTable1.setAutoCreateRowSorter(true);
        jTable1.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        jScrollPane1.setViewportView(jTable1);

        add(swingToolBar, BorderLayout.NORTH);
        add(jScrollPane1, BorderLayout.CENTER);
        add(infoPanel, BorderLayout.SOUTH);
    }

    class ChangeWorker extends SwingWorker<Void,Void> {
        public ChangeWorker() {
            infiniteProgressPanel.start();
            infiniteProgressPanel.setText("Verarbeite Abos...");
        }

        @Override
        protected void done() {
            infiniteProgressPanel.stop();
            infiniteProgressPanel.setText("");
        }

        @Override
        protected Void doInBackground() {
            daten.getListeAbo().aenderungMelden();
            return null;
        }
    }
}
