package mediathek.gui.abo;

import javafx.embed.swing.JFXPanel;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.layout.HBox;
import mediathek.config.Daten;
import mediathek.daten.abo.AboTags;
import mediathek.daten.abo.DatenAbo;
import mediathek.gui.actions.CreateNewAboAction;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.gui.messages.AboListChangedEvent;
import mediathek.javafx.tool.JavaFxUtils;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.MessageBus;
import mediathek.tool.NoSelectionErrorDialog;
import mediathek.tool.SVGIconUtilities;
import mediathek.tool.cellrenderer.CellRendererAbo;
import mediathek.tool.listener.BeobTableHeader;
import mediathek.tool.models.TModelAbo;
import mediathek.tool.table.MVAbosTable;
import mediathek.tool.table.MVTable;
import net.engio.mbassy.listener.Handler;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.net.URL;

public class ManageAboPanel extends JPanel {
    private static final String ACTION_MAP_KEY_EDIT_ABO = "edit_abo";
    private static final String ACTION_MAP_KEY_DELETE_ABO = "delete_abo";
    private static final Logger logger = LogManager.getLogger();
    private final MVTable tabelle = new MVAbosTable();
    private final Daten daten;
    private final CreateNewAboAction createAboAction = new CreateNewAboAction(Daten.getInstance().getListeAbo());
    private final JFXPanel toolBarPanel = new JFXPanel();
    private final JFXPanel infoPanel = new JFXPanel();
    private FXAboToolBar toolBar;
    private JScrollPane jScrollPane1;
    /*
     * controller must be kept in variable for strong ref, otherwise GC will erase controller and therefore
     * update of abos in dialog will stop working...
     */
    private AboInformationController infoController;

    public ManageAboPanel() {
        super();
        daten = Daten.getInstance();

        initComponents();
        jScrollPane1.setViewportView(tabelle);

        setupToolBar();
        setupInfoPanel();

        MessageBus.getMessageBus().subscribe(this);

        initListeners();

        initializeTable();
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
        JavaFxUtils.invokeInFxThreadAndWait(() -> {
            try {
                URL url = getClass().getResource("/mediathek/res/programm/fxml/abo/abo_information_panel.fxml");

                FXMLLoader loader = new FXMLLoader();
                loader.setLocation(url);

                HBox infoPane = loader.load();
                infoPanel.setScene(new Scene(infoPane));

                infoController = loader.getController();
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        });
    }

    private void initializeTable() {
        tabelleLaden();
        tabelle.readColumnConfigurationData();
        if (tabelle.getRowCount() > 0) {
            tabelle.setRowSelectionInterval(0, 0);
        }
    }

    private void setupToolBar() {
        CreateNewAboAction newAboAction = new CreateNewAboAction(Daten.getInstance().getListeAbo());
        JavaFxUtils.invokeInFxThreadAndWait(() -> {
            toolBar = new FXAboToolBar();
            toolBar.btnOn.setOnAction(e -> SwingUtilities.invokeLater(() -> changeAboActiveState(true)));
            toolBar.btnOff.setOnAction(e -> SwingUtilities.invokeLater(() -> changeAboActiveState(false)));
            toolBar.btnDelete.setOnAction(e -> SwingUtilities.invokeLater(this::aboLoeschen));
            toolBar.btnEdit.setOnAction(e -> SwingUtilities.invokeLater(this::editAbo));

            toolBar.btnNewAbo.setOnAction(e -> SwingUtilities.invokeLater(() -> newAboAction.actionPerformed(null)));

            toolBar.cbSender.setOnAction(e -> SwingUtilities.invokeLater(this::tabelleLaden));

            toolBarPanel.setScene(new Scene(toolBar));
        });
    }

    public void tabelleSpeichern() {
        if (tabelle != null) {
            tabelle.writeTableConfigurationData();
        }
    }

    @Handler
    private void handleAboListChanged(AboListChangedEvent e) {
        SwingUtilities.invokeLater(this::tabelleLaden);
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

        JavaFxUtils.invokeInFxThreadAndWait(() -> {
            final String selectedItem = toolBar.cbSender.getValue();
            if (selectedItem != null) {
                SwingUtilities.invokeLater(() -> {
                    addObjectData((TModelAbo) tabelle.getModel(), selectedItem);
                    tabelle.setSpalten();
                });
            }
        });
    }

    private void aboLoeschen() {
        final int[] rows = tabelle.getSelectedRows();
        if (rows.length > 0) {
            String text;
            if (rows.length == 1) {
                final int delRow = tabelle.convertRowIndexToModel(rows[0]);
                var abo = (DatenAbo)tabelle.getModel().getValueAt(delRow, DatenAbo.ABO_REF);
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
                        var abo = (DatenAbo)tabelle.getModel().getValueAt(modelRow, DatenAbo.ABO_REF);
                        listeAbo.remove(abo);
                    }
                } catch (Exception e) {
                    logger.error("aboLoeschen", e);
                }
            }
            tabelleLaden();

            selectFirstRow();

            daten.getListeAbo().aenderungMelden();
        } else {
            NoSelectionErrorDialog.show();
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
            NoSelectionErrorDialog.show();
            return;
        }

        final int[] rows = tabelle.getSelectedRows();
        int modelRow = tabelle.convertRowIndexToModel(tabelle.getSelectedRow());
        var editedAbo = (DatenAbo)tabelle.getModel().getValueAt(modelRow, DatenAbo.ABO_REF);

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
                    var curSelAbo = (DatenAbo)tabelle.getModel().getValueAt(modelRow, DatenAbo.ABO_REF);

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
        daten.getListeAbo().aenderungMelden();
    }

    private void changeAboActiveState(boolean ein) {
        final int[] rows = tabelle.getSelectedRows();
        if (rows.length > 0) {
            for (int row : rows) {
                int modelRow = tabelle.convertRowIndexToModel(row);
                var akt = (DatenAbo)tabelle.getModel().getValueAt(modelRow, DatenAbo.ABO_REF);
                akt.setActive(ein);
            }
            tabelleLaden();
            tabelle.clearSelection();
            tabelle.requestFocus();
            for (int row : rows) {
                tabelle.addRowSelectionInterval(row, row);
            }

            daten.getListeAbo().aenderungMelden();
        } else {
            NoSelectionErrorDialog.show();
        }
    }

    private void initComponents() {
        jScrollPane1 = new JScrollPane();
        var jTable1 = new JTable();

        setLayout(new BorderLayout());

        jTable1.setAutoCreateRowSorter(true);
        jTable1.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        jScrollPane1.setViewportView(jTable1);

        add(toolBarPanel, BorderLayout.NORTH);
        add(jScrollPane1, BorderLayout.CENTER);
        add(infoPanel, BorderLayout.SOUTH);
    }
}
