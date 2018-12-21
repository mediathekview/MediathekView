/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.gui;

import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mSearch.tool.Datum;
import mSearch.tool.Listener;
import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.daten.DatenAbo;
import mediathek.gui.actions.CreateNewAboAction;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.gui.messages.AboListChangedEvent;
import mediathek.gui.messages.UpdateStatusBarLeftDisplayEvent;
import mediathek.gui.toolbar.FXAboToolBar;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.NoSelectionErrorDialog;
import mediathek.tool.TModelAbo;
import mediathek.tool.cellrenderer.CellRendererAbo;
import mediathek.tool.listener.BeobTableHeader;
import mediathek.tool.table.MVAbosTable;
import mediathek.tool.table.MVTable;
import net.engio.mbassy.listener.Handler;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

@SuppressWarnings("serial")
public class GuiAbo extends JPanel {
    private final MVTable tabelle;
    private final Daten daten;
    private final JFrame parentComponent;

    public void tabelleSpeichern() {
        if (tabelle != null) {
            tabelle.tabelleNachDatenSchreiben();
        }
    }

    public GuiAbo(Daten d, MediathekGui parentComponent) {
        super();
        daten = d;
        this.parentComponent = parentComponent;

        initComponents();

        daten.getMessageBus().subscribe(this);

        tabelle = new MVAbosTable();
        jScrollPane1.setViewportView(tabelle);

        initListeners();
        tabelleLaden();
        tabelle.initTabelle();
        if (tabelle.getRowCount() > 0) {
            tabelle.setRowSelectionInterval(0, 0);
        }

        SwingUtilities.invokeLater(() -> {
            JFXPanel toolBarPanel = new JFXPanel();
            add(toolBarPanel,BorderLayout.NORTH);
            Platform.runLater(() -> toolBarPanel.setScene(new Scene(new FXAboToolBar(this))));
        });
    }

    private final CreateNewAboAction createAboAction = new CreateNewAboAction(Daten.getInstance().getListeAbo());

    public void einAus(boolean ein) {
        aboEinAus(ein);
    }

    public void loeschen() {
        aboLoeschen();
    }

    public void invertSelection() {
        tabelle.invertSelection();
    }

    private void setCellRenderer() {
        final CellRendererAbo cellRenderer = new CellRendererAbo(daten.getSenderIconCache());
        tabelle.setDefaultRenderer(Object.class, cellRenderer);
        tabelle.setDefaultRenderer(Datum.class, cellRenderer);
        tabelle.setDefaultRenderer(Integer.class, cellRenderer);
    }

    @Handler
    private void handleAboListChanged(AboListChangedEvent e) {
        SwingUtilities.invokeLater(this::tabelleLaden);
    }

    private static final String ACTION_MAP_KEY_EDIT_ABO = "edit_abo";
    private static final String ACTION_MAP_KEY_DELETE_ABO = "delete_abo";

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
        itemEinschalten.setIcon(IconFontSwing.buildIcon(FontAwesome.CHECK, 16));
        itemEinschalten.addActionListener(e -> aboEinAus(true));

        JMenuItem itemDeaktivieren = new JMenuItem("Abo ausschalten");
        itemDeaktivieren.setIcon(IconFontSwing.buildIcon(FontAwesome.TIMES, 16));
        itemDeaktivieren.addActionListener(e -> aboEinAus(false));

        JMenuItem itemLoeschen = new JMenuItem("Abo löschen");
        itemLoeschen.setIcon(IconFontSwing.buildIcon(FontAwesome.MINUS, 16));
        itemLoeschen.addActionListener(e -> aboLoeschen());

        JMenuItem itemAendern = new JMenuItem("Abo ändern");
        itemAendern.setIcon(IconFontSwing.buildIcon(FontAwesome.PENCIL_SQUARE_O, 16));
        itemAendern.addActionListener(e -> editAbo());

        JMenuItem itemNeu = new JMenuItem();
        itemNeu.setAction(createAboAction);

        JMenuItem miInvertSelection = new JMenuItem("Auswahl umkehren");
        miInvertSelection.addActionListener(e -> invertSelection());

        JPopupMenu jPopupMenu = new JPopupMenu();
        jPopupMenu.add(itemEinschalten);
        jPopupMenu.add(itemDeaktivieren);
        jPopupMenu.addSeparator();
        jPopupMenu.add(itemNeu);
        jPopupMenu.add(itemLoeschen);
        jPopupMenu.add(itemAendern);
        jPopupMenu.addSeparator();
        jPopupMenu.add(miInvertSelection);

        return jPopupMenu;
    }

    private void initListeners() {
        tabelle.setComponentPopupMenu(createContextMenu());

        setCellRenderer();

        tabelle.setModel(new TModelAbo(new Object[][]{}, DatenAbo.COLUMN_NAMES));
        tabelle.setLineBreak(MVConfig.getBool(MVConfig.Configs.SYSTEM_TAB_ABO_LINEBREAK));
        tabelle.getTableHeader().addMouseListener(new BeobTableHeader(tabelle, DatenAbo.COLUMN_NAMES, DatenAbo.spaltenAnzeigen,
                new int[]{DatenAbo.ABO_EINGESCHALTET},
                new int[]{},
                true,
                MVConfig.Configs.SYSTEM_TAB_ABO_LINEBREAK));

        setupKeyMap();

        //Filter
        setupSenderCombo();

        jSplitPane1.setDividerLocation(MVConfig.getInt(MVConfig.Configs.SYSTEM_PANEL_ABO_DIVIDER));
        jSplitPane1.addPropertyChangeListener(JSplitPane.DIVIDER_LOCATION_PROPERTY, pce -> {
            if (jScrollPaneFilter.isVisible()) {
                MVConfig.add(MVConfig.Configs.SYSTEM_PANEL_ABO_DIVIDER, String.valueOf(jSplitPane1.getDividerLocation()));
            }
        });
        jScrollPaneFilter.setVisible(MVConfig.getBool(MVConfig.Configs.SYSTEM_TAB_ABO_FILTER_VIS));
        Listener.addListener(new Listener(Listener.EREIGNIS_PANEL_ABO_FILTER_ANZEIGEN, GuiAbo.class.getSimpleName()) {
            @Override
            public void ping() {
                setFilter();
            }
        });
    }

    private void setupSenderCombo() {
        jcbSender.setModel(GuiFunktionen.getSenderListComboBoxModel(daten.getListeFilme()));
        jcbSender.addActionListener(l -> tabelleLaden());
    }

    private void setFilter() {
        // Panel anzeigen und die Filmliste anpassen
        jScrollPaneFilter.setVisible(MVConfig.getBool(MVConfig.Configs.SYSTEM_TAB_ABO_FILTER_VIS));
        if (jScrollPaneFilter.isVisible()) {
            jSplitPane1.setDividerLocation(MVConfig.getInt(MVConfig.Configs.SYSTEM_PANEL_ABO_DIVIDER));
        }
        updateUI();
    }

    private void tabelleLaden() {
        tabelle.getSpalten();
        final Object selectedItem = jcbSender.getSelectedItem();
        if (selectedItem != null) {
            daten.getListeAbo().addObjectData((TModelAbo) tabelle.getModel(), selectedItem.toString());
            tabelle.setSpalten();
            setInfo();
        }
    }

    private void aboLoeschen() {
        int[] rows = tabelle.getSelectedRows();
        if (rows.length > 0) {
            String text;
            if (rows.length == 1) {
                int delRow = tabelle.convertRowIndexToModel(rows[0]);
                text = '"' + tabelle.getModel().getValueAt(delRow, DatenAbo.ABO_NAME).toString() + "\" löschen?";
            } else {
                text = rows.length + " Abos löschen?";
            }
            final int ret = JOptionPane.showConfirmDialog(parentComponent, text, "Löschen?", JOptionPane.YES_NO_OPTION);
            if (ret == JOptionPane.OK_OPTION) {
                for (int i = rows.length - 1; i >= 0; --i) {
                    int delRow = tabelle.convertRowIndexToModel(rows[i]);
                    ((TModelAbo) tabelle.getModel()).removeRow(delRow);
                    daten.getListeAbo().remove(delRow);
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

        DatenAbo akt = daten.getListeAbo().getAboNr(modelRow);
        DialogEditAbo dialog = new DialogEditAbo(MediathekGui.ui(), true, daten, akt, tabelle.getSelectedRowCount() > 1);
        dialog.setTitle("Abo ändern");
        dialog.setVisible(true);
        if (!dialog.ok) {
            return;
        }

        if (tabelle.getSelectedRowCount() > 1) {
            // bei mehreren selektierten Zeilen
            for (int row : rows) {
                for (int b = 0; b < dialog.ch.length; ++b) {
                    if (!dialog.ch[b]) {
                        continue;
                    }
                    modelRow = tabelle.convertRowIndexToModel(row);
                    DatenAbo sel = daten.getListeAbo().getAboNr(modelRow);
                    sel.arr[b] = akt.arr[b];
                    if (b == DatenAbo.ABO_MINDESTDAUER) {
                        sel.setMindestDauerMinuten();
                    }
                    if (b == DatenAbo.ABO_MIN) {
                        sel.min = Boolean.parseBoolean(sel.arr[DatenAbo.ABO_MIN]);
                    }
                }
            }

        }

        tabelleLaden();
        daten.getListeAbo().aenderungMelden();
        setInfo();
    }

    private void aboEinAus(boolean ein) {
        final int[] rows = tabelle.getSelectedRows();
        if (rows.length > 0) {
            for (int row : rows) {
                int modelRow = tabelle.convertRowIndexToModel(row);
                DatenAbo akt = daten.getListeAbo().getAboNr(modelRow);
                akt.arr[DatenAbo.ABO_EINGESCHALTET] = String.valueOf(ein);
            }
            tabelleLaden();
            tabelle.clearSelection();
            tabelle.requestFocus();
            for (int row : rows) {
                tabelle.addRowSelectionInterval(row, row);
            }
            setInfo();
            daten.getListeAbo().aenderungMelden();
        } else {
            NoSelectionErrorDialog.show();
        }
    }

    private void setInfo() {
        daten.getMessageBus().publishAsync(new UpdateStatusBarLeftDisplayEvent());
    }

    /**
     * This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        jSplitPane1 = new JSplitPane();
        jScrollPane1 = new JScrollPane();
        var jTable1 = new JTable();
        jScrollPaneFilter = new JScrollPane();
        var jPanelFilter = new JPanel();
        var jLabel1 = new JLabel();
        jcbSender = new JComboBox<>();

        //======== this ========
        setLayout(new BorderLayout());

        //======== jSplitPane1 ========
        {
            jSplitPane1.setDividerLocation(200);

            //======== jScrollPane1 ========
            {

                //---- jTable1 ----
                jTable1.setAutoCreateRowSorter(true);
                jTable1.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
                jScrollPane1.setViewportView(jTable1);
            }
            jSplitPane1.setRightComponent(jScrollPane1);

            //======== jScrollPaneFilter ========
            {

                //======== jPanelFilter ========
                {

                    //---- jLabel1 ----
                    jLabel1.setText("Abos f\u00fcr Sender:"); //NON-NLS

                    //---- jcbSender ----
                    jcbSender.setMaximumRowCount(25);
                    jcbSender.setModel(new DefaultComboBoxModel<>(new String[] {
                        "Item 1", //NON-NLS
                        "Item 2", //NON-NLS
                        "Item 3", //NON-NLS
                        "Item 4" //NON-NLS
                    }));

                    GroupLayout jPanelFilterLayout = new GroupLayout(jPanelFilter);
                    jPanelFilter.setLayout(jPanelFilterLayout);
                    jPanelFilterLayout.setHorizontalGroup(
                        jPanelFilterLayout.createParallelGroup()
                            .addGroup(jPanelFilterLayout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanelFilterLayout.createParallelGroup()
                                    .addComponent(jcbSender)
                                    .addGroup(jPanelFilterLayout.createSequentialGroup()
                                        .addComponent(jLabel1)
                                        .addGap(0, 57, Short.MAX_VALUE)))
                                .addContainerGap())
                    );
                    jPanelFilterLayout.setVerticalGroup(
                        jPanelFilterLayout.createParallelGroup()
                            .addGroup(jPanelFilterLayout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jLabel1)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jcbSender, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                .addContainerGap(400, Short.MAX_VALUE))
                    );
                }
                jScrollPaneFilter.setViewportView(jPanelFilter);
            }
            jSplitPane1.setLeftComponent(jScrollPaneFilter);
        }
        add(jSplitPane1, BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JSplitPane jSplitPane1;
    private JScrollPane jScrollPane1;
    private JScrollPane jScrollPaneFilter;
    private JComboBox<String> jcbSender;
    // End of variables declaration//GEN-END:variables
}
