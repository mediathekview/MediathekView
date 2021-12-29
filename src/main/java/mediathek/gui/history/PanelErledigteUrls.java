package mediathek.gui.history;

import ca.odell.glazedlists.BasicEventList;
import ca.odell.glazedlists.EventList;
import ca.odell.glazedlists.swing.AdvancedTableModel;
import ca.odell.glazedlists.swing.GlazedListsSwing;
import mediathek.config.Daten;
import mediathek.controller.history.AboHistoryController;
import mediathek.controller.history.MVUsedUrl;
import mediathek.gui.messages.history.AboHistoryChangedEvent;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MessageBus;
import net.engio.mbassy.listener.Handler;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import java.awt.*;

public class PanelErledigteUrls extends JPanel {
    private final AboHistoryController workList = Daten.getInstance().getAboHistoryController();
    private final EventList<MVUsedUrl> urlList = new BasicEventList<>();
    private final AdvancedTableModel<MVUsedUrl> urlTableModel =
            GlazedListsSwing.eventTableModelWithThreadProxyList(urlList, new MVUsedUrlTableFormat());

    public PanelErledigteUrls() {
        initComponents();

        jTable1.setModel(urlTableModel);
        urlTableModel.addTableModelListener(e -> updateRowDisplay());
        jTable1.setComponentPopupMenu(createContextMenu());

        loadData();

        MessageBus.getMessageBus().subscribe(this);
    }

    private JPopupMenu createContextMenu() {
        var popupMenu = new JPopupMenu();
        //löschen
        var item = new JMenuItem("Eintrag löschen");
        item.addActionListener(e -> deleteUrl());
        popupMenu.add(item);
        //Url
        item = new JMenuItem("URL kopieren");
        item.addActionListener(e -> copyUrl());
        popupMenu.add(item);

        popupMenu.addPopupMenuListener(new PopupMenuListener() {

            @Override
            public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
                SwingUtilities.invokeLater(() -> {
                    var pt = SwingUtilities.convertPoint(popupMenu, new Point(0, 0), jTable1);
                    int rowAtPoint = jTable1.rowAtPoint(pt);
                    if (rowAtPoint != -1) {
                        jTable1.setRowSelectionInterval(rowAtPoint, rowAtPoint);
                    }
                });
            }

            @Override
            public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
            }

            @Override
            public void popupMenuCanceled(PopupMenuEvent e) {
            }
        });

        return popupMenu;
    }

    @Handler
    private void handleAboHistoryChangeEvent(AboHistoryChangedEvent e) {
        SwingUtilities.invokeLater(this::loadData);
    }

    private void updateRowDisplay() {
        var text = "";
        if (urlTableModel.getRowCount() > 0) {
            text = "Anzahl: " + urlTableModel.getRowCount();
        }
        jLabelSum.setText(text);
    }

    private void loadData() {
        try {
            urlList.getReadWriteLock().writeLock().lock();
            urlList.clear();
            urlList.addAll(workList.getListeUrlsSortDate());
        }
        finally {
            urlList.getReadWriteLock().writeLock().unlock();
        }
    }

    private void deleteUrl() {
        final int selectedTableRow = jTable1.getSelectedRow();
        if (selectedTableRow != -1) {
            try {
                urlList.getReadWriteLock().writeLock().lock();
                var modelIndex = jTable1.convertRowIndexToModel(selectedTableRow);
                var obj = urlList.get(modelIndex);
                urlList.remove(obj);
                //we have to unsubscribe/subscribe otherwise we are going to reload the whole list again
                MessageBus.getMessageBus().unsubscribe(this);
                workList.urlAusLogfileLoeschen(obj.getUrl());
                MessageBus.getMessageBus().subscribe(this);
            }
            finally {
                urlList.getReadWriteLock().writeLock().unlock();
            }
        }
    }

    private void copyUrl() {
        final int selectedTableRow = jTable1.getSelectedRow();
        if (selectedTableRow != -1) {
            try {
                urlList.getReadWriteLock().readLock().lock();
                var modelIndex = jTable1.convertRowIndexToModel(selectedTableRow);
                var obj = urlList.get(modelIndex);
                GuiFunktionen.copyToClipboard(obj.getUrl());
            }
            finally {
                urlList.getReadWriteLock().readLock().unlock();
            }
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        jLabelSum = new JLabel();
        var jScrollPane1 = new JScrollPane();
        jTable1 = new JTable();

        //======== this ========
        setLayout(new MigLayout(
            new LC().insets("5").hideMode(3).gridGap("0", "5"), //NON-NLS
            // columns
            new AC()
                .grow().fill(),
            // rows
            new AC()
                .grow().fill().gap()
                .fill()));

        //---- jLabelSum ----
        jLabelSum.setHorizontalAlignment(SwingConstants.RIGHT);
        jLabelSum.setText("0"); //NON-NLS
        add(jLabelSum, new CC().cell(0, 1).alignX("trailing").growX(0)); //NON-NLS

        //======== jScrollPane1 ========
        {
            jScrollPane1.setPreferredSize(new Dimension(640, 480));
            jScrollPane1.setMinimumSize(new Dimension(150, 150));

            //---- jTable1 ----
            jTable1.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
            jScrollPane1.setViewportView(jTable1);
        }
        add(jScrollPane1, new CC().cell(0, 0));
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JLabel jLabelSum;
    protected JTable jTable1;
    // End of variables declaration//GEN-END:variables
}
