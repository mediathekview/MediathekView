package mediathek.gui.history;

import mediathek.config.Daten;
import mediathek.controller.history.AboHistoryController;
import mediathek.controller.history.MVUsedUrlModelHelper;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.models.NonEditableTableModel;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.table.TableModel;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

public abstract class PanelErledigteUrls extends JPanel {
    protected final Daten daten;
    protected AboHistoryController workList;

    public PanelErledigteUrls() {
        this.daten = Daten.getInstance();

        initComponents();
        jTable1.addMouseListener(new BeobMausTabelle());
        initListeners();
    }

    protected void changeListHandler() {
        if (jToggleButtonLaden.isSelected())
            updateModelAndRecalculate(createDataModel());
    }

    private void initListeners() {
        jToggleButtonLaden.addActionListener((ActionEvent e) -> {
            if (jToggleButtonLaden.isSelected()) {
                updateModelAndRecalculate(createDataModel());
            } else {
                updateModelAndRecalculate(new NonEditableTableModel(null, MVUsedUrlModelHelper.TITLE_HEADER));
            }
        });
    }

    private void updateModelAndRecalculate(@NotNull TableModel model) {
        jTable1.setModel(model);
        setsum();
    }

    protected TableModel createDataModel() {
        final var data = MVUsedUrlModelHelper.getObjectData(workList.getListeUrlsSortDate());
        return new NonEditableTableModel(data, MVUsedUrlModelHelper.TITLE_HEADER);
    }

    private void setsum() {
        if (jTable1.getRowCount() <= 0) {
            jLabelSum.setText("");
        } else {
            jLabelSum.setText("Anzahl: " + jTable1.getRowCount());
        }
    }

    class BeobMausTabelle extends MouseAdapter {
        @Override
        public void mousePressed(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        @Override
        public void mouseReleased(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        private void showMenu(MouseEvent evt) {
            Point p = evt.getPoint();
            int nr = jTable1.rowAtPoint(p);
            if (nr >= 0) {
                jTable1.setRowSelectionInterval(nr, nr);
            }
            JPopupMenu jPopupMenu = new JPopupMenu();
            //löschen
            JMenuItem item = new JMenuItem("Url aus der Liste löschen");
            item.addActionListener(e -> {
                final int selectedTableRow = jTable1.getSelectedRow();
                if (selectedTableRow != -1) {
                    String del = jTable1.getValueAt(jTable1.convertRowIndexToModel(selectedTableRow), MVUsedUrlModelHelper.USED_URL_URL).toString();
                    workList.urlAusLogfileLoeschen(del);
                }
            });
            jPopupMenu.add(item);
            //Url
            item = new JMenuItem("URL kopieren");
            item.addActionListener(e -> {
                final int selectedTableRow = jTable1.getSelectedRow();
                if (selectedTableRow != -1) {
                    String url = jTable1.getValueAt(jTable1.convertRowIndexToModel(selectedTableRow), MVUsedUrlModelHelper.USED_URL_URL).toString();
                    GuiFunktionen.copyToClipboard(url);
                }
            });
            jPopupMenu.add(item);

            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jScrollPane1 = new JScrollPane();
        jTable1 = new JTable();
        var panel1 = new JPanel();
        jToggleButtonLaden = new JToggleButton();
        jLabelSum = new JLabel();

        //======== this ========
        setLayout(new MigLayout(
            new LC().insets("5").hideMode(3).gridGap("0", "0"), //NON-NLS
            // columns
            new AC()
                .grow().fill(),
            // rows
            new AC()
                .grow().fill().gap()
                .fill()));

        //======== jScrollPane1 ========
        {
            jScrollPane1.setViewportView(jTable1);
        }
        add(jScrollPane1, new CC().cell(0, 0));

        //======== panel1 ========
        {
            panel1.setLayout(new MigLayout(
                new LC().insets("5 5 0 5").hideMode(3).gridGap("5", "5"), //NON-NLS
                // columns
                new AC()
                    .fill().gap()
                    .grow().fill(),
                // rows
                new AC()
                    .fill()));

            //---- jToggleButtonLaden ----
            jToggleButtonLaden.setText("Laden"); //NON-NLS
            panel1.add(jToggleButtonLaden, new CC().cell(0, 0));

            //---- jLabelSum ----
            jLabelSum.setHorizontalAlignment(SwingConstants.RIGHT);
            panel1.add(jLabelSum, new CC().cell(1, 0));
        }
        add(panel1, new CC().cell(0, 1));
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    protected JTable jTable1;
    protected JToggleButton jToggleButtonLaden;
    private JLabel jLabelSum;
    // End of variables declaration//GEN-END:variables
}
