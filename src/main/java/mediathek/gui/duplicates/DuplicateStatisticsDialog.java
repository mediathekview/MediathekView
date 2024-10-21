/*
 * Created by JFormDesigner on Mon Oct 21 17:52:41 CEST 2024
 */

package mediathek.gui.duplicates;

import ca.odell.glazedlists.SortedList;
import ca.odell.glazedlists.swing.GlazedListsSwing;
import mediathek.config.Daten;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumnModel;
import java.awt.*;
import java.util.Comparator;

/**
 * @author christianfranzke
 */
public class DuplicateStatisticsDialog extends JDialog {
    private final AbstractAction action;

    public DuplicateStatisticsDialog(@NotNull Window owner, @NotNull AbstractAction action) {
        super(owner);
        this.action = action;

        initComponents();

        var tableFormat = new DuplicateStatisticsTableFormat();
        final var dupeStatsList = Daten.getInstance().getFilmListDuplicateStatisticsList();
        SortedList<DuplicateStatistics> sortedList = new SortedList<>(dupeStatsList, Comparator.comparing(DuplicateStatistics::sender));
        var model = GlazedListsSwing.eventTableModelWithThreadProxyList(sortedList, tableFormat);
        model.addTableModelListener(e -> updateTotalStats());
        table.setModel(model);

        updateTotalStats();

        //table.getColumnModel().getColumn(0).setPreferredWidth(120);
        table.getColumnModel().getColumn(1).setPreferredWidth(130);
        resizeSenderColumnWidth();

        pack();

        action.setEnabled(false);
    }

    @Override
    public void dispose() {
        action.setEnabled(true);

        super.dispose();
    }

    private void resizeSenderColumnWidth() {
        final TableColumnModel columnModel = table.getColumnModel();
        int width = 120; // Min width
        for (int row = 0; row < table.getRowCount(); row++) {
            TableCellRenderer renderer = table.getCellRenderer(row, 0);
            Component comp = table.prepareRenderer(renderer, row, 0);
            width = Math.max(comp.getPreferredSize().width + 1, width);
        }
        columnModel.getColumn(0).setPreferredWidth(width);
    }

    private void updateTotalStats() {
        resizeSenderColumnWidth();

        var statisticsEventList = Daten.getInstance().getFilmListDuplicateStatisticsList();
        statisticsEventList.getReadWriteLock().readLock().lock();
        long dupes = 0;
        for (var item: statisticsEventList) {
            dupes += item.count();
        }
        label1.setText(String.format("Gesamtanzahl Duplikate: %d", dupes));
        statisticsEventList.getReadWriteLock().readLock().unlock();
        pack();
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
        // Generated using JFormDesigner non-commercial license
        var contentPane = new JPanel();
        scrollPane1 = new JScrollPane();
        table = new JTable();
        label1 = new JLabel();

        //======== this ========
        setTitle("Filmduplikat-Statistik"); //NON-NLS
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setType(Window.Type.UTILITY);
        var contentPane2 = getContentPane();
        contentPane2.setLayout(new BorderLayout());

        //======== contentPane ========
        {
            contentPane.setBorder(new EmptyBorder(5, 5, 5, 5));
            contentPane.setLayout(new BorderLayout(0, 5));

            //======== scrollPane1 ========
            {

                //---- table ----
                table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
                table.setShowHorizontalLines(false);
                table.setShowVerticalLines(false);
                table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
                table.setPreferredScrollableViewportSize(new Dimension(250, 400));
                scrollPane1.setViewportView(table);
            }
            contentPane.add(scrollPane1, BorderLayout.CENTER);

            //---- label1 ----
            label1.setText("Gesamtanzahl Duplikate:"); //NON-NLS
            contentPane.add(label1, BorderLayout.PAGE_END);
        }
        contentPane2.add(contentPane, BorderLayout.CENTER);
        pack();
        setLocationRelativeTo(getOwner());
        // JFormDesigner - End of component initialization  //GEN-END:initComponents  @formatter:on
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables  @formatter:off
    // Generated using JFormDesigner non-commercial license
    private JScrollPane scrollPane1;
    private JTable table;
    private JLabel label1;
    // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
