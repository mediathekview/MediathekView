/*
 * Created by JFormDesigner on Mon Oct 21 17:52:41 CEST 2024
 */

package mediathek.gui.duplicates;

import ca.odell.glazedlists.swing.GlazedListsSwing;
import mediathek.config.Daten;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;

/**
 * @author christianfranzke
 */
public class DuplicateStatisticsDialog extends JDialog {

    public DuplicateStatisticsDialog(Window owner) {
        super(owner);
        initComponents();

        var tableFormat = new DuplicateStatisticsTableFormat();
        var model = GlazedListsSwing.eventTableModelWithThreadProxyList(Daten.getInstance().getFilmListDuplicateStatisticsList(), tableFormat);
        table.setModel(model);

        model.addTableModelListener(e -> {
            var statisticsEventList = Daten.getInstance().getFilmListDuplicateStatisticsList();
            statisticsEventList.getReadWriteLock().readLock().lock();
            long dupes = 0;
            for (var item: statisticsEventList) {
                dupes += item.count();
            }
            label1.setText(String.format("Gesamtanzahl Duplikate: %d", dupes));
            statisticsEventList.getReadWriteLock().readLock().unlock();
        });
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
        // Generated using JFormDesigner non-commercial license
        dialogPane = new JPanel();
        contentPanel = new JPanel();
        scrollPane1 = new JScrollPane();
        table = new JTable();
        label1 = new JLabel();

        //======== this ========
        setTitle("Duplikatstatistik"); //NON-NLS
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setType(Window.Type.UTILITY);
        var contentPane = getContentPane();
        contentPane.setLayout(new BorderLayout());

        //======== dialogPane ========
        {
            dialogPane.setBorder(new EmptyBorder(12, 12, 12, 12));
            dialogPane.setLayout(new BorderLayout());

            //======== contentPanel ========
            {
                contentPanel.setLayout(new BorderLayout());

                //======== scrollPane1 ========
                {

                    //---- table ----
                    table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
                    table.setShowHorizontalLines(false);
                    table.setShowVerticalLines(false);
                    scrollPane1.setViewportView(table);
                }
                contentPanel.add(scrollPane1, BorderLayout.CENTER);

                //---- label1 ----
                label1.setText("Gesamtanzahl Duplikate:"); //NON-NLS
                contentPanel.add(label1, BorderLayout.SOUTH);
            }
            dialogPane.add(contentPanel, BorderLayout.CENTER);
        }
        contentPane.add(dialogPane, BorderLayout.CENTER);
        pack();
        setLocationRelativeTo(getOwner());
        // JFormDesigner - End of component initialization  //GEN-END:initComponents  @formatter:on
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables  @formatter:off
    // Generated using JFormDesigner non-commercial license
    private JPanel dialogPane;
    private JPanel contentPanel;
    private JScrollPane scrollPane1;
    private JTable table;
    private JLabel label1;
    // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
