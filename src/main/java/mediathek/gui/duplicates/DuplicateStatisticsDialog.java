/*
 * Created by JFormDesigner on Mon Oct 21 17:52:41 CEST 2024
 */

package mediathek.gui.duplicates;

import ca.odell.glazedlists.swing.GlazedListsSwing;
import mediathek.config.Daten;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumnModel;
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
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
        // Generated using JFormDesigner non-commercial license
        dialogPane = new JPanel();
        contentPanel = new JPanel();
        scrollPane1 = new JScrollPane();
        table = new JTable();
        buttonBar = new JPanel();
        okButton = new JButton();

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
                    table.setModel(new DefaultTableModel(
                        new Object[][] {
                            {"ARD", 1L}, //NON-NLS
                            {"ZDF", 2L}, //NON-NLS
                        },
                        new String[] {
                            "Sender", "Anzahl Duplikate" //NON-NLS
                        }
                    ) {
                        final Class<?>[] columnTypes = new Class<?>[] {
                            String.class, Long.class
                        };
                        final boolean[] columnEditable = new boolean[] {
                            false, true
                        };
                        @Override
                        public Class<?> getColumnClass(int columnIndex) {
                            return columnTypes[columnIndex];
                        }
                        @Override
                        public boolean isCellEditable(int rowIndex, int columnIndex) {
                            return columnEditable[columnIndex];
                        }
                    });
                    {
                        TableColumnModel cm = table.getColumnModel();
                        cm.getColumn(1).setPreferredWidth(100);
                    }
                    table.setAutoCreateRowSorter(true);
                    table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
                    table.setShowHorizontalLines(false);
                    table.setShowVerticalLines(false);
                    scrollPane1.setViewportView(table);
                }
                contentPanel.add(scrollPane1, BorderLayout.CENTER);
            }
            dialogPane.add(contentPanel, BorderLayout.CENTER);

            //======== buttonBar ========
            {
                buttonBar.setBorder(new EmptyBorder(12, 0, 0, 0));
                buttonBar.setLayout(new GridBagLayout());
                ((GridBagLayout)buttonBar.getLayout()).columnWidths = new int[] {0, 80};
                ((GridBagLayout)buttonBar.getLayout()).columnWeights = new double[] {1.0, 0.0};

                //---- okButton ----
                okButton.setText("Schlie\u00dfen"); //NON-NLS
                buttonBar.add(okButton, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0,
                    GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                    new Insets(0, 0, 0, 0), 0, 0));
            }
            dialogPane.add(buttonBar, BorderLayout.SOUTH);
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
    private JPanel buttonBar;
    private JButton okButton;
    // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
