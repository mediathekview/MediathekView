/*
 * Created by JFormDesigner on Tue Oct 22 13:37:47 CEST 2024
 */

package mediathek.gui.duplicates.details;

import ca.odell.glazedlists.BasicEventList;
import ca.odell.glazedlists.SortedList;
import ca.odell.glazedlists.gui.AbstractTableComparatorChooser;
import ca.odell.glazedlists.swing.GlazedListsSwing;
import ca.odell.glazedlists.swing.TableComparatorChooser;
import mediathek.config.Daten;
import mediathek.daten.DatenFilm;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;

/**
 * @author christianfranzke
 */
public class DuplicateFilmDetailsDialog extends JDialog {
    public DuplicateFilmDetailsDialog(Window owner, @NotNull DatenFilm film) {
        super(owner);
        initComponents();

        okButton.addActionListener(l -> dispose());

        BasicEventList<DatenFilm> duplicateList = new BasicEventList<>();
        SortedList<DatenFilm> sortedList = new SortedList<>(duplicateList);
        var model = GlazedListsSwing.eventTableModelWithThreadProxyList(sortedList, new DuplicateFilmDetailsTableFormat());
        table1.setModel(model);
        TableComparatorChooser.install(table1, sortedList, AbstractTableComparatorChooser.SINGLE_COLUMN);

        table1.getColumnModel().getColumn(0).setPreferredWidth(90);
        table1.getColumnModel().getColumn(1).setPreferredWidth(120);
        table1.getColumnModel().getColumn(2).setPreferredWidth(120);

        var url = film.getUrlNormalQuality();
        Daten.getInstance().getListeFilme().parallelStream()
                .filter(f -> f.getUrlNormalQuality().equals(url))
                .forEach(duplicateList::add);

    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
        // Generated using JFormDesigner non-commercial license
        dialogPane = new JPanel();
        contentPanel = new JPanel();
        scrollPane1 = new JScrollPane();
        table1 = new JTable();
        buttonBar = new JPanel();
        okButton = new JButton();

        //======== this ========
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setModal(true);
        setTitle("Zusammengeh\u00f6rige Filme"); //NON-NLS
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

                    //---- table1 ----
                    table1.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
                    table1.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
                    table1.setShowHorizontalLines(false);
                    table1.setShowVerticalLines(false);
                    table1.setPreferredScrollableViewportSize(new Dimension(500, 150));
                    scrollPane1.setViewportView(table1);
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
    private JTable table1;
    private JPanel buttonBar;
    private JButton okButton;
    // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
