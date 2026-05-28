/*
 * Created by JFormDesigner on Wed Oct 23 21:39:11 CEST 2024
 */

package mediathek.gui.duplicates.overview;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;

/**
 * Base class for UI Designer.
 * Subclasses contain the hand-written dialog behavior.
 *
 * @author christianfranzke
 */
public class FilmDuplicateOverviewDialogBase extends JDialog {
    public FilmDuplicateOverviewDialogBase(Window owner) {
        super(owner);
        initComponents();
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
        // Generated using JFormDesigner non-commercial license
        var dialogPane = new JPanel();
        var contentPanel = new JPanel();
        var splitPane1 = new JSplitPane();
        var scrollPane1 = new JScrollPane();
        tree = new JTree();
        var scrollPane2 = new JScrollPane();
        table = new JTable();
        var buttonBar = new JPanel();
        okButton = new JButton();

        //======== this ========
        setTitle("\u00dcbersicht aller Duplikate"); //NON-NLS
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setModal(true);
        setPreferredSize(new Dimension(640, 480));
        var contentPane = getContentPane();
        contentPane.setLayout(new BorderLayout());

        //======== dialogPane ========
        {
            dialogPane.setBorder(new EmptyBorder(12, 12, 12, 12));
            dialogPane.setPreferredSize(new Dimension(800, 600));
            dialogPane.setLayout(new BorderLayout());

            //======== contentPanel ========
            {
                contentPanel.setLayout(new BorderLayout());

                //======== splitPane1 ========
                {
                    splitPane1.setDividerLocation(350);

                    //======== scrollPane1 ========
                    {
                        scrollPane1.setViewportView(tree);
                    }
                    splitPane1.setLeftComponent(scrollPane1);

                    //======== scrollPane2 ========
                    {

                        //---- table ----
                        table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
                        scrollPane2.setViewportView(table);
                    }
                    splitPane1.setRightComponent(scrollPane2);
                }
                contentPanel.add(splitPane1, BorderLayout.CENTER);
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
    protected JTree tree;
    protected JTable table;
    protected JButton okButton;
    // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
