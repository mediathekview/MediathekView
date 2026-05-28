/*
 * Created by JFormDesigner on Mon Oct 21 17:52:41 CEST 2024
 */

package mediathek.gui.duplicates.statistics;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;

/**
 * Base class for UI Designer.
 * Subclasses contain the hand-written dialog behavior.
 *
 * @author christianfranzke
 */
public class DuplicateStatisticsDialogBase extends JDialog {
    public DuplicateStatisticsDialogBase(Window owner) {
        super(owner);
        initComponents();
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
        // Generated using JFormDesigner non-commercial license
        var tabbedPane = new JTabbedPane();
        var commonStats = new JPanel();
        var scrollPane2 = new JScrollPane();
        tblCommon = new JTable();
        lblTotalCommon = new JLabel();
        var duplicatePanel = new JPanel();
        var scrollPane1 = new JScrollPane();
        tblDuplicates = new JTable();
        lblTotalDuplicates = new JLabel();

        //======== this ========
        setTitle("Film-Statistik"); //NON-NLS
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setType(Window.Type.UTILITY);
        var contentPane = getContentPane();
        contentPane.setLayout(new BorderLayout());

        //======== tabbedPane ========
        {
            tabbedPane.setBorder(new EmptyBorder(5, 5, 5, 5));

            //======== commonStats ========
            {
                commonStats.setBorder(new EmptyBorder(5, 5, 5, 5));
                commonStats.setLayout(new BorderLayout(0, 5));

                //======== scrollPane2 ========
                {

                    //---- tblCommon ----
                    tblCommon.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
                    tblCommon.setPreferredScrollableViewportSize(new Dimension(250, 400));
                    tblCommon.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
                    tblCommon.setShowHorizontalLines(false);
                    tblCommon.setShowVerticalLines(false);
                    scrollPane2.setViewportView(tblCommon);
                }
                commonStats.add(scrollPane2, BorderLayout.CENTER);

                //---- lblTotalCommon ----
                lblTotalCommon.setText("Gesamtanzahl Filme:"); //NON-NLS
                commonStats.add(lblTotalCommon, BorderLayout.SOUTH);
            }
            tabbedPane.addTab("Allgemein", commonStats); //NON-NLS

            //======== duplicatePanel ========
            {
                duplicatePanel.setBorder(new EmptyBorder(5, 5, 5, 5));
                duplicatePanel.setLayout(new BorderLayout(0, 5));

                //======== scrollPane1 ========
                {

                    //---- tblDuplicates ----
                    tblDuplicates.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
                    tblDuplicates.setShowHorizontalLines(false);
                    tblDuplicates.setShowVerticalLines(false);
                    tblDuplicates.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
                    tblDuplicates.setPreferredScrollableViewportSize(new Dimension(250, 400));
                    scrollPane1.setViewportView(tblDuplicates);
                }
                duplicatePanel.add(scrollPane1, BorderLayout.CENTER);

                //---- lblTotalDuplicates ----
                lblTotalDuplicates.setText("Gesamtanzahl Duplikate:"); //NON-NLS
                duplicatePanel.add(lblTotalDuplicates, BorderLayout.PAGE_END);
            }
            tabbedPane.addTab("Duplikate", duplicatePanel); //NON-NLS
        }
        contentPane.add(tabbedPane, BorderLayout.CENTER);
        pack();
        setLocationRelativeTo(getOwner());
        // JFormDesigner - End of component initialization  //GEN-END:initComponents  @formatter:on
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables  @formatter:off
    // Generated using JFormDesigner non-commercial license
    protected JTable tblCommon;
    protected JLabel lblTotalCommon;
    protected JTable tblDuplicates;
    protected JLabel lblTotalDuplicates;
    // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
