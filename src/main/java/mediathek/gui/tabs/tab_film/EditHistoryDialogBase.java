/*
 * Created by JFormDesigner on Sat Apr 27 12:49:11 CEST 2024
 */

package mediathek.gui.tabs.tab_film;

import mediathek.tool.SVGIconUtilities;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;

/**
 * Base class for UI Designer.
 * Subclasses contain the hand-written dialog behavior.
 *
 * @author christianfranzke
 */
public class EditHistoryDialogBase extends JDialog {
    public EditHistoryDialogBase(Window owner) {
        super(owner);
        initComponents();
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
        // Generated using JFormDesigner non-commercial license
        var dialogPane = new JPanel();
        var contentPanel = new JPanel();
        var scrollPane1 = new JScrollPane();
        list = new JList<>();
        var toolBar1 = new JToolBar();
        btnDeleteEntries = new JButton();
        btnDeleteEntries.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/trash-can.svg")); //NON-NLS
        btnUp = new JButton();
        btnUp.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/arrow-up.svg")); //NON-NLS
        btnDown = new JButton();
        btnDown.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/arrow-down.svg")); //NON-NLS

        //======== this ========
        setTitle("Suchhistorie bearbeiten"); //NON-NLS
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
                    scrollPane1.setViewportView(list);
                }
                contentPanel.add(scrollPane1, BorderLayout.CENTER);

                //======== toolBar1 ========
                {
                    toolBar1.setFloatable(false);

                    //---- btnDeleteEntries ----
                    btnDeleteEntries.setToolTipText("Ausgew\u00e4hlte Eintr\u00e4ge l\u00f6schen"); //NON-NLS
                    toolBar1.add(btnDeleteEntries);

                    //---- btnUp ----
                    btnUp.setToolTipText("Element nach oben verschieben"); //NON-NLS
                    toolBar1.add(btnUp);

                    //---- btnDown ----
                    btnDown.setToolTipText("Element nach unten verschieben"); //NON-NLS
                    toolBar1.add(btnDown);
                }
                contentPanel.add(toolBar1, BorderLayout.NORTH);
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
    protected JList<String> list;
    protected JButton btnDeleteEntries;
    protected JButton btnUp;
    protected JButton btnDown;
    // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
