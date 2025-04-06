/*
 * Created by JFormDesigner on Sun Apr 06 14:18:54 CEST 2025
 */

package mediathek.gui.dialog.bookmark;

import java.awt.*;
import javax.swing.*;
import com.formdev.flatlaf.extras.*;
import mediathek.tool.SVGIconUtilities;
import net.miginfocom.swing.*;

/**
 * @author Markus
 */
public class BookmarkDialog extends JDialog {
  public BookmarkDialog(Window owner) {
    super(owner);
    initComponents();
  }

  private void initComponents() {
    // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
    // Generated using JFormDesigner Educational license - Markus Jannek
    toolBar1 = new JToolBar();
    btnDeleteEntry = new JButton();
    btnMarkViewed = new JButton();
    btnEditNote = new JButton();
    panel1 = new JPanel();
    btnSaveList = new JButton();
    btnShowDetails = new JToggleButton();
    btnFilter = new JButton();
    splitPane1 = new JSplitPane();
    scrollPane1 = new JScrollPane();
    textArea1 = new JTextArea();
    label1 = new JLabel();

    //======== this ========
    setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
    setResizable(false);
    setTitle("Merkliste verwalten");
    var contentPane = getContentPane();
    contentPane.setLayout(new MigLayout(
      "hidemode 3",
      // columns
      "[97,fill]" +
      "[283,fill]" +
      "[fill]",
      // rows
      "[]" +
      "[213]" +
      "[]"));

    //======== toolBar1 ========
    {

      //---- btnDeleteEntry ----
      btnDeleteEntry.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/trash-can.svg"));
      btnDeleteEntry.setToolTipText("Aus der Merkliste l\u00f6schen");
      toolBar1.add(btnDeleteEntry);

      //---- btnMarkViewed ----
      btnMarkViewed.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/eye.svg"));
      toolBar1.add(btnMarkViewed);

      //---- btnEditNote ----
      btnEditNote.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/pen.svg"));
      btnEditNote.setToolTipText("Anmerkungen bearbeiten");
      toolBar1.add(btnEditNote);

      //======== panel1 ========
      {
        panel1.setLayout(new MigLayout(
          "hidemode 3,alignx center",
          // columns
          "[fill]" +
          "[fill]",
          // rows
          "[]" +
          "[]" +
          "[]"));
      }
      toolBar1.add(panel1);

      //---- btnSaveList ----
      btnSaveList.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/floppy-disk.svg"));
      btnSaveList.setToolTipText("Ge\u00e4nderte Merkliste abspeichern");
      toolBar1.add(btnSaveList);
      toolBar1.addSeparator();

      //---- btnShowDetails ----
      btnShowDetails.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-info.svg"));
      btnShowDetails.setToolTipText("Erweiterte Film Informationen anzeigen");
      toolBar1.add(btnShowDetails);

      //---- btnFilter ----
      btnFilter.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/filter.svg"));
      toolBar1.add(btnFilter);
    }
    contentPane.add(toolBar1, "cell 0 0 3 1");

    //======== splitPane1 ========
    {
      splitPane1.setOrientation(JSplitPane.VERTICAL_SPLIT);

      //======== scrollPane1 ========
      {
        scrollPane1.setViewportView(textArea1);
      }
      splitPane1.setBottomComponent(scrollPane1);

      //---- label1 ----
      label1.setText("text");
      splitPane1.setBottomComponent(label1);
    }
    contentPane.add(splitPane1, "cell 0 1 3 1");
    pack();
    setLocationRelativeTo(getOwner());
    // JFormDesigner - End of component initialization  //GEN-END:initComponents  @formatter:on
  }

  // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables  @formatter:off
  // Generated using JFormDesigner Educational license - Markus Jannek
  private JToolBar toolBar1;
  private JButton btnDeleteEntry;
  private JButton btnMarkViewed;
  private JButton btnEditNote;
  private JPanel panel1;
  private JButton btnSaveList;
  private JToggleButton btnShowDetails;
  private JButton btnFilter;
  private JSplitPane splitPane1;
  private JScrollPane scrollPane1;
  private JTextArea textArea1;
  private JLabel label1;
  // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
