/*
 * Created by JFormDesigner on Mon Apr 07 16:51:26 CEST 2025
 */

package mediathek.gui.dialog.bookmark;

import java.awt.*;
import javax.swing.*;
import net.miginfocom.swing.*;
import org.jdesktop.swingx.*;

/**
 * @author Markus
 */
public class BorkmarkNoteDialog extends JDialog {
  public BorkmarkNoteDialog(Window owner) {
    super(owner);
    initComponents();
  }

  private void initComponents() {
    // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
    // Generated using JFormDesigner Educational license - Markus Jannek
    label1 = new JLabel();
    xDatePicker1 = new JXDatePicker();
    btnWebDate = new JButton();
    progressBar1 = new JProgressBar();
    label3 = new JLabel();
    scrollPane1 = new JScrollPane();
    textArea1 = new JTextArea();
    label2 = new JLabel();
    cancelButton = new JButton();
    saveButton = new JButton();

    //======== this ========
    var contentPane = getContentPane();
    contentPane.setLayout(new MigLayout(
      "hidemode 3",
      // columns
      "[fill]" +
      "[fill]" +
      "[fill]" +
      "[fill]" +
      "[fill]" +
      "[fill]" +
      "[fill]" +
      "[fill]" +
      "[fill]" +
      "[fill]" +
      "[fill]" +
      "[fill]" +
      "[fill]" +
      "[fill]" +
      "[fill]" +
      "[fill]" +
      "[fill]" +
      "[fill]",
      // rows
      "[]" +
      "[]" +
      "[]" +
      "[]" +
      "[]" +
      "[]" +
      "[]" +
      "[]" +
      "[]" +
      "[]"));

    //---- label1 ----
    label1.setText("Verf\u00fcgbar bis:");
    contentPane.add(label1, "cell 0 1");
    contentPane.add(xDatePicker1, "cell 1 1");

    //---- btnWebDate ----
    btnWebDate.setToolTipText("Verf\u00fcgbar bis Datum auf der Webseite suchen");
    contentPane.add(btnWebDate, "cell 3 1");
    contentPane.add(progressBar1, "cell 6 1");

    //---- label3 ----
    label3.setToolTipText("Suche nach Datum l\u00e4uft");
    contentPane.add(label3, "cell 11 1");

    //======== scrollPane1 ========
    {
      scrollPane1.setViewportView(textArea1);
    }
    contentPane.add(scrollPane1, "cell 1 2 1 5");

    //---- label2 ----
    label2.setText("Notiz:");
    contentPane.add(label2, "cell 0 4");

    //---- cancelButton ----
    cancelButton.setText("Abbruch");
    contentPane.add(cancelButton, "cell 11 9");

    //---- saveButton ----
    saveButton.setText("Speichern");
    saveButton.setEnabled(false);
    contentPane.add(saveButton, "cell 12 9 6 1");
    pack();
    setLocationRelativeTo(getOwner());
    // JFormDesigner - End of component initialization  //GEN-END:initComponents  @formatter:on
  }

  // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables  @formatter:off
  // Generated using JFormDesigner Educational license - Markus Jannek
  private JLabel label1;
  private JXDatePicker xDatePicker1;
  private JButton btnWebDate;
  private JProgressBar progressBar1;
  private JLabel label3;
  private JScrollPane scrollPane1;
  private JTextArea textArea1;
  private JLabel label2;
  private JButton cancelButton;
  private JButton saveButton;
  // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
