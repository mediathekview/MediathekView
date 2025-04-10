/*
 * Created by JFormDesigner on Mon Apr 07 16:51:26 CEST 2025
 */

package mediathek.gui.dialog.bookmark;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import javax.swing.*;
import mediathek.tool.SVGIconUtilities;
import net.miginfocom.swing.*;
import org.jdesktop.swingx.*;

/**
 * @author Markus
 */
public class BookmarkNoteDialog extends JDialog {
  private final DateTimeFormatter dateformatter;
  private DatenBookmark data;
  private boolean datachanged;
  private boolean hasWebURL;

  public BookmarkNoteDialog(Window owner) {
    super(owner);
    dateformatter = DateTimeFormatter.ofPattern("dd.MM.yyyy");
    initComponents();
    initActions();
  }

  private void initActions() {
    btnWebDate.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/magnifying-glass.svg"));
    datePicker.getEditor().addKeyListener(new KeyAdapter() {
      @Override
      public void keyTyped(KeyEvent e) {
        handleChange();
      }
    });

// MouseEvent: Bei Mausklick auf das Feld
    datePicker.getEditor().addMouseListener(new MouseAdapter() {
      @Override
      public void mouseClicked(MouseEvent e) {
        handleChange();
      }
    });

    datePicker.addActionListener(e -> handleChange());
    note.addKeyListener(new java.awt.event.KeyAdapter() {
      public void keyTyped(java.awt.event.KeyEvent e) {
        handleChange();
      }
    });


    cancelButton.addActionListener(e -> handleCancel());
    saveButton.addActionListener(e -> handleSave());
    btnWebDate.addActionListener(e -> btnSearchWeb(e));

  }
  private void btnSearchWeb(ActionEvent e) {
  progress.setVisible(true);
    status.setVisible(true);
    btnWebDate.setEnabled(false);

    // Simulate the background task
    SwingUtilities.invokeLater(() -> {
      // Your background task logic here
      try {
        // Simulate delay
        Thread.sleep(2000);
      } catch (InterruptedException ex) {
        ex.printStackTrace();
      }
      progress.setVisible(false);
      status.setVisible(false);
      btnWebDate.setEnabled(true);

      // Simulating result from search
      String result = "01.05.2025"; // Example result from the search

      if (result != null) {
        LocalDate localDate = LocalDate.parse(result, dateformatter);
        Date date = java.sql.Date.valueOf(localDate);
        datePicker.setDate(date);

      } else {
        JOptionPane.showMessageDialog(this, "No expiry date found.", "Warning", JOptionPane.WARNING_MESSAGE);
      }
    });
  }

  private void handleCancel() {
    datachanged = false;
    setVisible(false);
  }

  private void handleSave() {
    if (!note.getText().equals(data.getNote())) {
      data.setNote(note.getText());
      datachanged = true;
    }

    String dv = getDateValue();
    if (!(dv == null && data.getExpiry() == null) || (dv != null && !dv.equals(data.getExpiry()))) {
      data.setExpiry(dv);
      datachanged = true;
    }
    this.setVisible(false);
  }

  private void handleChange() {
    boolean isok = verify();
    saveButton.setEnabled(isok);
    datePicker.setBorder(isok ? UIManager.getBorder("TextField.border") : BorderFactory.createLineBorder(Color.RED));
  }

  private boolean verify() {
    boolean rc = true;
    String dv = getDateValue();
    if (dv != null) {
      try {
        LocalDate.parse(dv, dateformatter);
      } catch (Exception e) {
        rc = false;
      }
    }
    return rc;
  }

  private String getDateValue() {
    LocalDate selectedDate = datePicker.getDate().toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
    if (selectedDate != null) {
      return selectedDate.format(dateformatter);
    }
    return null;
  }

  private void initComponents() {
    // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
    // Generated using JFormDesigner Educational license - Markus Jannek
    expiry = new JLabel();
    datePicker = new JXDatePicker();
    btnWebDate = new JButton();
    status = new JLabel();
    progress = new JProgressBar();
    scrollPane1 = new JScrollPane();
    note = new JTextArea();
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
      "[54,fill]" +
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
      "[]"));

    //---- expiry ----
    expiry.setText("Verf\u00fcgbar bis:");
    contentPane.add(expiry, "cell 0 0");
    contentPane.add(datePicker, "cell 1 0");

    //---- btnWebDate ----
    btnWebDate.setToolTipText("Verf\u00fcgbar bis Datum auf der Webseite suchen");
    contentPane.add(btnWebDate, "cell 3 0");

    //---- status ----
    status.setToolTipText("Suche nach Datum l\u00e4uft");
    contentPane.add(status, "cell 4 0");
    contentPane.add(progress, "cell 5 0");

    //======== scrollPane1 ========
    {
      scrollPane1.setViewportView(note);
    }
    contentPane.add(scrollPane1, "cell 1 1 1 2");

    //---- label2 ----
    label2.setText("Notiz:");
    contentPane.add(label2, "cell 0 2");

    //---- cancelButton ----
    cancelButton.setText("Abbruch");
    contentPane.add(cancelButton, "cell 2 8");

    //---- saveButton ----
    saveButton.setText("Speichern");
    saveButton.setEnabled(false);
    contentPane.add(saveButton, "cell 3 8 5 1");
    pack();
    setLocationRelativeTo(getOwner());
    // JFormDesigner - End of component initialization  //GEN-END:initComponents  @formatter:on
  }

  public boolean setAndShow(DatenBookmark data) {
    this.data = data;
    this.hasWebURL = data.hasWebURL();
    this.setTitle(data.getNote() != null || data.getExpiry() != null ? "Change Notes" : "New Notes");
    note.setText(data.getNote() != null ? data.getNote() : "");
    if (data.isLiveStream()) {
      expiry.setEnabled(false);
      datePicker.setEnabled(false);
      btnWebDate.setEnabled(false);
    } else {
      if (data.getExpiry() != null && !data.getExpiry().isEmpty()) {
        try {
          LocalDate localDate = LocalDate.parse(data.getExpiry(), dateformatter);
          Date date = java.sql.Date.valueOf(localDate);
          datePicker.setDate(date);

        } catch (Exception ignored) {
        }
      }
      btnWebDate.setEnabled(hasWebURL);
    }
    handleChange();
    this.setVisible(true);
    return datachanged;
  }

  // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables  @formatter:off
  // Generated using JFormDesigner Educational license - Markus Jannek
  private JLabel expiry;
  private JXDatePicker datePicker;
  private JButton btnWebDate;
  private JLabel status;
  private JProgressBar progress;
  private JScrollPane scrollPane1;
  private JTextArea note;
  private JLabel label2;
  private JButton cancelButton;
  private JButton saveButton;
  // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
