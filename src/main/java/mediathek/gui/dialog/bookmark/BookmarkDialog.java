/*
 * Created by JFormDesigner on Sun Apr 06 14:18:54 CEST 2025
 */

package mediathek.gui.dialog.bookmark;

import javax.swing.table.*;
import mediathek.gui.*;
import static mediathek.config.StandardLocations.getBookmarkFilePath;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import java.awt.*;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.swing.*;
import com.formdev.flatlaf.extras.*;
import com.intellij.uiDesigner.core.*;
import mediathek.daten.bookmark.DatenBookmark;
import mediathek.daten.bookmark.ListeBookmark;
import mediathek.tool.SVGIconUtilities;
import mediathek.tool.models.BookmarkModel;
import net.miginfocom.swing.*;
import org.jdesktop.swingx.*;

/**
 * @author Markus
 */
public class BookmarkDialog extends JDialog {
  private static final String JSON_DATEI = getBookmarkFilePath().toString();
  private List<DatenBookmark> bookmarks;
  private BookmarkModel model;

  public BookmarkDialog(Window owner) {
    super(owner);
    bookmarks = ladeBookmarks(JSON_DATEI);
    model = new BookmarkModel(bookmarks);
    initComponents();
    initActions();
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
    scrollPane2 = new JScrollPane();
    tabelle = new JTable();
    panel2 = new JPanel();
    scrollPane1 = new JScrollPane();
    textArea1 = new JTextArea();
    xHyperlink1 = new JXHyperlink();

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

      //======== scrollPane2 ========
      {

        //---- tabelle ----
        tabelle.setModel(model);
        tabelle.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
        scrollPane2.setViewportView(tabelle);
      }
      splitPane1.setTopComponent(scrollPane2);

      //======== panel2 ========
      {
        panel2.setLayout(new GridLayoutManager(3, 2, new Insets(0, 0, 0, 0), -1, -1));

        //======== scrollPane1 ========
        {
          scrollPane1.setViewportView(textArea1);
        }
        panel2.add(scrollPane1, new GridConstraints(0, 0, 1, 1,
          GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_NONE,
          GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW,
          GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW,
          null, null, null));

        //---- xHyperlink1 ----
        xHyperlink1.setText("Link zur Webseite");
        panel2.add(xHyperlink1, new GridConstraints(2, 0, 1, 1,
          GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_NONE,
          GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW,
          GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW,
          null, null, null));
      }
      splitPane1.setBottomComponent(panel2);
    }
    contentPane.add(splitPane1, "cell 0 1 3 2");
    pack();
    setLocationRelativeTo(getOwner());
    // JFormDesigner - End of component initialization  //GEN-END:initComponents  @formatter:on
  }

  private void initActions() {
    // Löschen
    btnDeleteEntry.addActionListener(e -> {
      int selectedRow = tabelle.getSelectedRow();
      if (selectedRow >= 0) {
        int modelRow = tabelle.convertRowIndexToModel(selectedRow);
        DatenBookmark bookmark = model.getBookmarks().get(modelRow);
        int confirm = JOptionPane.showConfirmDialog(this,
            "Eintrag wirklich löschen?\n" + bookmark.getTitel(),
            "Löschen bestätigen", JOptionPane.YES_NO_OPTION);
        if (confirm == JOptionPane.YES_OPTION) {
          model.getBookmarks().remove(modelRow);
          bookmarks.remove(bookmark); // auch in Original-Liste entfernen
        }
      } else {
        JOptionPane.showMessageDialog(this, "Bitte erst einen Eintrag auswählen.", "Hinweis", JOptionPane.INFORMATION_MESSAGE);
      }
    });

    // Speichern
    btnSaveList.addActionListener(e -> {
      speichereBookmarks(bookmarks, JSON_DATEI);
      JOptionPane.showMessageDialog(this, "Merkliste gespeichert.", "Info", JOptionPane.INFORMATION_MESSAGE);
    });
  }


  private static List<DatenBookmark> ladeBookmarks(String pfad) {
    ObjectMapper mapper = new ObjectMapper();
    File file = new File(pfad);
    if (!file.exists()) return new ArrayList<>();
    try {
      ListeBookmark liste = mapper.readValue(file, ListeBookmark.class);
      return liste.getBookmarks();
    } catch (IOException e) {
      e.printStackTrace();
      return new ArrayList<>();
    }
  }

  private static void speichereBookmarks(List<DatenBookmark> bookmarks, String pfad) {
    ObjectMapper mapper = new ObjectMapper().enable(SerializationFeature.INDENT_OUTPUT);
    try {
      mapper.writeValue(new File(pfad), new ListeBookmark(bookmarks));
    } catch (IOException e) {
      e.printStackTrace();
    }
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
  private JScrollPane scrollPane2;
  private JTable tabelle;
  private JPanel panel2;
  private JScrollPane scrollPane1;
  private JTextArea textArea1;
  private JXHyperlink xHyperlink1;
  // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
