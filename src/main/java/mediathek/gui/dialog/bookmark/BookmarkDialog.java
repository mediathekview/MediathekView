/*
 * Created by JFormDesigner on Sun Apr 06 14:18:54 CEST 2025
 */

package mediathek.gui.dialog.bookmark;

import java.net.URI;
import java.net.URISyntaxException;
import javafx.event.ActionEvent;
import javafx.scene.control.Tooltip;
import javax.swing.table.*;

import static mediathek.config.StandardLocations.getBookmarkFilePath;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import java.awt.*;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.swing.*;
import com.intellij.uiDesigner.core.*;
import mediathek.daten.bookmark.DatenBookmark;
import mediathek.daten.bookmark.ListeBookmark;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.javafx.bookmark.BookmarkData;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.SVGIconUtilities;
import mediathek.tool.SwingErrorDialog;
import mediathek.tool.models.BookmarkModel;
import net.miginfocom.swing.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jdesktop.swingx.*;

/**
 * @author Markus
 */
public class BookmarkDialog extends JDialog {

  private static final Logger logger = LogManager.getLogger();
  private static final String JSON_DATEI = getBookmarkFilePath().toString();
  private List<DatenBookmark> bookmarks;
  private BookmarkModel model;
  private int filterState;
  private int divposition;

  public BookmarkDialog(Window owner) {
    super(owner);
    bookmarks = ladeBookmarks(JSON_DATEI);
    model = new BookmarkModel(bookmarks);
    initComponents();
    tabelle.setModel(model);
    initActions();
    initIcons();
    filterState = -1;
    btnShowDetails.setSelected(ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".details", true));
    double ratio = ApplicationConfiguration.getConfiguration().getDouble(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".divider", 0.5);
    divposition = (int) (splitPane.getHeight() * ratio);


  }

  private void initIcons() {
    btnDeleteEntry.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/trash-can.svg"));
    btnMarkViewed.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/eye.svg"));
    btnEditNote.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/pen.svg"));
    btnSaveList.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/floppy-disk.svg"));
    btnShowDetails.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-info.svg"));
    btnFilter.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/filter.svg"));
  }

  private void initComponents() {
    // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
    // Generated using JFormDesigner Educational license - Markus Jannek
    toolBar = new JToolBar();
    btnDeleteEntry = new JButton();
    btnMarkViewed = new JButton();
    btnEditNote = new JButton();
    panel1 = new JPanel();
    btnSaveList = new JButton();
    btnShowDetails = new JToggleButton();
    btnFilter = new JButton();
    splitPane = new JSplitPane();
    tableScrollPane = new JScrollPane();
    tabelle = new JTable();
    bottomPanel = new JPanel();
    taScrollPane = new JScrollPane();
    taDescription = new JTextArea();
    hyperlink = new JXHyperlink();

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

    //======== toolBar ========
    {

      //---- btnDeleteEntry ----
      btnDeleteEntry.setToolTipText("Aus der Merkliste l\u00f6schen");
      toolBar.add(btnDeleteEntry);
      toolBar.add(btnMarkViewed);

      //---- btnEditNote ----
      btnEditNote.setToolTipText("Anmerkungen bearbeiten");
      toolBar.add(btnEditNote);

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
      toolBar.add(panel1);

      //---- btnSaveList ----
      btnSaveList.setToolTipText("Ge\u00e4nderte Merkliste abspeichern");
      toolBar.add(btnSaveList);
      toolBar.addSeparator();

      //---- btnShowDetails ----
      btnShowDetails.setToolTipText("Erweiterte Film Informationen anzeigen");
      toolBar.add(btnShowDetails);
      toolBar.add(btnFilter);
    }
    contentPane.add(toolBar, "cell 0 0 3 1");

    //======== splitPane ========
    {
      splitPane.setOrientation(JSplitPane.VERTICAL_SPLIT);

      //======== tableScrollPane ========
      {

        //---- tabelle ----
        tabelle.setModel(new DefaultTableModel(
          new Object[][] {
            {null, null},
            {null, null},
          },
          new String[] {
            null, null
          }
        ));
        tabelle.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
        tableScrollPane.setViewportView(tabelle);
      }
      splitPane.setTopComponent(tableScrollPane);

      //======== bottomPanel ========
      {
        bottomPanel.setLayout(new GridLayoutManager(3, 2, new Insets(0, 0, 0, 0), -1, -1));

        //======== taScrollPane ========
        {

          //---- taDescription ----
          taDescription.setEditable(false);
          taScrollPane.setViewportView(taDescription);
        }
        bottomPanel.add(taScrollPane, new GridConstraints(0, 0, 1, 1,
          GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_NONE,
          GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW,
          GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW,
          null, null, null));

        //---- hyperlink ----
        hyperlink.setText("Link zur Webseite");
        bottomPanel.add(hyperlink, new GridConstraints(2, 0, 1, 1,
          GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_NONE,
          GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW,
          GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW,
          null, null, null));
      }
      splitPane.setBottomComponent(bottomPanel);
    }
    contentPane.add(splitPane, "cell 0 1 3 2");
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

    // Tabelle: Auswahl-Listener für Hyperlink Tooltip
    tabelle.getSelectionModel().addListSelectionListener(e -> {
      if (!e.getValueIsAdjusting()) {
        int selectedRow = tabelle.getSelectedRow();
        if (selectedRow >= 0) {
          int modelRow = tabelle.convertRowIndexToModel(selectedRow);
          DatenBookmark bookmark = model.getBookmarks().get(modelRow);
          if (bookmark.getUrl() != null && !bookmark.getUrl().isEmpty()) {
            hyperlink.setToolTipText(bookmark.getUrl());
            hyperlink.setEnabled(true);
          } else {
            hyperlink.setToolTipText("");
            hyperlink.setEnabled(false);
          }
        } else {
          hyperlink.setToolTipText("");
          hyperlink.setEnabled(false);
        }
      }
    });


    //Hyperlink
    hyperlink.addActionListener(e -> {
      if (!hyperlink.getToolTipText().isEmpty()) {
        var toolTipText = hyperlink.getToolTipText();
        if (Desktop.isDesktopSupported()) {
          var d = Desktop.getDesktop();
          if (d.isSupported(Desktop.Action.BROWSE)) {
            try {
              d.browse(new URI(toolTipText));
            } catch (Exception ex) {
              SwingErrorDialog.showExceptionMessage(
                  MediathekGui.ui(),
                  "Es trat ein Fehler beim Öffnen des Links auf.\nSollte dies häufiger auftreten kontaktieren Sie bitte das Entwicklerteam.",
                  ex);
            }
          } else {
            openUrl(toolTipText);
          }
        } else {
          openUrl(toolTipText);
        }
      }
    });
  }

  private void openUrl(String url) {
    try {
      UrlHyperlinkAction.openURL(url);
    } catch (URISyntaxException ex) {
      logger.warn(ex);
    }
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
  /*private void btnFilterAction(ActionEvent e) {
    if (++filterState > 2) {
      filterState = 0;
    }
    switch (filterState) {
      case 0 -> filteredBookmarkList.setPredicate(f -> true);  // show all
      case 1 -> filteredBookmarkList.setPredicate(film -> { // show only unseen
        return !film.getSeen();
      });
      case 2 ->
        // show only seen
          filteredBookmarkList.setPredicate(BookmarkData::getSeen);
    }
    btnFilter.setTooltip(new Tooltip(BTNFILTER_TOOLTIPTEXT[FilterState]));
    lblFilter.setText(LBLFILTER_MESSAGETEXT[FilterState]);
    lblSeen.setDisable(LBLSEEN_DISABLE[FilterState]);
    refresh();
  }*/
  // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables  @formatter:off
  // Generated using JFormDesigner Educational license - Markus Jannek
  private JToolBar toolBar;
  private JButton btnDeleteEntry;
  private JButton btnMarkViewed;
  private JButton btnEditNote;
  private JPanel panel1;
  private JButton btnSaveList;
  private JToggleButton btnShowDetails;
  private JButton btnFilter;
  private JSplitPane splitPane;
  private JScrollPane tableScrollPane;
  private JTable tabelle;
  private JPanel bottomPanel;
  private JScrollPane taScrollPane;
  private JTextArea taDescription;
  private JXHyperlink hyperlink;
  // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
