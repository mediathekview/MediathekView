/*
 * Created by JFormDesigner on Sun Apr 06 14:18:54 CEST 2025
 */

package mediathek.gui.dialog.bookmark;

import java.util.List;
import java.util.Optional;
import mediathek.config.Daten;
import mediathek.daten.DatenFilm;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.gui.tabs.tab_film.FilmDescriptionPanel;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.SVGIconUtilities;
import mediathek.tool.SwingErrorDialog;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jdesktop.swingx.JXHyperlink;

import javax.swing.*;
import java.awt.*;
import java.net.URI;
import java.net.URISyntaxException;

import static mediathek.config.StandardLocations.getBookmarkFilePath;

/**
 * @author Markus
 */
public class BookmarkDialog extends JDialog {

  private static BookmarkDialog instance;

  enum FilterState {
    STATE1, STATE2, STATE3;
  }
  private static final Logger logger = LogManager.getLogger();
  private static final String JSON_DATEI = getBookmarkFilePath().toString();
  private final BookmarkModel model;
  //private int filterState; // nimm enum dafür! -> Typsicherheit
  private FilterState filterState; // Beispiel!
  public BookmarkDialog(Window owner) {
    super(owner);
    instance = this;
    initComponents();

    //deine bookmarks Referenz ist überflüssig wenn man ein wenig anders herangeht. so verwirrt der code auch nicht mehr
    model = new BookmarkModel(Daten.getInstance().getListeBookmark().getBookmarks());
    tabelle.setModel(model);
    initActions();
    filterState = FilterState.STATE1;
tabbedPane1.remove(0);
    /*
     * Der key ist schlechter Stil. So kannst Du später nie sauber nach Referenzen suchen.
     * public static final String APPLICATION_UI_BOOKMARKLIST_DETAILS = APPLICATION_UI_BOOKMARKLIST + ".details";
     * ist okay, danach kann man dann auch entspannt suchen. Der Rest ist suboptimal.
     */
    btnShowDetails.setSelected(ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".details", true));

    FilmDescriptionPanel descriptionPanel = new FilmDescriptionPanel();

    descriptionPanel.install(tabbedPane1, tabelle, () -> {
      int selectedRow = tabelle.getSelectedRow();
      if (selectedRow < 0) {
        return Optional.empty();
      }
      int modelRow = tabelle.convertRowIndexToModel(selectedRow);

      BookmarkModel model = (BookmarkModel) tabelle.getModel();

      DatenBookmark bookmark = (DatenBookmark) model.getValueAt(modelRow,BookmarkModel.BOOKMARK_REF);

      if (bookmark == null) {
        return Optional.empty();
      }

      DatenFilm datenFilm = bookmark.getDatenFilm();

      return Optional.ofNullable(datenFilm);
    });

  }

  public static void refresh() {
    if (instance != null) {
      instance.getOwner().repaint();
    }
  }

  private void initComponents() {
    // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
    // Generated using JFormDesigner Educational license - Markus Jannek
    toolBar = new JToolBar();
    btnDeleteEntry = new JButton();
    btnMarkViewed = new JButton();
    btnEditNote = new JButton();
    hSpacer1 = new JPanel(null);
    btnSaveList = new JButton();
    btnShowDetails = new JToggleButton();
    btnFilter = new JButton();
    tabbedPane1 = new JTabbedPane();
    panel1 = new JPanel();
    label1 = new JLabel();
    taDescription = new JLabel();
    hyperlink = new JXHyperlink();
    scrollPane1 = new JScrollPane();
    tabelle = new JTable();

    //======== this ========
    setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
    setTitle("Merkliste verwalten");
    setPreferredSize(new Dimension(800, 400));
    var contentPane = getContentPane();
    contentPane.setLayout(new BorderLayout());

    //======== toolBar ========
    {
      toolBar.setFloatable(false);

      //---- btnDeleteEntry ----
      btnDeleteEntry.setToolTipText("Aus der Merkliste l\u00f6schen");
      toolBar.add(btnDeleteEntry);
      toolBar.add(btnMarkViewed);

      //---- btnEditNote ----
      btnEditNote.setToolTipText("Anmerkungen bearbeiten");
      toolBar.add(btnEditNote);
      toolBar.add(hSpacer1);

      //---- btnSaveList ----
      btnSaveList.setToolTipText("Ge\u00e4nderte Merkliste abspeichern");
      toolBar.add(btnSaveList);
      toolBar.addSeparator();

      //---- btnShowDetails ----
      btnShowDetails.setToolTipText("Erweiterte Film Informationen anzeigen");
      toolBar.add(btnShowDetails);
      toolBar.add(btnFilter);
    }
    contentPane.add(toolBar, BorderLayout.NORTH);

    //======== tabbedPane1 ========
    {

      //======== panel1 ========
      {
        panel1.setMinimumSize(new Dimension(315, 150));
        panel1.setLayout(new BorderLayout());
        panel1.add(label1, BorderLayout.CENTER);
        panel1.add(taDescription, BorderLayout.NORTH);

        //---- hyperlink ----
        hyperlink.setText("Link zur Webseite");
        panel1.add(hyperlink, BorderLayout.SOUTH);
      }
      tabbedPane1.addTab("Beschreibung", panel1);
    }
    contentPane.add(tabbedPane1, BorderLayout.SOUTH);

    //======== scrollPane1 ========
    {

      //---- tabelle ----
      tabelle.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
      tabelle.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
      scrollPane1.setViewportView(tabelle);
    }
    contentPane.add(scrollPane1, BorderLayout.CENTER);
    pack();
    setLocationRelativeTo(getOwner());
    // JFormDesigner - End of component initialization  //GEN-END:initComponents  @formatter:on
  }

  private void initActions() {
    btnDeleteEntry.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/trash-can.svg"));
    btnMarkViewed.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/eye.svg"));
    btnEditNote.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/pen.svg"));
    btnSaveList.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/floppy-disk.svg"));
    btnShowDetails.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-info.svg"));
    btnFilter.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/filter.svg"));
    // Löschen
    btnDeleteEntry.addActionListener(e -> {
      int selectedRow = tabelle.getSelectedRow();
      // hier wird -1 zurückgeliefert, also warum nicht danach abfragen?
      if (selectedRow != -1) {
        int modelRow = tabelle.convertRowIndexToModel(selectedRow);
        var bookmark = (DatenBookmark) model.getValueAt(modelRow, BookmarkModel.BOOKMARK_REF);//bookmarks.get(modelRow);
        int confirm = JOptionPane.showConfirmDialog(this,
                "Eintrag wirklich löschen?\n" + bookmark.getTitel(),
            "Löschen bestätigen", JOptionPane.YES_NO_OPTION);
        if (confirm == JOptionPane.YES_OPTION) {
          model.removeRow(modelRow);
          var film = bookmark.getDatenFilm();
          film.setBookmark(null); // löscht die Referenz zur Bookmark, wichtig!
          getOwner().repaint(); // mit repaint wird das Tab aktualisiert
        }
      } else {
        JOptionPane.showMessageDialog(this, "Bitte erst einen Eintrag auswählen.", "Hinweis", JOptionPane.INFORMATION_MESSAGE);
      }
    });

    // Speichern
    btnSaveList.addActionListener(e -> {
      Daten.getInstance().getListeBookmark().saveToFile();
      JOptionPane.showMessageDialog(this, "Merkliste gespeichert.", "Info", JOptionPane.INFORMATION_MESSAGE);
    });

    // Info anzeigen
    btnShowDetails.addActionListener(e -> {
      //Hier soll die Filmbeschreibung angezeigt werden für die Selection
      // der button blendet die Filmbeschreibung ein/aus
      // deine logik ist hier falsch denn sie zeigt Info an wenn man auf den button klickt. schau dir das original nochmal an
      //FIXME fehlerhafte Logik, wird durch FilmDescriptionPanel nachher auch teils überflüssig
      final var btnSelected = btnShowDetails.isSelected();

      if (btnSelected) {
        int selectedRow = tabelle.getSelectedRow();
        if (selectedRow != -1) {
          int modelRow = tabelle.convertRowIndexToModel(selectedRow);
          var bookmark = (DatenBookmark)model.getValueAt(modelRow, BookmarkModel.BOOKMARK_REF);//bookmarks.get(modelRow);
          var film = bookmark.getDatenFilm();
          taDescription.setText(film != null ? film.getDescription() : "");
        } else {
          taDescription.setText("Kein Eintrag ausgewählt.");
        }
      } else {
        taDescription.setText("");
      }
      taDescription.setVisible(btnSelected);
      hyperlink.setVisible(btnSelected);
    });

    // Gesehen/Ungesehen markieren
    btnMarkViewed.addActionListener(e -> {
      int selectedRow = tabelle.getSelectedRow();
      if (selectedRow != -1) {
        int modelRow = tabelle.convertRowIndexToModel(selectedRow);
        var bookmark = (DatenBookmark) model.getValueAt(modelRow, BookmarkModel.BOOKMARK_REF);
        boolean isSeen = bookmark.getSeen();
        bookmark.setSeen(!isSeen); // Toggle Zustand
        model.fireTableRowsUpdated(modelRow, modelRow); // Tabelle updaten
        getOwner().repaint(); // UI neu zeichnen

        // Icon anpassen
        if (bookmark.getSeen()) {
          btnMarkViewed.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/eye.svg"));
        } else {
          btnMarkViewed.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/eye-slash.svg"));
        }
      } else {
        JOptionPane.showMessageDialog(this, "Bitte erst einen Eintrag auswählen.", "Hinweis", JOptionPane.INFORMATION_MESSAGE);
      }
    });

    // Tabelle: Auswahl-Listener für Hyperlink Tooltip
    tabelle.getSelectionModel().addListSelectionListener(e -> {
      //TODO wird mit Einbau eines FilmDescriptionPanel später (fast) überflüssig
      // davon abgesehen sind zwei listener da etwas übertrieben.
      if (!e.getValueIsAdjusting()) {
        int selectedRow = tabelle.getSelectedRow();
        if (selectedRow >= 0) {
          int modelRow = tabelle.convertRowIndexToModel(selectedRow);
          var bookmark = (DatenBookmark) model.getValueAt(modelRow, BookmarkModel.BOOKMARK_REF);//bookmarks.get(modelRow);
          boolean isSeen = bookmark.getSeen();
          if(isSeen){
            btnMarkViewed.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/eye.svg"));
          }else{
            btnMarkViewed.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/eye-slash.svg"));
          }
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

  public static BookmarkModel getModel(){
    return (BookmarkModel) tabelle.getModel();
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
  private JPanel hSpacer1;
  private JButton btnSaveList;
  private JToggleButton btnShowDetails;
  private JButton btnFilter;
  private JTabbedPane tabbedPane1;
  private JPanel panel1;
  private JLabel label1;
  private JLabel taDescription;
  private JXHyperlink hyperlink;
  private JScrollPane scrollPane1;
  private static JTable tabelle;
  // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
