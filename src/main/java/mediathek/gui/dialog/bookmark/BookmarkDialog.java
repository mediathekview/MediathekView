/*
 * Created by JFormDesigner on Sun Apr 06 14:18:54 CEST 2025
 */

package mediathek.gui.dialog.bookmark;

import java.util.List;
import mediathek.config.Daten;
import mediathek.gui.actions.UrlHyperlinkAction;
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
  enum FilterState {
    STATE1, STATE2, STATE3
  }
  private static final Logger logger = LogManager.getLogger();
  private static final String JSON_DATEI = getBookmarkFilePath().toString();
  private final BookmarkModel model;
  //private int filterState; // nimm enum dafür! -> Typsicherheit
  private FilterState filterState; // Beispiel!

  /*
  Warum hast Du den komplexesten Layout Manager MigLayout genommen um den Dialog zu bauen?
  Und dann noch vergewaltigt.
  Ein Panel als Platzhalter in der Toolbar zu verwenden ist scheiße. Das sieht in der Größe ggf gut aus,
  wenn man resized geht das ganze schon in die Hose.
  Dann zig Reihen deren Logik sich mir nicht erschlossen hat. Der Splitter hat auch nicht richtig funktioniert u.a. wegen der
  sinnlosen rows usw.
  Ich hab das ganze nun weggeworfen und mit BorderLayout realisiert. Wenn das JTabbedPane ein/ausgebendet wird erzielt das denselben
  Effekt und das DescriptionPanel später wird eh nicht resized.

  Eigentlich besteht der Dialog nur aus 3 Teilen:
   oben: Toolbar
   mitte: (soll den Dialog primär ausfüllen): Tabelle
   unten: Filmbeschreibungspanel bei Bedarf

   Hier ist BorderLayout NORTH/CENTER/SOUTH deutlich einfacher anzuwenden und zu debuggen. Generiert auch weniger Code.
   */
  public BookmarkDialog(Window owner) {
    super(owner);
    initComponents();

    //deine bookmarks Referenz ist überflüssig wenn man ein wenig anders herangeht. so verwirrt der code auch nicht mehr
    model = new BookmarkModel(Daten.getInstance().getListeBookmark().getBookmarks());
    tabelle.setModel(model);
    initActions();
    initIcons();
    //filterState = -1;
    filterState = FilterState.STATE1;

    /*
     * Der key ist schlechter Stil. So kannst Du später nie sauber nach Referenzen suchen.
     * public static final String APPLICATION_UI_BOOKMARKLIST_DETAILS = APPLICATION_UI_BOOKMARKLIST + ".details";
     * ist okay, danach kann man dann auch entspannt suchen. Der Rest ist suboptimal.
     */
    btnShowDetails.setSelected(ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".details", true));
  }

  private void initIcons() {
    //TODO finde ich keinen guten Stil, da Logik und setup getrennt sind, das icon kann gerne in initActions rutschen
    btnDeleteEntry.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/trash-can.svg"));
    btnMarkViewed.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/eye.svg"));
    btnEditNote.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/pen.svg"));
    btnSaveList.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/floppy-disk.svg"));
    btnShowDetails.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-info.svg"));
    btnFilter.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/filter.svg"));
  }

  private void initComponents() {
    // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
    // Generated using JFormDesigner non-commercial license
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
    setTitle("Merkliste verwalten"); //NON-NLS
    setPreferredSize(new Dimension(800, 400));
    var contentPane = getContentPane();
    contentPane.setLayout(new BorderLayout());

    //======== toolBar ========
    {
        toolBar.setFloatable(false);

        //---- btnDeleteEntry ----
        btnDeleteEntry.setToolTipText("Aus der Merkliste l\u00f6schen"); //NON-NLS
        toolBar.add(btnDeleteEntry);
        toolBar.add(btnMarkViewed);

        //---- btnEditNote ----
        btnEditNote.setToolTipText("Anmerkungen bearbeiten"); //NON-NLS
        toolBar.add(btnEditNote);
        toolBar.add(hSpacer1);

        //---- btnSaveList ----
        btnSaveList.setToolTipText("Ge\u00e4nderte Merkliste abspeichern"); //NON-NLS
        toolBar.add(btnSaveList);
        toolBar.addSeparator();

        //---- btnShowDetails ----
        btnShowDetails.setToolTipText("Erweiterte Film Informationen anzeigen"); //NON-NLS
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

            //---- label1 ----
            label1.setText("Hier kommt sp\u00e4ter mal ein FilmDescriptionPanel rein. Das muss nur noch angepasst werden da es schon existiert."); //NON-NLS
            panel1.add(label1, BorderLayout.CENTER);

            //---- taDescription ----
            taDescription.setText("Simuliert deine Beschreibung"); //NON-NLS
            panel1.add(taDescription, BorderLayout.NORTH);

            //---- hyperlink ----
            hyperlink.setText("Link zur Webseite"); //NON-NLS
            panel1.add(hyperlink, BorderLayout.SOUTH);
        }
        tabbedPane1.addTab("Beschreibung", panel1); //NON-NLS
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


    // Tabelle: Auswahl-Listener für Hyperlink Tooltip
    tabelle.getSelectionModel().addListSelectionListener(e -> {
      //TODO wird mit Einbau eines FilmDescriptionPanel später (fast) überflüssig
      // davon abgesehen sind zwei listener da etwas übertrieben.
      if (!e.getValueIsAdjusting()) {
        int selectedRow = tabelle.getSelectedRow();
        if (selectedRow >= 0) {
          int modelRow = tabelle.convertRowIndexToModel(selectedRow);
          var bookmark = (DatenBookmark) model.getValueAt(modelRow, BookmarkModel.BOOKMARK_REF);//bookmarks.get(modelRow);
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
  // Generated using JFormDesigner non-commercial license
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
  private JTable tabelle;
  // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
