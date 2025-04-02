package mediathek.gui.bookmark;

import java.awt.Frame;
import java.awt.BorderLayout;
import java.util.List;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.WindowConstants;
import javax.swing.table.TableColumn;
import mediathek.config.Daten;
import mediathek.daten.bookmark.DatenBookmark;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.SVGIconUtilities;
import mediathek.tool.listener.BeobTableHeader;
import mediathek.tool.models.TModelBookmark;
import mediathek.tool.table.MVBookmarkTable;
import org.apache.commons.configuration2.sync.LockMode;
import org.jdesktop.swingx.JXHyperlink;

public class BookmarkDialog extends JDialog {
  private JToolBar toolBar;
  private JButton btnDeleteEntry, btnMarkViewed, btnEditNote, btnSaveList, btnFilter;
  private JToggleButton btnShowDetails;
  private JLabel lblCount, lblSeen, lblFilter, lblMessage;
  private JSplitPane spSplitPane;
  private MVBookmarkTable tabelle;
  private JXHyperlink hyperLink;
  private JTextArea taDescription;

  public BookmarkDialog(Frame parent) {
    super(parent, true);
    setTitle("Merkliste verwalten");
    setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
    setLayout(new BorderLayout());

    initComponents();
    restoreWindowSizeFromConfig();
    restoreColumnsFromConfig();
    loadDataToTable();
  }

  private void loadDataToTable() {
    List<DatenBookmark> list = Daten.getInstance().getListeBookmark().getList();
    int dataItems = list.size();
    System.out.println(list.get(0).getThema());
  }

  private void initComponents() {
    toolBar = new JToolBar();
    btnDeleteEntry = createToolbarButton("icons/fontawesome/minus.svg", "Aus der Merkliste löschen");
    btnMarkViewed = createToolbarButton("icons/fontawesome/eye.svg", "Als gesehen markieren");
    btnEditNote = createToolbarButton("icons/fontawesome/pen.svg", "Anmerkungen bearbeiten");
    btnSaveList = createToolbarButton("icons/fontawesome/floppy-disk.svg", "Merkliste speichern");
    btnShowDetails = new JToggleButton(SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-info.svg"));
    btnFilter = createToolbarButton("icons/fontawesome/filter.svg", "Filter setzen");

    toolBar.add(btnDeleteEntry);
    toolBar.add(btnMarkViewed);
    toolBar.add(btnEditNote);
    toolBar.add(new JLabel());
    toolBar.add(btnSaveList);
    toolBar.add(new JToolBar.Separator());
    toolBar.add(btnShowDetails);
    toolBar.add(btnFilter);

    tabelle = new MVBookmarkTable();
    tabelle.setModel(new TModelBookmark());
    tabelle.getTableHeader().addMouseListener(new BeobTableHeader(tabelle,
        DatenBookmark.spaltenAnzeigen,
        new int[]{DatenBookmark.BOOKMARK_ABSPIELEN, DatenBookmark.BOOKMARK_AUFZEICHNEN},
        new int[]{DatenBookmark.BOOKMARK_ABSPIELEN, DatenBookmark.BOOKMARK_AUFZEICHNEN},
        true, true, null));

    JScrollPane tableScrollPane = new JScrollPane(tabelle);

    taDescription = new JTextArea(5, 20);
    JScrollPane textScrollPane = new JScrollPane(taDescription);

    hyperLink = new JXHyperlink();
    hyperLink.setText("Link zur Webseite");

    JPanel bottomPanel = new JPanel(new BorderLayout());
    bottomPanel.add(textScrollPane, BorderLayout.NORTH);
    bottomPanel.add(hyperLink, BorderLayout.CENTER);

    spSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, tableScrollPane, bottomPanel);

    JPanel statusPanel = new JPanel();
    lblCount = new JLabel("Label");
    lblSeen = new JLabel("Label");
    lblFilter = new JLabel("Label");
    lblMessage = new JLabel("Label");
    statusPanel.add(lblCount);
    statusPanel.add(lblSeen);
    statusPanel.add(lblFilter);
    statusPanel.add(lblMessage);

    add(toolBar, BorderLayout.NORTH);
    add(spSplitPane, BorderLayout.CENTER);
    add(statusPanel, BorderLayout.SOUTH);

    pack();
  }

  private JButton createToolbarButton(String iconPath, String tooltip) {
    JButton button = new JButton(SVGIconUtilities.createSVGIcon(iconPath));
    button.setToolTipText(tooltip);
    return button;
  }

  private void restoreColumnsFromConfig() {
    var config = ApplicationConfiguration.getConfiguration();
    try {
      config.lock(LockMode.READ);
      int ansColumn = config.getInt(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".columns.no", 0);
      for (int i = 1; i <= ansColumn; i++) {
        boolean isVisible = config.getBoolean(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".columns.col" + i + ".visible", true);
        int colWidth = config.getInt(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".columns.col" + i + ".size", 100);
        TableColumn column = tabelle.getColumnModel().getColumn(i - 1);
        if (!isVisible) {
          column.setMinWidth(0);
          column.setPreferredWidth(0);
          column.setMaxWidth(0);
        } else {
          column.setMinWidth(10);
          column.setPreferredWidth(colWidth);
          column.setMaxWidth(3000);
        }
      }
    } finally {
      config.unlock(LockMode.READ);
    }
  }

  private void restoreWindowSizeFromConfig() {
    var config = ApplicationConfiguration.getConfiguration();
    try {
      config.lock(LockMode.READ);
      int width = config.getInt(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".width", 640);
      int height = config.getInt(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".height", 480);
      int x = config.getInt(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".location.x", 0);
      int y = config.getInt(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".location.y", 0);
      setBounds(x, y, width, height);
    } finally {
      config.unlock(LockMode.READ);
    }
  }

  @Override
  public void dispose() {
    tabelleSpeichern();
    super.dispose();
  }

  private void tabelleSpeichern() {
    if (tabelle != null) {
      tabelle.writeTableConfigurationData();
    }
  }
}
