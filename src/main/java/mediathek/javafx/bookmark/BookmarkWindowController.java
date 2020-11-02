package mediathek.javafx.bookmark;

import javafx.application.Platform;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.collections.transformation.SortedList;
import javafx.event.ActionEvent;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.geometry.Insets;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TextArea;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.image.Image;
import javafx.scene.input.ContextMenuEvent;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.layout.CornerRadii;
import javafx.scene.paint.Color;
import javafx.stage.Modality;
import javafx.stage.Stage;
import jiconfont.icons.FontAwesome;
import jiconfont.javafx.IconNode;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;
import mediathek.daten.DatenPset;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.gui.dialog.DialogAddMoreDownload;
import mediathek.gui.messages.DownloadListChangedEvent;
import mediathek.gui.tabs.tab_film.GuiFilme;
import mediathek.javafx.tool.JavaFxUtils;
import mediathek.javafx.tool.TableViewColumnContextMenuHelper;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.MVC;
import org.apache.commons.configuration2.Configuration;
import org.apache.commons.configuration2.sync.LockMode;
import org.apache.logging.log4j.LogManager;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.datatransfer.StringSelection;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.ResourceBundle;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import static javafx.scene.input.MouseButton.PRIMARY;
import static mediathek.config.MVColor.*;


/**
 * Bookmark window controller:
 * Create and display bookmark window
 *
 * @author Klaus Wich <klaus.wich@aim.com>
 */
public class BookmarkWindowController implements Initializable {

  private Stage stage;
  private final BookmarkDataList listeBookmarkList;
  private FilteredList<BookmarkData> filteredBookmarkList;
  private Color ColorExpired;
  private Color ColorLive;
  private Background BackgroundSeen;
  private Background BackgroundSelected;
  private final SeenHistoryController history = new SeenHistoryController();
  private MenuItem playitem;
  private MenuItem loaditem;
  private MenuItem deleteitem;
  private MenuItem viewitem;
  private MenuItem webitem;
  private MenuItem ccopyitem;
  private MenuItem edititem;
  private ContextMenu cellContextMenu;
  private GuiFilme infotab;  // used for update information
  private double divposition;
  private boolean listUpdated; // indicates new updates to bookmarklist
  private ScheduledFuture<?> SaveBookmarkTask; // Future task to save
  private int FilterState;

  @FXML
  private Button btnSaveList;
  @FXML
  private Button btnDeleteEntry;
  @FXML
  private Button btnMarkViewed;
  @FXML
  private ToggleButton btnShowDetails;
  @FXML
  private Button btnFilter;
  @FXML
  private Button btnEditNote;
  @FXML
  private TableView<BookmarkData> tbBookmarks;
  @FXML
  private TableColumn<BookmarkData, String> colSender;
  @FXML
  private TableColumn<BookmarkData, String> colTheme;
  @FXML
  private TableColumn<BookmarkData, String> colTitle;
  @FXML
  private TableColumn<BookmarkData, String> colDuration;
  @FXML
  private TableColumn<BookmarkData, String> colRunDate;
  @FXML
  private TableColumn<BookmarkData, String> colUrl;
  @FXML
  private TableColumn<BookmarkData, String> colBtnPlay;
  @FXML
  private TableColumn<BookmarkData, String> colBtnDownload;
  @FXML
  private TableColumn<BookmarkData, String> colNote;
  @FXML
  private TableColumn<BookmarkData, String> colExpiry;
  @FXML
  private Label lblCount;
  @FXML
  private Label lblSeen;
  @FXML
  private Label lblMessage;
  @FXML
  private Label lblFilter;
  @FXML
  private TextArea taDescription;
  @FXML
  private SplitPane spSplitPane;
  @FXML
  private Hyperlink hyperLink;

  public BookmarkWindowController() {
    listeBookmarkList = Daten.getInstance().getListeBookmarkList();
    listUpdated = false;
  }

  /**
   * Marks all selected films as seen if unseen (at least one in selection)
   * or unseen if all are seen
   */
  @FXML
  private void btnMarkEntryAsViewed(Event e) {
    ObservableList<BookmarkData> selections = tbBookmarks.getSelectionModel().getSelectedItems();
    if (!selections.isEmpty()) {
      boolean hasUnSeen = isUnSeenSelected(); // true if unseen in selection
      List<BookmarkData> bookmarks = new ArrayList<>(selections); // copy list
      List<DatenFilm> filmlist = new ArrayList<>();
      bookmarks.forEach((data) -> {
        data.setSeen(hasUnSeen);
        DatenFilm film = data.getDatenFilm();
        if (film != null) {
          filmlist.add(film);
        }
      });
      if (hasUnSeen) {
        history.markSeen(filmlist);
      }
      else {
        history.markUnseen(filmlist);
      }
      setSeenButtonState(hasUnSeen, selections.size() > 1);
       // reselect to trigger updates:
      tbBookmarks.getSelectionModel().clearSelection();
      bookmarks.forEach((data) -> tbBookmarks.getSelectionModel().select(data));
    }
  }



  @FXML
  private void btnSaveBookMarkList(Event e) {
    cancelBookmarkSave();
    saveBookMarkList();
  }

  @FXML
  private void btnDeleteEntry(Event e) {
    ArrayList<BookmarkData> selections = new ArrayList<>(tbBookmarks.getSelectionModel().getSelectedItems());
    if (!selections.isEmpty()) {
      listeBookmarkList.deleteEntries(tbBookmarks.getSelectionModel().getSelectedItems());
      updateDisplay();
      tbBookmarks.getSelectionModel().clearSelection();
      infotab.repaint(); // Update display in GuiFilme
    }
  }

  @FXML
  private void btnEditNote(Event e) {

    Stage dlgstage = new Stage();
    dlgstage.initModality(Modality.WINDOW_MODAL);
    dlgstage.initOwner(this.stage);
    try {
      FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("/mediathek/res/programm/fxml/bookmarkNoteDialog.fxml"));
      BookmarkNoteDialog bdialog = new BookmarkNoteDialog();
      fxmlLoader.setController(bdialog);
      Scene scene = new Scene(fxmlLoader.load());
      scene.getStylesheets().add(getClass().getResource("/mediathek/res/css/bookmarkNoteDialog.css").toExternalForm());
      dlgstage.getIcons().add(new Image("/mediathek/res/MediathekView.png"));
      dlgstage.setScene(scene);
      if (bdialog.SetandShow(dlgstage, tbBookmarks.getSelectionModel().getSelectedItem())) {
        listUpdated = true;
        refresh();
      }
    }
    catch (IOException ex) {
      LogManager.getLogger(BookmarkWindowController.class).error("{} Can't find/load the FXML description! Exception - {}",
                                                                  getClass(), ex.toString());
    }
  }

  @FXML
  private void hyperLinkSelected(Event e) {
    String url = tbBookmarks.getSelectionModel().getSelectedItem().getWebUrl();
    try {
      if (url != null) {
        UrlHyperlinkAction.openURL(null,url);
      }
    }
    catch (URISyntaxException ex) {
      LogManager.getLogger(BookmarkWindowController.class).error("{} Hyperlink Syntax exception - {}", getClass(), ex.toString());
    }
  }

  @SuppressWarnings("unchecked")
  private void copy2Clipboard(Event e) {
    TablePosition<BookmarkData, String> pos = tbBookmarks.getSelectionModel().getSelectedCells().get(0);
    BookmarkData item = tbBookmarks.getItems().get(pos.getRow());
    String data = pos.getTableColumn().getCellObservableValue(item).getValue();
    Toolkit.getDefaultToolkit().getSystemClipboard().setContents(new StringSelection(data), null);
  }

  @Override
  public void initialize(URL arg0, ResourceBundle arg1) {
    restoreTableStateAndContextMenu();
    // connect columns with underlying data
    colSender.setCellValueFactory(new PropertyValueFactory<>("sender"));
    colTheme.setCellValueFactory(new PropertyValueFactory<>("thema"));
    colTitle.setCellValueFactory(new PropertyValueFactory<>("titel"));
    colDuration.setCellValueFactory(new PropertyValueFactory<>("dauer"));
    colRunDate.setCellValueFactory(new PropertyValueFactory<>("sendDate"));
    colRunDate.setComparator(new BookmarkDateComparator());
    colUrl.setCellValueFactory(new PropertyValueFactory<>("url"));
    colNote.setCellValueFactory(new PropertyValueFactory<>("note"));
    colExpiry.setCellValueFactory(new PropertyValueFactory<>("expiry"));
    colExpiry.setComparator(new BookmarkDateComparator());

    // add button to play URL:
    colBtnPlay.setCellFactory((final var UNUSED) -> new TableCell<>() {
      @Override
      public void updateItem(String item, boolean empty) {
        super.updateItem(item, empty);
        if (empty || getTableView().getItems().get(getIndex()).isNotInFilmList()) {
          setGraphic(null);
        } else {
          setGraphic(new IconNode(FontAwesome.PLAY));
          this.setOnMouseClicked(UNUSED -> playAction(getTableView().getItems().get(getIndex())));
        }
      }
    });

    // add button to download URL:
    colBtnDownload.setCellFactory((final var UNUSED) -> new TableCell<>() {
      @Override
      public void updateItem(String item, boolean empty) {
        super.updateItem(item, empty);
        if (empty || !getTableView().getItems().get(getIndex()).hasURL()) {
          setGraphic(null);
        } else {
          setGraphic(new IconNode(FontAwesome.DOWNLOAD));
          this.setOnMouseClicked(UNUSED -> loadAction(getTableView().getItems().get(getIndex())));
        }
      }
    });

    colExpiry.setCellFactory((final var UNUSED) -> new TableCell<>() {
      @Override
      public void updateItem(String item, boolean empty) {
        super.updateItem(item, empty);
        if (!empty) {
          BookmarkData data = getTableView().getItems().get(getIndex());
          this.setText(data.getExpiry());
          if (data.willExpire()) {
            this.getStyleClass().add("Expiry");
          } else {
            this.getStyleClass().removeAll("Expiry");
          }
        } else {
          this.setText(null);
        }
      }
    });

    // add row renderer to set colors
    tbBookmarks.setRowFactory((var UNUSED) -> new TableRow<>() {
      @Override
      protected void updateItem(BookmarkData data, boolean empty) {
        super.updateItem(data, empty);
        if (empty || data == null) {
          setBackground(Background.EMPTY);
        } else {
          setBackground(isSelected() ? BackgroundSelected : data.getSeen() ? BackgroundSeen : Background.EMPTY);
          // set foreground color:
          Color fillcolor = isSelected() ? Color.WHITE : data.isNotInFilmList() ? ColorExpired : data.isLiveStream() ? ColorLive : null;
          if (fillcolor != null) {
            this.getChildren().forEach((n) -> ((Labeled) n).setTextFill(fillcolor));
          }
        }
      }
    });

    // create filtered and sortable list
    filteredBookmarkList = new FilteredList<>(listeBookmarkList.getObervableList(), p -> true);
    SortedList<BookmarkData> slisteBookmarkList = new SortedList<>(filteredBookmarkList);
    slisteBookmarkList.comparatorProperty().bind(tbBookmarks.comparatorProperty());

    listeBookmarkList.getObervableList().addListener((ListChangeListener.Change<? extends BookmarkData> c) -> {
      while (c.next()) {
        if (c.wasAdded() || c.wasRemoved() || c.wasUpdated()) {
          listUpdated = true;
          break;
        }
      }
      tbBookmarks.refresh();
      JavaFxUtils.invokeInFxThreadAndWait(this::updateDisplay);
    });

    tbBookmarks.setItems(slisteBookmarkList);
    tbBookmarks.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);

    // Add listener to set button and context item state depending on selection
    tbBookmarks.getSelectionModel().selectedIndexProperty().addListener((obs, oldSelection, newSelection) -> {
       boolean disable = newSelection == null || newSelection.intValue() == -1;
       btnDeleteEntry.setDisable(disable);
       btnMarkViewed.setDisable(disable || onlyLifeStreamSelected());
       boolean multipleSelected = tbBookmarks.getSelectionModel().getSelectedItems().size() > 1;
       disable = disable || multipleSelected; // allow only for single selection
       btnEditNote.setDisable(disable);
       playitem.setDisable(disable);
       edititem.setDisable(disable);
       loaditem.setDisable(disable);
       viewitem.setDisable(onlyLifeStreamSelected());
       webitem.setDisable(disable || tbBookmarks.getSelectionModel().getSelectedItem().getWebUrl() == null);
       ccopyitem.setDisable(disable);

       // Update buttons: Check if not seen in selection and adapt button text
       boolean setViewed = isUnSeenSelected();
       setSeenButtonState(setViewed, multipleSelected);
       deleteitem.setText(String.format("Film%s aus der Merkliste entfernen",(multipleSelected ? "e" : "")));
       // change description
       updateDescriptionArea();
    });

    tbBookmarks.getSortOrder().addListener((ListChangeListener.Change<? extends TableColumn<BookmarkData,?>> pc) -> {
      tbBookmarks.getSelectionModel().clearSelection(); // clear selection after sort
    });

    FilterState = -1;
    btnFilterAction (null);
    btnShowDetails.setSelected(ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".details", true));
    divposition = ApplicationConfiguration.getConfiguration().getDouble(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".divider", spSplitPane.getDividerPositions()[0]);
    btnShowDetailsAction(null);
    updateDescriptionArea();

    setupColumnContextMenu();
  }

  private void setupColumnContextMenu() {
    tbBookmarks.setTableMenuButtonVisible(true);
    // setup column context menu
    new TableViewColumnContextMenuHelper(tbBookmarks) {
      @Override protected CustomMenuItem createColumnCustomMenuItem(
              final ContextMenu contextMenu, final TableColumn<?, ?> column) {
        final CheckBox checkBox;
        if (!column.getText().isEmpty())
          checkBox = new CheckBox(column.getText());
        else {
          checkBox = new CheckBox(" ");
          Node icon;
          switch (column.getId()) {
            case "colBtnPlay":
              icon = new IconNode(FontAwesome.PLAY);
              break;

            case "colBtnDownload":
              icon = new IconNode(FontAwesome.DOWNLOAD);
              break;

            default:
              throw new IllegalStateException("unknown id");
          }
          checkBox.setGraphic(icon);
        }
        // adds listener to the check box to change the size so the user
        // can click anywhere in the menu items area and not just on the
        // text to activate its onAction
        contextMenu.focusedProperty().addListener(
                event -> checkBox.setPrefWidth(contextMenu.getWidth() * 0.75));
        // the context menu item's state controls its bound column's visibility
        checkBox.selectedProperty().bindBidirectional(column.visibleProperty());

        final CustomMenuItem customMenuItem = new CustomMenuItem(checkBox);
        customMenuItem.getStyleClass().set(1, "check-menu-item");
        customMenuItem.setOnAction(event -> {
          checkBox.setSelected(!checkBox.isSelected());
          event.consume();
        });
        // set to false so the context menu stays visible after click
        customMenuItem.setHideOnClick(false);
        return customMenuItem;
      }
    };
  }

  private void updateDescriptionArea() {
    taDescription.setText(tbBookmarks.getSelectionModel().getSelectedItems().size() == 1 ? tbBookmarks.getSelectionModel().getSelectedItem().getExtendedDescription() : "");
    boolean showurl = false;
    if (tbBookmarks.getSelectionModel().getSelectedItems().size() == 1) {
      String url = tbBookmarks.getSelectionModel().getSelectedItem().getWebUrl();
      if (url != null && !url.isEmpty()) {
        hyperLink.setTooltip(new Tooltip(url));
        hyperLink.setVisited(false);
        showurl = true;
      }
    }
    hyperLink.setVisible(showurl);
  }

  private void setSeenButtonState(boolean setViewed, boolean multipleSelected) {
    btnMarkViewed.setGraphic(new IconNode(setViewed ? FontAwesome.EYE: FontAwesome.EYE_SLASH));
    String text = String.format("Film%s als %sgesehen markieren", (multipleSelected ? "e" : ""), (setViewed ? "" : "un"));
    btnMarkViewed.setTooltip(new Tooltip(text));
    viewitem.setText(text);
    viewitem.setGraphic(new IconNode(setViewed ? FontAwesome.EYE: FontAwesome.EYE_SLASH));
  }

 /**
  * Restore table state and context menues from stored settings
  */
  private void restoreTableStateAndContextMenu() {
    // create cell ContextMenu
    cellContextMenu = new ContextMenu();
    // - create items
    playitem = new MenuItem("Film abspielen");
    playitem.setOnAction((ActionEvent UNUSED) -> playAction(tbBookmarks.getSelectionModel().getSelectedItem()));
    playitem.setGraphic(new IconNode(FontAwesome.PLAY));

    loaditem = new MenuItem("Film aufzeichnen");
    loaditem.setOnAction((ActionEvent UNUSED) -> loadAction(tbBookmarks.getSelectionModel().getSelectedItem()));
    loaditem.setGraphic(new IconNode(FontAwesome.DOWNLOAD));

    viewitem = new MenuItem();
    viewitem.setOnAction(this::btnMarkEntryAsViewed);

    edititem = new MenuItem("Anmerkungen bearbeiten");
    edititem.setOnAction(this::btnEditNote);
    edititem.setGraphic(new IconNode(FontAwesome.PENCIL));

    deleteitem = new MenuItem();
    deleteitem.setOnAction(this::btnDeleteEntry);
    deleteitem.setGraphic(new IconNode(FontAwesome.MINUS));

    webitem = new MenuItem("Film Webseite öffnen");
    webitem.setOnAction(this::hyperLinkSelected);

    ccopyitem = new MenuItem("Zellinhalt in die Ablage kopieren");
    ccopyitem.setOnAction(this::copy2Clipboard);

    // - add menue items to Cell ContextMenu
    cellContextMenu.getItems().addAll(playitem, loaditem, viewitem, new SeparatorMenuItem(), edititem, deleteitem,
                                      new SeparatorMenuItem(), webitem, ccopyitem);

    // Restore column width, state and sequence
    Configuration config = ApplicationConfiguration.getConfiguration();
    try {
      config.lock(LockMode.READ);
      String colbase = ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".columns.";
      int entries = config.getInt(colbase + "no", 0);
      if (entries > 0) {
        List<TableColumn<BookmarkData, ?>> collist = new ArrayList<>(tbBookmarks.getColumns());
        tbBookmarks.getColumns().clear();
        for (int i = 1; i <= entries; i++) {
          String colref = colbase + "col" + i;
          String colid = config.getString(colref + ".id");
          int colsize = config.getInt(colref + ".size", 20);
          boolean colvisible = config.getBoolean(colref + ".visible", true);
          for (var column : collist) {
            if (column.getId().equals(colid)) {
              column.setPrefWidth(colsize);
              column.setVisible(colvisible);
              tbBookmarks.getColumns().add(column);
              break;
            }
          }
        }
      }
    }
    finally {
      config.unlock(LockMode.READ);
    }
  }

  @FXML
  private void btnShowDetailsAction(ActionEvent event) {
    double newposition;
    if (btnShowDetails.isSelected()) {
      newposition = divposition;
    }
    else {
      divposition = spSplitPane.getDividerPositions()[0];
      newposition = 1.0;
    }
    spSplitPane.setDividerPositions(newposition);
  }

  /**
   * Set the display filter:
   * Rotate: All bookmarks -> Unseen bookmarks -> Seen Bookmarks -+
   *              ^                                               |
   *              +-----------------------------------------------+
   */
  private static final String[] BTNFILTER_TOOLTIPTEXT = {"Nur ungesehene Filme anzeigen", "Nur gesehene Filme anzeigen", "Alle Filme anzeigen"};
  private static final String[] LBLFILTER_MESSAGETEXT = {"", "Ungesehene Filme", "Gesehene Filme"};
  private static final boolean[] LBLSEEN_DISABLE = {false, true, false};
  @FXML
  private void btnFilterAction(ActionEvent e) {
    if (++FilterState > 2) {
      FilterState = 0;
    }
    switch (FilterState) {
      case 0:
        filteredBookmarkList.setPredicate(f -> true);  // show all
        break;
      case 1:
        filteredBookmarkList.setPredicate(film -> { // show only unseen
          return !film.getSeen();
        });
        break;
      case 2:
        // show only seen
        filteredBookmarkList.setPredicate(BookmarkData::getSeen);
        break;
    }
    btnFilter.setTooltip(new Tooltip(BTNFILTER_TOOLTIPTEXT[FilterState]));
    lblFilter.setText(LBLFILTER_MESSAGETEXT[FilterState]);
    lblSeen.setDisable(LBLSEEN_DISABLE[FilterState]);
    refresh();
  }

  @FXML
  @SuppressWarnings("unchecked")
  private void tbviewOnContextRequested(ContextMenuEvent event) {
    if (!tbBookmarks.getSelectionModel().getSelectedItems().isEmpty()) { // Do not show row context menu if nothing is selected
      if (!ccopyitem.isDisable()) { // adapt copy content to column
        TablePosition<BookmarkData, String> pos = tbBookmarks.getSelectionModel().getSelectedCells().get(0);
        BookmarkData item = tbBookmarks.getItems().get(pos.getRow());
        String sdata = pos.getTableColumn() != null ? pos.getTableColumn().getCellObservableValue(item).getValue() : "";
        ccopyitem.setDisable(sdata == null || sdata.isBlank()); // Disable if cell is empty:
        ccopyitem.setText((pos.getTableColumn() != null ? pos.getTableColumn().getText(): "Text" ) +  " kopieren");
      }
      cellContextMenu.show(tbBookmarks, event.getScreenX(), event.getScreenY());
    }
  }

  @FXML
  private void tbviewMouseClick(MouseEvent e) {
    if (e.getButton() == PRIMARY) {
      if (cellContextMenu.isShowing())
        cellContextMenu.hide();

      if (e.getClickCount() > 1 && tbBookmarks.getSelectionModel().getSelectedItems().size() == 1) {
        btnEditNote(null);
      }
    }
  }

  /**
   * Display Window on screen
   * During first call a new window is created, for successive calls the existing window is reused
   */
  public void show() {
    JavaFxUtils.invokeInFxThreadAndWait(() -> {
      if (stage == null) {
        stage = new Stage();
        setStageSize(stage); // restore size
        setStageEvents();
        stage.setTitle("Merkliste verwalten");
        stage.getIcons().add(new Image("/mediathek/res/MediathekView.png"));
        try {
          FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("/mediathek/res/programm/fxml/bookmarkWindow.fxml"));
          fxmlLoader.setController(this);
          Scene scene = new Scene(fxmlLoader.load());
          stage.setScene(scene);
          scene.getStylesheets().add(getClass().getResource("/mediathek/res/css/bookmarkWindow.css").toExternalForm());
        }
        catch (IOException e) {
          LogManager.getLogger(BookmarkWindowController.class).error("{} Can't find/load the FXML description! Exception - {}",
                                                                      getClass(), e.toString());
          stage = null;
        }
      }
      else {
        tbBookmarks.getSelectionModel().clearSelection();
        if (stage.isIconified()) {
          stage.setIconified(false);
        }
        stage.toFront();
        stage.requestFocus();
      }

      if (stage != null) {
        stage.show();
        refresh();
      }
    });
  }

  /**
   * Store reference used to inform about changes
   */
  public void setPartner(GuiFilme partner) { this.infotab = partner;}

  private void refresh() {
    if (stage.isShowing()) {
      tbBookmarks.refresh();
      updateDisplay();
    }
  }

  private void updateDisplay() {
    lblCount.setText(String.format("Einträge: %d / %d", filteredBookmarkList.size(), listeBookmarkList.getNbOfEntries()));
    lblSeen.setText(String.format("Gesehen: %d", listeBookmarkList.getSeenNbOfEntries()));
    btnSaveList.setDisable(!listUpdated);
    if (listUpdated) { // Schedule new save task after 30 s
      cancelBookmarkSave();
      SaveBookmarkTask = Daten.getInstance().getTimerPool().schedule(() -> {
                                        saveBookMarkList();
                                        SaveBookmarkTask = null;
                                    },
                                    30,
                                    TimeUnit.SECONDS);
    }
    lblMessage.setText("");
  }

  /**
   * Save Bookmark list to backup storage
   */
  private void saveBookMarkList() {
    if (listUpdated) {
      listeBookmarkList.saveToFile(Daten.getBookmarkFilePath());
      btnSaveList.setDisable(true);
      JavaFxUtils.invokeInFxThreadAndWait(() -> lblMessage.setText("Merkliste ist gesichert"));
    }
    listUpdated = false;
  }

  private static final String ALERT_TITLE = "Merkliste";

  private void playAction(BookmarkData data) {
    Daten.getInstance().starterClass.urlMitProgrammStarten(Daten.listePset.getPsetAbspielen(), data.getDataAsDatenFilm(), "");
    tbBookmarks.getSelectionModel().clearSelection(); // re-select to trigger UI update
    tbBookmarks.getSelectionModel().select(data);
  }

  /**
   * Trigger Download of movie   (mirror fucntionality of FilmGUI)
   * @param data movie object to be used for download
   *
   * Note: Due to mixture of JavaFX and Swing the windows do not arrange properly,
   *       workaround is to hide bookmark window during processing
   */
  private void loadAction(@NotNull BookmarkData data) {
    Optional<DatenFilm> datenFilm = Optional.ofNullable(data.getDatenFilm());
    final var daten = Daten.getInstance();

    refresh();
    datenFilm.ifPresent(film -> {
      DatenDownload previouslyCreatedDownload = daten.getListeDownloads().getDownloadUrlFilm(film.getUrl());
      if (previouslyCreatedDownload == null) {
        createDownload(film);
      }
      else
      {
        ButtonType yes = new ButtonType("Ja", ButtonBar.ButtonData.OK_DONE);
        ButtonType no = new ButtonType("Nein", ButtonBar.ButtonData.CANCEL_CLOSE);
        Alert alert = new Alert(Alert.AlertType.WARNING,
                "Ein Download für den Film existiert bereits.\nNochmal anlegen?",
                yes, no);
        alert.initOwner(stage);
        alert.setTitle(ALERT_TITLE);
        alert.showAndWait().filter(response -> response == ButtonType.OK)
                .ifPresent(response -> createDownload(film));
      }
    });
  }

  private void createDownload(DatenFilm film) {
    final var daten = Daten.getInstance();

    stage.hide();

    SwingUtilities.invokeLater(() -> { // swing dialogs must be called from EDT!!
      DatenPset pSet = Daten.listePset.getListeSpeichern().getFirst();
      final var ui = MediathekGui.ui();
      DialogAddMoreDownload damd = new DialogAddMoreDownload(ui, pSet);
      damd.setLocationRelativeTo(ui);
      damd.setVisible(true);

      String pfad = damd.getPath();
      boolean info = damd.info;
      boolean subtitle = damd.subtitle;
      if (!damd.cancel) {
        DatenDownload datenDownload = new DatenDownload(pSet, film, DatenDownload.QUELLE_DOWNLOAD, null, "", pfad, "");
        datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI] = Boolean.toString(info);
        datenDownload.arr[DatenDownload.DOWNLOAD_SUBTITLE] = Boolean.toString(subtitle);

        daten.getListeDownloads().addMitNummer(datenDownload);
        daten.getMessageBus().publishAsync(new DownloadListChangedEvent());
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD_D_STARTEN))) {
          datenDownload.startDownload();  // und evtl. auch gleich starten
        }
      }
      showStage();
    });
  }

  private void showStage() {
    //this may be called from swing EDT!
    Platform.runLater(() -> {
      stage.show();
      stage.toFront();
      stage.requestFocus();
    });
  }

  /**
   * Save Window settings to application configuration
   */
  public void saveSettings() {
    Configuration config = ApplicationConfiguration.getConfiguration();
    // - Window size and position:
    try {
      config.lock(LockMode.WRITE);
      config.setProperty(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".width", (int)stage.getWidth());
      config.setProperty(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".heigth", (int)stage.getHeight());
      config.setProperty(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".location.x", (int)stage.getX());
      config.setProperty(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".location.y", (int)stage.getY());
      // - Column state, width and order:
      String colbase = ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".columns";
      config.setProperty(colbase + ".no", tbBookmarks.getColumns().size());
      int k = 0;
      for (var column: tbBookmarks.getColumns()) {
        k++;
        String colref = colbase + ".col" + k;
        config.setProperty(colref + ".id", column.getId());
        config.setProperty(colref + ".size", (int)column.getWidth());
        config.setProperty(colref + ".visible", column.isVisible());
      }
      // - Button States:
      config.setProperty(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".details", btnShowDetails.isSelected());
      config.setProperty(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".divider", btnShowDetails.isSelected()
        ? spSplitPane.getDividerPositions()[0] : divposition);
    }
    catch(Exception e) {
      LogManager.getLogger(ApplicationConfiguration.class).error("Save Config exception: ", e);
    }
    finally {
      config.unlock(LockMode.WRITE);
    }
  }

  private static void setStageSize(Stage window) {
    Configuration config = ApplicationConfiguration.getConfiguration();
    try {
      config.lock(LockMode.READ);
      window.setWidth(config.getInt(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".width", 640));
      window.setHeight(config.getInt(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".heigth", 480));
      window.setX(config.getInt(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".location.x", 0));
      window.setY(config.getInt(ApplicationConfiguration.APPLICATION_UI_BOOKMARKLIST + ".location.y", 0));
    }
    finally {
      config.unlock(LockMode.READ);
    }
  }

  private void setStageEvents() {
    stage.setOnShowing(e -> {
      initSettings(); // re-read config values on showing
    });
    stage.setOnHiding(e -> {
      if (listUpdated) { // Save pending changes on hiding
        cancelBookmarkSave();
        saveBookMarkList();
        listUpdated = false;
      }
    });
  }

  /**
   * Returns true if the current table selection contains unseen items
   * @return boolean
   *
   * Note: Lifestreams are ignored
   */
  private boolean isUnSeenSelected() {
    boolean unSeen = false;
    for (BookmarkData data: tbBookmarks.getSelectionModel().getSelectedItems()) {
      if (!data.getSeen() && !data.isLiveStream()) {
        unSeen = true;
        break;
      }
    }
    return unSeen;
  }

  /**
   * Returns true if the current table selection contains only livestreams
   * @return boolean
   */
  private boolean onlyLifeStreamSelected() {
    boolean lifestream = true;
    for (BookmarkData data: tbBookmarks.getSelectionModel().getSelectedItems()) {
      if (!data.isLiveStream()) {
        lifestream = false;
        break;
      }
    }
    return lifestream;
  }

  private static Color convertMVCAWTColor(MVC mvc) {
    return Color.rgb(mvc.color.getRed(),mvc.color.getGreen(), mvc.color.getBlue());
  }

  private void initSettings() {
    Color colorSeen = convertMVCAWTColor(FILM_HISTORY);
    Color colorNew = convertMVCAWTColor(FILM_NEU);
    ColorExpired = convertMVCAWTColor(DOWNLOAD_FEHLER);
    ColorLive = convertMVCAWTColor(FILM_LIVESTREAM);
    BackgroundSeen = new Background(new BackgroundFill(colorSeen, CornerRadii.EMPTY, Insets.EMPTY));
    BackgroundSelected = new Background(new BackgroundFill(colorNew, CornerRadii.EMPTY, Insets.EMPTY));
  }

  /**
   * Cancel a waiting bookmark save task
   */
  private void cancelBookmarkSave() {
    if (SaveBookmarkTask != null) {
      SaveBookmarkTask.cancel(false);
      SaveBookmarkTask = null;
    }
  }
}