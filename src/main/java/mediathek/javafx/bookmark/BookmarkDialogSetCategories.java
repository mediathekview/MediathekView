package mediathek.javafx.bookmark;

import java.util.ArrayList;
import javafx.beans.value.ObservableValue;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.paint.Color;
import mediathek.config.Daten;
import mediathek.tool.javafx.FXDialogTemplate;


/**
 * Dialog to administrate categories
 * Returns true if category was deleted
 *
 * @author Klaus Wich <klaus.wich@aim.com>
 */

public class BookmarkDialogSetCategories extends FXDialogTemplate {

  // defined by template:
  //  protected Button fxCancelButton;
  //  protected Button fxSaveButton;
  //  protected Label fxStatus;
  //  protected boolean result;

  @FXML
  private ColorPicker fxColorPick;
  @FXML
  private ColorPicker fxColorPickBackground;
  @FXML
  private ListView<BookmarkCategory> fxCategoryList;
  @FXML
  protected Button fxBtnMoveUp;
  @FXML
  protected Button fxBtnMoveDown;
  @FXML
  protected Button fxBtnAdd;
  @FXML
  protected Button fxBtnDelete;
  @FXML
  private TextField fxCategoryName;
  @FXML
  private TextArea fxMessage;

  private BookmarkCategoryList listeBookmarkCategoryList;
  private boolean ignorechange;
  private ArrayList<String> deletedCategoryNames;
  private ArrayList<String> aboCategoryNames;

  @Override
  public void initialize() {
    listeBookmarkCategoryList = Daten.getInstance().getListeBookmarkCategoryList();
    fxCategoryList.setCellFactory((ListView<BookmarkCategory> param) -> {
        return new BookmarkCategoryListCell(true);
    });
    fxCategoryList.setItems(listeBookmarkCategoryList.getObervableList());
    fxColorPick.setValue(Color.BLACK);
    fxCategoryList.getSelectionModel().selectedItemProperty().addListener((ObservableValue<? extends BookmarkCategory> ov, BookmarkCategory t, BookmarkCategory s) -> {
      if (!ignorechange) {
        BookmarkCategory category = fxCategoryList.getSelectionModel().getSelectedItem();
        fxCategoryName.setText(category != null ? category.getName() : "");
        fxColorPick.setValue(category != null ? category.getColor() : Color.BLACK);
        fxColorPickBackground.setValue(category != null ? category.getBackgroundColor() : Color.WHITE);
        fxBtnAdd.setText(category != null ? "Ändern" : "Hinzufügen");   
        fxMessage.setVisible(category != null && aboCategoryNames.contains(category.getName())); 
        update(null);
      }
      else {
        ignorechange = false;
      }
    });
    deletedCategoryNames = null;
    aboCategoryNames = Daten.getInstance().getListeAbo().getBookmarkAboCategories();
  }
  
  @Override
  public ArrayList<String> getResult() {
    return deletedCategoryNames;
  }
  
  @FXML
  private void handleNameKey(Event e) {
    BookmarkCategory category = listeBookmarkCategoryList.getItemByName(fxCategoryName.getText());
    ignorechange = true;
    if (category == null) {
      fxCategoryList.getSelectionModel().clearSelection();
      fxBtnAdd.setText("Hinzufügen");
    }
    else {
      fxCategoryList.getSelectionModel().select(category);
      fxBtnAdd.setText("Ändern");
    }
    update(null);
  }
  
  @FXML
  private void btnMoveUp(Event e) { 
    move(true);  
  }
  
  @FXML
  private void btnMoveDown(Event e) {
    move(false);
  }
  
  private void move(boolean up) {
    ignorechange = true;
    int idx = fxCategoryList.getSelectionModel().getSelectedIndex();
    BookmarkCategory category = listeBookmarkCategoryList.getObervableList().get(idx);
    listeBookmarkCategoryList.getObervableList().remove(idx);
    idx = idx + (up ? -1 : 1);
    listeBookmarkCategoryList.getObervableList().add(idx, category);
    fxCategoryList.getSelectionModel().select(idx);
    update(null);
  }
    
  @FXML
  private void btnAdd(Event e) {
    BookmarkCategory category = listeBookmarkCategoryList.getItemByName(fxCategoryName.getText());
    if (category != null) {
      category.setColor(fxColorPick.getValue());
      category.setBackgroundColor(fxColorPickBackground.getValue());
    }
    else {
      category = new BookmarkCategory(fxCategoryName.getText(), fxColorPick.getValue(), fxColorPickBackground.getValue());
      listeBookmarkCategoryList.getObervableList().add(category);
      // Remove from deleted if any:
      if (deletedCategoryNames != null) {
        for (String cn: deletedCategoryNames) {
          if (cn.equals(category.getName())) {
            deletedCategoryNames.remove(cn);
            break;
          }
        }
        result = deletedCategoryNames.size() > 0;
      }   
    }
    fxCategoryList.getSelectionModel().clearSelection();
    update(null);
  }
  
  @FXML
  private void btnDelete(Event e) {
    BookmarkCategory category = fxCategoryList.getSelectionModel().getSelectedItem();
    // Check if in use:
    if (!Daten.getInstance().getListeAbo().hasBookmarkAboWithCategory(category.getName())) {    
      if (deletedCategoryNames == null) {
        deletedCategoryNames = new ArrayList<>();
      }
      deletedCategoryNames.add(category.getName());  // Store deleted name for later removal
      fxCategoryList.getSelectionModel().clearSelection();
      listeBookmarkCategoryList.getObervableList().remove(category);
    }
    result = true;
  }
  
  @FXML
  private void update(Event e) {
    fxCategoryName.setStyle(String.format("-fx-text-fill: #%s;", fxColorPick.getValue().toString().substring(2, 8)));
    fxCategoryName.setBackground(new Background(new BackgroundFill(fxColorPickBackground.getValue(),null,null)));
    boolean disable = fxCategoryList.getSelectionModel().getSelectedIndex() < 0;
    fxBtnMoveUp.setDisable(fxCategoryList.getSelectionModel().getSelectedIndex() < 1);
    fxBtnMoveDown.setDisable(disable || fxCategoryList.getSelectionModel().getSelectedIndex() == fxCategoryList.getItems().size()-1);
    fxBtnDelete.setDisable(fxMessage.isVisible());
    fxBtnAdd.setDisable(fxCategoryName.getText().length() < 3);
  }
    
}
