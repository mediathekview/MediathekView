package mediathek.javafx.bookmark;

import javafx.concurrent.Task;
import javafx.fxml.FXML;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.ProgressIndicator;
import mediathek.config.Daten;
import mediathek.tool.javafx.FXDialogTemplate;

/**
 * Starts a background task and displays an progress dialog to update the expiry date for 
 * bookmark entries.
 * 
 * @author Klaus Wich <klaus.wich@aim.com>
 */
public class BookmarkDialogSetExpiry extends FXDialogTemplate {

  // defined by template:
  //  protected Button fxCancelButton;
  //  protected Button fxSaveButton;
  //  protected Label fxStatus; 

  @FXML
  private ProgressIndicator fxProgress;
  @FXML
  private ProgressBar fxProgressBar;

  private UpDateExpiryTask task;
  private Thread taskThread;
  private BookmarkWindowController parent;
  private static final String NOEXPIRY = "Kein Ablaufdatum!";
  private static final String ENDMSG = "Fertig";
  private static final String CANCELMSG = "Fertig - Abgebrochen";
  
  protected class UpDateExpiryTask extends Task<Void> {

    @Override
    protected Void call() throws Exception {
      BookmarkDataList bdata = Daten.getInstance().getListeBookmarkList();
      int count = bdata.getNbOfEntries();
      int pos = 0;
      updateProgress(pos, count);
      for (BookmarkData bookmark: bdata.getObervableList()) {
        try {
          if (isCancelled()) {
            break;
          }
          updateMessage(bookmark.getTitel());
          if (bookmark.getExpiry() == null || bookmark.getExpiry().isEmpty()) {
            String expiry = BookmarkDataList.searchExpiryDate(bookmark);
            if (expiry != null && !expiry.isBlank()) {
              bookmark.setExpiry(expiry);
            }
            else {
              String note = bookmark.getNote();
              if (note == null || note.length() == 0) {
                bookmark.setNote(NOEXPIRY);
              } 
              else {
                if (!note.contains(NOEXPIRY)) {
                  bookmark.setNote(NOEXPIRY + " " + note);
                }        
              }
            }
          }
        }
        catch (Exception e) {
          updateMessage("Exception " +  e.getMessage());
        }       
        updateProgress(++pos, count);
      }
      updateMessage(isCancelled() ? CANCELMSG : ENDMSG);
      return null;
    }   
  } 
  
  @Override
  public void initialize() {
    task = new UpDateExpiryTask();
    fxStatus.textProperty().bind(task.messageProperty());
    fxStatus.textProperty().addListener((obs, oldText, newText) -> {
      if (newText.startsWith(ENDMSG)) {
        fxCancelButton.setDisable(true);
      }
      parent.refresh();
    });
    fxProgressBar.progressProperty().bind(task.progressProperty()); //task.workDoneProperty());
    fxProgress.progressProperty().bind(task.progressProperty());
  }
  
  @Override
  protected void handleCancel() {
    result = false;
    task.cancel();
    fxCancelButton.setDisable(true);
  }
  
  @Override
  protected void Setup(Object o) {
    parent = (BookmarkWindowController) o; 
    taskThread = new Thread(task, "update-expiry");
    taskThread.setDaemon(true);
    fxProgressBar.setDisable(false);
    fxProgress.setDisable(false);
    fxCancelButton.setDisable(false);
    taskThread.start();
  }  
}
