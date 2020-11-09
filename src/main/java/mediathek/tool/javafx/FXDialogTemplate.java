package mediathek.tool.javafx;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.stage.Stage;

/**
 * FXML Dialog Template
 * 
 * Base for Dialogs with Cancel and Save buttons and a framework
 * to control these.
 * Save button is only enbaled if Verify returns true.
 *
 * @author Klaus Wich <klaus.wich@aim.com>
 */
public abstract class FXDialogTemplate 
{

  @FXML
  protected Button fxSaveButton;
  @FXML
  protected Button fxCancelButton;
  @FXML
  protected Label fxStatus;

  protected Stage _dlgstage;
  protected boolean result;

  @FXML
  protected void handleCancel() {
    result = false;
    _dlgstage.hide();
  }

  @FXML
  protected void handleSave() {
    result = Verify();
    if (result) {
      if (Save()) {
        _dlgstage.hide();
      }
    }
  }

  @FXML
  protected void handleChange() {
    if (fxSaveButton != null) {
      fxSaveButton.setDisable(!Verify());
    }
  }

  /**
   * Initializes the controller class (to be overwritten)
   */
  @FXML
  public void initialize() {
    System.out.println("DialogTemplate initialize - overwrite");
  }

  /**
   * Setup the dialog parameters and display the dialog
   * @param dlgstage: Stage, provided by FXDialogControl
   * @param o       : Parameter object, forwarded to setup
   * @return boolean: Result of dialog, for details use getResult()
   */ 
  public final boolean SetandShow(Stage dlgstage, Object o) {
    this._dlgstage = dlgstage;
    Setup(o);                 // Forward parameter object to implementation
    _dlgstage.showAndWait();  // Display the Dialog and wait for execution
    return result;
  }
  
  /**
   * Can be overwritten to return result details
   *
   * @return result Object with results
   */
  protected Object getResult() {
    return result;
  }

  /**
   * Optional: Define additional setup here
     * @param o: additional parameters
   */
  protected void Setup(Object o) {}

  /**
   * Template for verification
   * Should be overwritten
   * @return boolean: True if content is complete/ok
   */
  protected boolean Verify() {
    setStatus(true, "");
    return true;
  }

  /**
   * Should be overwritten
   *
   * @return
   */
  protected boolean Save() {
    return true;
  }
  
  protected void setStatus(boolean ok, String s) {
    if (fxStatus != null) {
      fxStatus.setText(ok ? "OK" : s);
      String style = ok ? "red" : "green";
      fxStatus.getStyleClass().remove(style);
      style = ok ? "green" : "red";
      if (!fxStatus.getStyleClass().contains(style)) {
        fxStatus.getStyleClass().add(style);
      }
    }
  }

}
