package mediathek.tool.javafx;

import java.io.IOException;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.apache.logging.log4j.LogManager;


/**
 * Control wrapper to simplify the creation and call of FXML based dialogs
 * @author Klaus Wich <klaus.wich@aim.com>
 *
 * Dialog has to be based on FXDialTemplate class
 *
 * Dialog gets the MediathekView.png as icon
 *
 * Usage:
 *   // create, call and execute dialog with
 *   result boolean = new FXDialogControl(..).SetAndShow(Object o);
 *
 * Or:
 *  // create:
 *  FXDialogControl ctrl = new FXDialogControl(..)
 *  // set css:
 *  ctrl.addStylesheet(..)
 *  // execute
 *  result boolean = ctrl.SetAndShow(Object o);
 *  // get details back:
 *  Object o = ctrl.getResult()
 *
 *   Object o : Dialog specific data (packed into one object), e.g. record to be displayed
 *
 */
public class FXDialogControl
{
  Stage dlgstage;
  FXMLLoader loader;

  // creates a modal dialog
  public FXDialogControl(String resourcename, String title) {
    this(resourcename, title, Modality.APPLICATION_MODAL);
  }

  public FXDialogControl(String resourcename, String title, Modality modal) {
    try {
      dlgstage = new Stage();
      loader = new FXMLLoader(getClass().getResource(resourcename));
      dlgstage.initModality(modal);
      if (title != null) {
        dlgstage.setTitle(title);
      }
      Scene scene = new Scene(loader.load());
      dlgstage.setScene(scene);
      dlgstage.getIcons().add(new Image("/mediathek/res/MediathekView.png"));
    }
    catch (IOException ex) {
      LogManager.getLogger(FXDialogControl.class).error("{} Can't find/load the FXML description! Exception - {}",
                                                                  getClass(), ex.toString());
      loader = null;
    }
  }

  public boolean SetAndShow(Object o) {
    return ((FXDialogTemplate) loader.getController()).SetandShow(dlgstage, o);
  }
  
  public Object getResult() {
    return ((FXDialogTemplate) loader.getController()).getResult();
  }

  public void addStylesheet(String pathToStyleSheet) {
    dlgstage.getScene().getStylesheets().add(getClass().getResource(pathToStyleSheet).toExternalForm());
  }

  public <T> T getController() {
    return loader.getController();
  }

}
