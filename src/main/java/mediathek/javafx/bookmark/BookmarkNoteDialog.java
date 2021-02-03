package mediathek.javafx.bookmark;

import javafx.concurrent.Task;
import javafx.concurrent.WorkerStateEvent;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import javafx.stage.Stage;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.tool.http.MVHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ResourceBundle;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * Dialog to set expiry date and notes for bookmarked movies
 *
 * includes search for movies's expiry date on webpage
 *
 * @author Klaus Wich <klaus.wich@aim.com>
 */

public class BookmarkNoteDialog implements Initializable
{
  @FXML
  private DatePicker fxDate;
  @FXML
  private TextArea fxNote;
  @FXML
  protected Button SaveButton;
  @FXML
  protected Button CancelButton;
  @FXML
  protected Button btnWebDate;
  @FXML
  protected Button btnWebLink;
  @FXML
  private ProgressIndicator fxProgress;
  @FXML
  private Label fxStatus; 
  @FXML
  private Label fxResult; 
  @FXML
  private Label fxExpiry;

  protected Stage dlgstage;
  protected boolean datachanged;
  private BookmarkData data;
  private DateTimeFormatter dateformatter;
  private boolean hasWebURL;

  @Override
  public void initialize(URL arg0, ResourceBundle arg1) {  
    dateformatter = DateTimeFormatter.ofPattern("dd.MM.yyyy");
    fxDate.setOnKeyTyped((var e) -> handleChange());
    fxDate.setOnMouseClicked((var e) -> handleChange());
    fxDate.getEditor().setOnKeyTyped((var e) -> handleChange());
  }
  
  @FXML
  protected void handleCancel() {
    datachanged = false;
    dlgstage.hide();
  }

  @FXML
  protected void handleSave() {
    if (!fxNote.getText().equals(data.getNote())) {
      data.setNote(fxNote.getText());   
      datachanged = true;
    }

    String dv = getDateValue();
    if (!(dv == null && data.getExpiry() == null) || (dv != null && !dv.equals(data.getExpiry()))) {
      data.setExpiry(dv);
      datachanged = true;
    }
    dlgstage.hide();
  }

  @FXML
  protected void handleChange() {
    boolean isok = Verify();
    SaveButton.setDisable(!isok);
    int idx = fxDate.getEditor().getStyleClass().indexOf("Invalid");
    if (isok && idx > -1) {
      fxDate.getEditor().getStyleClass().remove("Invalid");
    }
    else if (!isok && idx == -1) {
      fxDate.getEditor().getStyleClass().add("Invalid");
    }
  }
  
  /**
   * Search the expiry date on Webpage
   * @param e 
   * 
   * Starts own background task
   */
  @FXML
  private void btnSearchWeb(Event e) {
    fxProgress.setVisible(true);
    fxStatus.setVisible(true);
    btnWebDate.setDisable(true);
    Task<String> task = new Task<>() {
      @Override
      protected String call() {
        return searchExpiryDate();
      }
    };
    task.setOnSucceeded((WorkerStateEvent t) -> {
      String result = task.getValue();
      if (result != null) {
        fxResult.setText("\"Verfügbar bis Datum\" -" + result + "- gefunden");
        fxDate.getEditor().setText(result);
      }
      else {
        fxResult.setText("\"Verfügbar bis\" Datum nicht gefunden");
      }
      fxProgress.setVisible(false);
      fxStatus.setVisible(false);
      btnWebDate.setDisable(false);
    });
    new Thread(task).start();
  }
  
  /**
   * Opens the linked webpage Webpage
   * @param e (unused)
   */
  @FXML
  private void btnOpenWebLink(Event e) {
    try {
      if (hasWebURL) {
        UrlHyperlinkAction.openURL(null,data.getWebUrl());
      }
    }
    catch (URISyntaxException ignored) {}
  }
  
  
  public final boolean SetandShow(Stage dlgstage, BookmarkData data) {
    this.dlgstage = dlgstage;
    this.data = data;
    this.hasWebURL = data.hasWebURL();
    this.dlgstage.setTitle(data.getNote() != null || data.getExpiry() != null ? "Anmerkungen ändern" : "neue Anmerkungen");
    fxNote.setText(data.getNote() != null ? data.getNote() :  "");
    if (data.isLiveStream()) { // For live stream disable expiry handling
      fxExpiry.setDisable(true);
      fxDate.setDisable(true);
      btnWebDate.setDisable(true);
    }
    else {
      if (data.getExpiry() != null && !data.getExpiry().isEmpty()) { // copy expiry from record
        try {
          fxDate.setValue(LocalDate.parse(data.getExpiry(),dateformatter));
        }
        catch (Exception ignored) {}
      }
      btnWebDate.setDisable(!hasWebURL); 
      btnWebLink.setDisable(!hasWebURL);
    }   
    handleChange();
    // Display the Dialog and wait
    this.dlgstage.showAndWait();
    return datachanged;
  }
  
  protected boolean Verify() {
    boolean rc = true;
    // Check date format:
    String dv = getDateValue();
    if (dv != null) {
      try {
        LocalDate.parse(dv, dateformatter);
      }
      catch (Exception e) {
        rc = false;
      }
    }
    return rc;
  }

  /**
   * Get date value or null
   * @return String
   */
  private String getDateValue() {
    String dv = fxDate.getEditor().getText();
    if (dv != null && dv.isEmpty()) {
      dv = null;
    }
    return dv;
  }
  
  /**
   * Try to retrieve the expiry date from the associated webpage
   */
  private static final Pattern[] DATE_PATTERNS = {null, null};
  private static final String[] DATE_PATTERN_STRINGS = {"verfügbar.+?bis.+?([0-9]{2}\\.[0-9]{2}\\.[0-9]{4})", "verfügbar.+?bis.+?([0-9]{2}/[0-9]{2}/[0-9]{4})"};
  private static final int EXCERPT_LEN = 1000;

    private String searchExpiryDate() {
        String result = null;
        if (hasWebURL) {
            final Request request = new Request.Builder().url(data.getWebUrl()).get().build();
            try (Response response = MVHttpClient.getInstance().getReducedTimeOutClient().newCall(request).execute();
                 ResponseBody body = response.body()) {
                if (response.isSuccessful() && body != null) {
                    try (var is = body.byteStream();
                         var isr = new InputStreamReader(is, StandardCharsets.UTF_8);
                         BufferedReader in = new BufferedReader(isr)) {
                        StringBuilder a = new StringBuilder();
                        String str;
                        boolean save = false;
                        // 1.) get EXCERPT_LEN characters beginning with the search term
                        while ((str = in.readLine()) != null) {
                            if (!save) {
                                int idx = str.toLowerCase().indexOf("verfügbar ");
                                if (idx > -1) {
                                    String sdate = str.substring(idx, str.length() - 1); // < idx+EXCERPT_LEN ? str.length() : idx+EXCERPT_LEN);
                                    a.append(sdate);
                                    save = true;
                                }
                            } else {
                                if (a.length() < EXCERPT_LEN) {
                                    a.append(str.toLowerCase());
                                } else {
                                    break;
                                }
                            }
                        }

                        if (a.length() > 0) {
                            // 2.) use regex to extract date
                            for (int k = 0; k < DATE_PATTERNS.length; k++) {
                                if (DATE_PATTERNS[k] == null) {   // compile pattern only once!
                                    DATE_PATTERNS[k] = Pattern.compile(DATE_PATTERN_STRINGS[k], Pattern.CASE_INSENSITIVE);
                                }
                                Matcher matcher = DATE_PATTERNS[k].matcher(a);
                                if (matcher.find()) {
                                    result = matcher.group(1).replaceAll("/", "\\.");
                                    break;
                                }
                            }
                        }
                    }
                }
            } catch (IOException ignored) {
            }
        }

        return result;
    }
}
