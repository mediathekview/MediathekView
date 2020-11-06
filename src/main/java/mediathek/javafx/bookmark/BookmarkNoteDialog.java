package mediathek.javafx.bookmark;

import javafx.concurrent.Task;
import javafx.concurrent.WorkerStateEvent;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.tool.MVHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.net.URISyntaxException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javafx.util.StringConverter;
import mediathek.tool.javafx.FXDialogTemplate;


/**
 * Dialog to set expiry date and notes for bookmarked movies
 *
 * includes search for movies's expiry date on webpage
 *
 * Returns true if data was changed
 *
 * @author Klaus Wich <klaus.wich@aim.com>
 */

public class BookmarkNoteDialog extends FXDialogTemplate {

  // defined by template:
  //  protected Button fxCancelButton;
  //  protected Button fxSaveButton;
  //  protected Label fxStatus;

  @FXML
  private DatePicker fxDate;
  @FXML
  private TextArea fxNote;
  @FXML
  protected Button btnWebDate;
  @FXML
  protected Button btnWebLink;
  @FXML
  private ProgressIndicator fxProgress;
  @FXML
  private Label fxResult;
  @FXML
  private Label fxExpiry;
  @FXML
  private ComboBox<BookmarkCategory> cbKategory;

  private BookmarkData data;
  private DateTimeFormatter dateformatter;
  private boolean hasWebURL;

  @Override
  public void initialize() {
    dateformatter = DateTimeFormatter.ofPattern("dd.MM.yyyy");
    fxDate.setOnKeyTyped((var e) -> handleChange());
    fxDate.setOnMouseClicked((var e) -> handleChange());
    fxDate.getEditor().setOnKeyTyped((var e) -> handleChange());
    cbKategory.getItems().add(new BookmarkCategory(BookmarkCategoryList.NOCATEGORY));
    cbKategory.getItems().addAll(BookmarkCategoryList.getInstance().getObervableList());
    cbKategory.setCellFactory((ListView<BookmarkCategory> param) -> {
        return new BookmarkCategoryListCell(true);
    });
    cbKategory.setButtonCell(new BookmarkCategoryListCell(false));
    cbKategory.setConverter(new StringConverter<BookmarkCategory>() {
      @Override
      public String toString(BookmarkCategory bc) {
        return bc != null ? bc.getName() : null;
      }

      @Override
      public BookmarkCategory fromString(String string) { // not needed
        return null;
      }
    });
  }

  @Override
  protected void handleSave() {
    if (!fxNote.getText().equals(data.getNote())) {
      data.setNote(fxNote.getText());
      result = true;
    }
    String dv = getDateValue();
    if (!(dv == null && data.getExpiry() == null) || (dv != null && !dv.equals(data.getExpiry()))) {
      data.setExpiry(dv);
      result = true;
    }
    int idx = cbKategory.getSelectionModel().getSelectedIndex();
    if (idx > -1 ) {
      String newCategory = idx > 0 ? cbKategory.getValue().getName() : null;
      String oldCategory = data.getCategory();
      if ((oldCategory == null && newCategory != null) || (oldCategory != null && newCategory == null) || !newCategory.equals(oldCategory)) {
        result = true;
        data.setCategory(newCategory);
      }
    }
    _dlgstage.hide();
  }

  @Override
  protected void handleChange() {
    boolean isok = Verify();
    fxSaveButton.setDisable(!isok);
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
        return BookmarkDataList.searchExpiryDate(data);
      }
    };
    task.setOnSucceeded((WorkerStateEvent t) -> {
      String value = task.getValue();
      if (value != null && !value.isEmpty()) {
        fxResult.setText("\"Verfügbar bis Datum\" -" + value + "- gefunden");
        fxDate.getEditor().setText(value);
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

  @Override
  protected void Setup(Object o) {
    this.data = (BookmarkData) o;
    this.hasWebURL = data.hasWebURL();
    this._dlgstage.setTitle(data.getNote() != null || data.getExpiry() != null ? "Anmerkungen ändern" : "neue Anmerkungen");
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
    String category = data.getCategory() != null ? data.getCategory() : BookmarkCategoryList.NOCATEGORY;
    for (BookmarkCategory bcat : cbKategory.getItems()) {
      if (bcat.getName().equals(category)) {
        cbKategory.getSelectionModel().select(bcat);
        break;
      }
    }
    handleChange();
  }

  @Override
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
