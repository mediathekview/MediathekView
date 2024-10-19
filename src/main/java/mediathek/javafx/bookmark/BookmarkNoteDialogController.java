package mediathek.javafx.bookmark;

import javafx.concurrent.WorkerStateEvent;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import javafx.stage.Stage;
import mediathek.config.Konstanten;

import java.net.URL;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ResourceBundle;


/**
 * Dialog to set expiry date and notes for bookmarked movies
 * <p>
 * includes search for movies's expiry date on webpage
 *
 * @author Klaus Wich <klaus.wich@aim.com>
 */

public class BookmarkNoteDialogController implements Initializable {
    /**
     * Try to retrieve the expiry date from the associated webpage
     */
    private final DateTimeFormatter dateformatter;
    @FXML
    protected Button SaveButton;
    @FXML
    protected Button CancelButton;
    @FXML
    protected Button btnWebDate;
    protected Stage dlgstage;
    protected boolean datachanged;
    @FXML
    private DatePicker fxDate;
    @FXML
    private TextArea fxNote;
    @FXML
    private ProgressIndicator fxProgress;
    @FXML
    private Label fxStatus;
    @FXML
    private Label fxExpiry;
    private BookmarkData data;
    private boolean hasWebURL;

    public BookmarkNoteDialogController() {
        dateformatter = DateTimeFormatter.ofPattern("dd.MM.yyyy");
    }

    @Override
    public void initialize(URL arg0, ResourceBundle arg1) {
        fxDate.setOnKeyTyped((var e) -> handleChange());
        fxDate.setOnMouseClicked((var e) -> handleChange());
        fxDate.getEditor().setOnKeyTyped((var e) -> handleChange());

        ButtonBar.setButtonData(CancelButton, ButtonBar.ButtonData.CANCEL_CLOSE);
        ButtonBar.setButtonData(SaveButton, ButtonBar.ButtonData.OK_DONE);
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
        } else if (!isok && idx == -1) {
            fxDate.getEditor().getStyleClass().add("Invalid");
        }
    }

    /**
     * Search the expiry date on Webpage
     *
     * @param e Starts own background task
     */
    @FXML
    private void btnSearchWeb(Event e) {
        fxProgress.setVisible(true);
        fxStatus.setVisible(true);
        btnWebDate.setDisable(true);
        var searchExpirationDateTask = new SearchExpirationDateTask(hasWebURL, data.getWebUrl());
        searchExpirationDateTask.setOnSucceeded((WorkerStateEvent t) -> {
            fxProgress.setVisible(false);
            fxStatus.setVisible(false);

            String result = searchExpirationDateTask.getValue();
            if (result == null) {
                // do avail found
                Alert alert = new Alert(Alert.AlertType.NONE);
                alert.initOwner(dlgstage);
                alert.setTitle(Konstanten.PROGRAMMNAME);
                alert.setHeaderText("Suche nach Datum");
                alert.setAlertType(Alert.AlertType.WARNING);
                alert.setContentText("Es konnte keine Verfügbarkeit gefunden werden.");
                alert.showAndWait();
            }
            else {
                fxDate.getEditor().setText(result);
            }

            btnWebDate.setDisable(false);
        });
        new Thread(searchExpirationDateTask).start();
    }

    public final boolean SetandShow(Stage dlgstage, BookmarkData data) {
        this.dlgstage = dlgstage;
        this.data = data;
        this.hasWebURL = data.hasWebURL();
        this.dlgstage.setTitle(data.getNote() != null || data.getExpiry() != null ? "Anmerkungen ändern" : "Neue Anmerkungen");
        fxNote.setText(data.getNote() != null ? data.getNote() : "");
        if (data.isLiveStream()) { // For live stream disable expiry handling
            fxExpiry.setDisable(true);
            fxDate.setDisable(true);
            btnWebDate.setDisable(true);
        } else {
            if (data.getExpiry() != null && !data.getExpiry().isEmpty()) { // copy expiry from record
                try {
                    fxDate.setValue(LocalDate.parse(data.getExpiry(), dateformatter));
                } catch (Exception ignored) {
                }
            }
            btnWebDate.setDisable(!hasWebURL);
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
            } catch (Exception e) {
                rc = false;
            }
        }
        return rc;
    }

    /**
     * Get date value or null
     *
     * @return String
     */
    private String getDateValue() {
        String dv = fxDate.getEditor().getText();
        if (dv != null && dv.isEmpty()) {
            dv = null;
        }
        return dv;
    }

}
