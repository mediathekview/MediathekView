package mediathek.javafx.bookmark;

import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.TextArea;
import javafx.stage.Stage;
import org.jetbrains.annotations.NotNull;

import java.net.URL;
import java.util.ResourceBundle;


/**
 * Dialog to set expiry date and notes for bookmarked movies
 * <p>
 * includes search for movies's expiry date on webpage
 *
 * @author Klaus Wich <klaus.wich@aim.com>
 */

public class BookmarkNoteDialogController implements Initializable {
    @FXML
    private TextArea fxNote;
    @FXML
    protected Button SaveButton;
    @FXML
    protected Button CancelButton;
    protected Stage stage;
    protected boolean datachanged;
    private BookmarkData data;

    @Override
    public void initialize(URL arg0, ResourceBundle arg1) {
        ButtonBar.setButtonData(CancelButton, ButtonBar.ButtonData.CANCEL_CLOSE);
        ButtonBar.setButtonData(SaveButton, ButtonBar.ButtonData.OK_DONE);
    }

    @FXML
    protected void handleCancel() {
        datachanged = false;
        stage.hide();
    }

    @FXML
    protected void handleSave() {
        var fxNoteText = fxNote.getText();
        if (!fxNoteText.equals(data.getNote())) {
            data.setNote(fxNoteText);
            datachanged = true;
        }

        stage.hide();
    }

    public final boolean setAndShow(@NotNull Stage dlgstage, @NotNull BookmarkData data) {
        this.data = data;
        this.stage = dlgstage;

        data.getNoteOptional().ifPresentOrElse(note -> {
            stage.setTitle("Notizen ändern");
            fxNote.setText(note);
        }, () -> {
            stage.setTitle("Neue Notiz");
            fxNote.setText("");
        });

        stage.showAndWait();
        return datachanged;
    }
}
