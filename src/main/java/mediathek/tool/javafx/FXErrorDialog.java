package mediathek.tool.javafx;

import javafx.scene.control.Alert;
import javafx.scene.control.Label;
import javafx.scene.control.TextArea;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.stage.Modality;
import mediathek.javafx.tool.JFXHiddenApplication;
import mediathek.javafx.tool.JavaFxUtils;
import mediathek.mainwindow.MediathekGui;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;

public class FXErrorDialog {
    private static GridPane createExceptionPanel(@NotNull Exception ex) throws IOException {
        GridPane expContent = new GridPane();
        try (StringWriter sw = new StringWriter();
             PrintWriter pw = new PrintWriter(sw)) {
            ex.printStackTrace(pw);
            String exceptionText = sw.toString();

            Label label = new Label("Stacktrace:");

            TextArea textArea = new TextArea(exceptionText);
            textArea.setEditable(false);
            textArea.setWrapText(true);

            textArea.setMaxWidth(Double.MAX_VALUE);
            textArea.setMaxHeight(Double.MAX_VALUE);
            GridPane.setVgrow(textArea, Priority.ALWAYS);
            GridPane.setHgrow(textArea, Priority.ALWAYS);

            expContent.setMaxWidth(Double.MAX_VALUE);
            expContent.add(label, 0, 0);
            expContent.add(textArea, 0, 1);
        }

        return expContent;
    }

    private static Alert getBaseAlert(@NotNull String title, @NotNull String header, @NotNull String detailedErrorMessage) {
        var alert = new Alert(Alert.AlertType.ERROR);
        alert.setTitle(title);
        alert.setHeaderText(header);
        alert.setContentText(detailedErrorMessage);
        alert.initModality(Modality.APPLICATION_MODAL);

        return alert;
    }

    private static void createExceptionContent(@NotNull Alert alert, @NotNull Exception ex) {
        try {
            var content = createExceptionPanel(ex);
            alert.getDialogPane().setExpandableContent(content);
        }
        catch (IOException ignored) {
            // do not set content
        }
    }

    public static void showErrorDialog(String title, String header, String detailedErrorMessage, @NotNull Exception ex) {
        Alert alert = getBaseAlert(title,header, detailedErrorMessage);
        createExceptionContent(alert, ex);
        alert.initOwner(JFXHiddenApplication.getPrimaryStage());
        JFXHiddenApplication.showAlert(alert, MediathekGui.ui());
    }

    public static void showErrorDialogWithoutParent(String title, String header, String detailedErrorMessage, @NotNull Exception ex) {
        JavaFxUtils.invokeInFxThreadAndWait(() -> {
            Alert alert = getBaseAlert(title,header, detailedErrorMessage);
            createExceptionContent(alert, ex);
            alert.showAndWait();
        });
    }
}
