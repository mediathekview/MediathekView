package mSearch.tool.javafx;

import javafx.application.Platform;
import javafx.scene.control.Alert;
import javafx.scene.control.Label;
import javafx.scene.control.TextArea;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.stage.Modality;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;

public class FXErrorDialog {
    public static void showErrorDialog(String title, String header, String detailedErrorMessage, Exception ex) {
        Platform.runLater(() -> {
            Alert alert = new Alert(Alert.AlertType.ERROR);
            alert.setTitle(title);
            alert.setHeaderText(header);
            alert.setContentText(detailedErrorMessage);

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

                GridPane expContent = new GridPane();
                expContent.setMaxWidth(Double.MAX_VALUE);
                expContent.add(label, 0, 0);
                expContent.add(textArea, 0, 1);

                alert.getDialogPane().setExpandableContent(expContent);
                alert.initModality(Modality.APPLICATION_MODAL);
                alert.showAndWait();
            } catch (IOException ignored) {
            }
        });
    }
}
