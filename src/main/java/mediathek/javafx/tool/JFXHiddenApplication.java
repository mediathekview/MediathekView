package mediathek.javafx.tool;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonType;
import javafx.scene.image.Image;
import javafx.scene.layout.Pane;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import mediathek.config.Konstanten;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class JFXHiddenApplication extends Application {

    private static Stage primaryStage;
    private static final String ICON_NAME = "MediathekView.png";
    private static final String ICON_PATH = "/mediathek/res/";

    public static void launchApplication() {
        new Thread(JFXHiddenApplication::launch).start();
    }

    public static Image getApplicationImage() {
        return new Image(JFXHiddenApplication.class.getResourceAsStream(ICON_PATH + ICON_NAME));
    }

    @Override
    public void start(Stage primaryStage) {
        JFXHiddenApplication.primaryStage = primaryStage;

        primaryStage.initStyle(StageStyle.TRANSPARENT);
        primaryStage.setScene(new Scene(new Pane(), 1, 1));
        primaryStage.setTitle("MediathekView Invisible Helper Window");


        primaryStage.getIcons().add(getApplicationImage());
        primaryStage.show();
        primaryStage.hide();
    }

    public static Stage getPrimaryStage() {
        return primaryStage;
    }

    public static void showJavaFXDialog(Stage fxDialog, JFrame swingParent) {
        fxDialog.setOpacity(0d);
        fxDialog.show();
        fxDialog.hide();
        fxDialog.setOpacity(1d);

        fxDialog.setX(swingParent.getBounds().getCenterX() - (fxDialog.getWidth() / 2));
        fxDialog.setY(swingParent.getBounds().getCenterY() - (fxDialog.getHeight() / 2));

        fakeModalDialog(fxDialog, swingParent);

        fxDialog.setAlwaysOnTop(true);
        fxDialog.showAndWait();
    }

    public static void showAlert(@NotNull Alert alert, @NotNull JFrame swingParent) {
        var dialogPane = alert.getDialogPane();
        var scene = dialogPane.getScene();
        Stage alertStage = new Stage();
        alertStage.getIcons().add(getApplicationImage());
        alertStage.setTitle(Konstanten.PROGRAMMNAME);
        alertStage.setScene(scene);
        alertStage.initModality(Modality.APPLICATION_MODAL);
        alertStage.initOwner(getPrimaryStage());
        final Button btnFoo = (Button) dialogPane.lookupButton( ButtonType.OK );
        btnFoo.setOnAction(evt -> {
            alertStage.close();
            evt.consume();
        });

        showJavaFXDialog(alertStage, swingParent);
    }

    private static void fakeModalDialog(Stage fxDialog, JFrame swingParent) {

        fxDialog.setOnShown(e -> SwingUtilities.invokeLater(() -> {
            swingParent.setEnabled(false);
            if (swingParent.getJMenuBar() != null) {
                swingParent.getJMenuBar().setEnabled(false);
            }
        }));

        fxDialog.setOnHidden(e -> SwingUtilities.invokeLater(() -> {
            swingParent.setEnabled(true);
            if (swingParent.getJMenuBar() != null) {
                swingParent.getJMenuBar().setEnabled(true);
            }
            swingParent.toFront();
        }));
    }
}