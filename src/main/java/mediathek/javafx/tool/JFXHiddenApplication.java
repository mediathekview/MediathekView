package mediathek.javafx.tool;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.layout.Pane;
import javafx.stage.Stage;
import javafx.stage.StageStyle;

public class JFXHiddenApplication extends Application {

    private static Stage primaryStage;
    public static void launchApplication() {
        new Thread(JFXHiddenApplication::launch).start();
    }

    @Override
    public void start(Stage primaryStage) {
        JFXHiddenApplication.primaryStage = primaryStage;

        primaryStage.initStyle(StageStyle.TRANSPARENT);
        primaryStage.setScene(new Scene(new Pane(), 1, 1));
        primaryStage.setTitle("MediathekView Invisible Helper Window");


        primaryStage.show();
        primaryStage.hide();
    }

    public static Stage getPrimaryStage() {
        return primaryStage;
    }
}