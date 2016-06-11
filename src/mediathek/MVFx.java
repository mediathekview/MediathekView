/*
 * MediathekView
 * Copyright (C) 2014 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek;

import java.awt.*;
import javafx.application.Application;
import javafx.application.Platform;
import javafx.application.Preloader.ProgressNotification;
import javafx.application.Preloader.StateChangeNotification;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;
import mediathek.controller.Log;
import mediathek.daten.Daten;
import mediathek.tool.Konstanten;

public class MVFx extends Application {

    Daten daten;
    int splash = 1;
    final int SPLASH_MAX = 6;

    @Override
    public void init() throws Exception {
        try {
            final SplashScreen splash = SplashScreen.getSplashScreen();
            if (splash != null) {
                splash.close();
            }
        } catch (Exception ignored) {
            Log.systemMeldung("NoSplashscreen");
        }

        splash("Anfang");
        splash("Start");
        splash("Weiter");
        splash("Und noch weiter");
        splash("Fast geschafft");
        splash("ENDE");

        daten = new Daten("", this);

    }

    @Override
    public void start(Stage primaryStage) throws Exception {
//        final Parent root = FXMLLoader.load(getClass().getResource("mvFx.fxml"));
//        Scene scene = new Scene(root, 500, 500);
//        primaryStage.setTitle(Konstanten.PROGRAMMNAME + " " + Konstanten.VERSION);
//        primaryStage.setScene(scene);
//        primaryStage.show();

        FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("mvFx.fxml"));
        MVFxController c = new MVFxController();
        fxmlLoader.setController(c);
        Parent root = fxmlLoader.load();
        Scene scene = new Scene(root, 500, 500);

        primaryStage.setTitle(Konstanten.PROGRAMMNAME + " " + Konstanten.VERSION);
        primaryStage.setScene(scene);
        primaryStage.setOnCloseRequest(e -> quit());

        primaryStage.show();
        notifyPreloader(new StateChangeNotification(StateChangeNotification.Type.BEFORE_START));
    }

    private synchronized void splash(String text) {
        try {
            wait(200); //onliy for testing
        } catch (Exception ig) {
        }
        notifyPreloader(new ProgressNotification(((double) splash++) / SPLASH_MAX));
    }

    public void quit() {
        Platform.exit();
        System.exit(0);
    }

    @Override
    public void stop() {
    }

}
