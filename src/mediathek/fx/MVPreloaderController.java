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
package mediathek.fx;

import javafx.application.Preloader;
import javafx.application.Preloader.PreloaderNotification;
import javafx.application.Preloader.StateChangeNotification;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import mSearch.tool.DebugMsg;

public class MVPreloaderController extends Preloader {

    @FXML
    ProgressBar prBar;
    @FXML
    Label lblText;
    Stage stage;

    public MVPreloaderController() {

    }

    @Override
    public void start(Stage stage) throws Exception {
        this.stage = stage;

        FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("MVPreloader.fxml"));
        fxmlLoader.setController(this);
        Parent root = fxmlLoader.load();
        stage.initStyle(StageStyle.UNDECORATED);

        Scene scene = new Scene(root, 300, 300);
        stage.setScene(scene);
        stage.show();
    }

    public void setText(String text) {
        lblText.setText(text);
    }

    @Override
    public void handleProgressNotification(ProgressNotification pn) {
        //application loading progress
        DebugMsg.print("handleProgressNotification: " + pn.getProgress());
    }

    @Override
    public void handleStateChangeNotification(StateChangeNotification evt) {
        //ignore, hide after application signals it is ready
        DebugMsg.print("handleStateChangeNotification: " + evt.getType().toString());
    }

    @Override
    public void handleApplicationNotification(PreloaderNotification pn) {
        if (pn instanceof PreloaderNotify) {
            DebugMsg.print("handleApplicationNotification-ProgressNotification: " + ((PreloaderNotify) pn).getProgress());
            double v = ((ProgressNotification) pn).getProgress();
            prBar.setProgress(v);
            lblText.setText(((PreloaderNotify) pn).getText());
        } else if (pn instanceof StateChangeNotification) {
            DebugMsg.print("handleApplicationNotification-StateChangeNotification:" + ((StateChangeNotification) pn).getType().toString());
            if (((StateChangeNotification) pn).getType().equals(StateChangeNotification.Type.BEFORE_START)) {
                stage.hide();
            }

        }
    }

}
