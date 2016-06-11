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

import javafx.application.Preloader;
import javafx.application.Preloader.PreloaderNotification;
import javafx.application.Preloader.StateChangeNotification;
import javafx.scene.Scene;
import javafx.scene.control.ProgressBar;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;
import mediathek.tool.DebugMsg;

public class MVPreloader extends Preloader {

    ProgressBar bar;
    Stage stage;

    private Scene createPreloaderScene() {
        bar = new ProgressBar(0);
        BorderPane p = new BorderPane();
        p.setCenter(bar);
        return new Scene(p, 300, 150);
    }

    @Override
    public void start(Stage stage) throws Exception {
        this.stage = stage;
        stage.setScene(createPreloaderScene());
        stage.show();
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
        if (pn instanceof ProgressNotification) {
            DebugMsg.print("handleApplicationNotification-ProgressNotification: " + ((ProgressNotification) pn).getProgress());
            double v = ((ProgressNotification) pn).getProgress();
            bar.setProgress(v);
        } else if (pn instanceof StateChangeNotification) {
            DebugMsg.print("handleApplicationNotification-StateChangeNotification:" + ((StateChangeNotification) pn).getType().toString());
            if (((StateChangeNotification) pn).getType().equals(StateChangeNotification.Type.BEFORE_START)) {
                stage.hide();
            }

        }
    }
}
