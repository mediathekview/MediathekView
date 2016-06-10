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

import com.jidesoft.utils.SystemInfo;
import java.net.URL;
import java.util.ResourceBundle;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.MenuItem;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.StackPane;
import javax.swing.SwingUtilities;
import mediathek.daten.Daten;
import mediathek.gui.MVAboutDialog;
import mediathek.gui.MVHelpDialog;

public class MVFxController extends AnchorPane implements Initializable {

    @FXML
    MenuItem miQuit;
    @FXML
    MenuItem miHelp;
    @FXML
    MenuItem miAbout;
    @FXML
    Button btnFilm;
    @FXML
    StackPane paneTabs;

    @FXML
    @Override
    public void initialize(URL url, ResourceBundle rb) {
        miHelp.setOnAction(e -> {
            SwingUtilities.invokeLater(() -> {
                MVHelpDialog mVHelpDialog = new MVHelpDialog(null, true, null, "Hilfe zum Programm");
                mVHelpDialog.setVisible(true);
                mVHelpDialog.dispose();
            });
        });

        miAbout.setOnAction(e -> {
            SwingUtilities.invokeLater(() -> {
                MVAboutDialog aboutDialog = new MVAboutDialog(null, SystemInfo.isMacOSX());
                aboutDialog.setVisible(true);
                aboutDialog.dispose();
            });
        });
        miQuit.setOnAction(e -> {
            Daten.mVFx.quit();
        });

    }

}
