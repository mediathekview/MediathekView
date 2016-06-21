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
import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.StackPane;
import javax.swing.SwingUtilities;
import mediathek.daten.Daten;
import mediathek.gui.MVAboutDialog;
import mediathek.gui.MVHelpDialog;
import mediathek.gui.dialogEinstellungen.DialogEinstellungen;

public class MVFxController extends AnchorPane implements Initializable {

    @FXML
    MenuItem miQuitt;
    @FXML
    Menu menuFilm;
    @FXML
    Menu menuDownload;
    @FXML
    Menu menuAbo;

    @FXML
    MenuItem miConfig;
    @FXML
    MenuItem miHelp;
    @FXML
    MenuItem miOver;

    @FXML
    StackPane paneCont;

    private AnchorPane FXFilm;
    private AnchorPane FXDownload;
    private AnchorPane FXAbo;
    private DialogEinstellungen dialogEinstellungen;

    @FXML
    @Override
    public void initialize(URL url, ResourceBundle rb) {
        dialogEinstellungen = new DialogEinstellungen(null, MVFx.daten);

        menuFilm.setOnShowing(e -> {
            FXFilm.toFront();
        });
        menuDownload.setOnShowing(e -> {
            FXDownload.toFront();
        });
        menuAbo.setOnShowing(e -> {
            FXAbo.toFront();
        });
        //Quitt
        miQuitt.setOnAction(e -> Daten.mVFx.quit());

        //Config
        miConfig.setOnAction(e -> {
            Platform.runLater(() -> {
                dialogEinstellungen.setVisible(true);
            });
        });

        //Help
        miHelp.setOnAction(e -> {
            SwingUtilities.invokeLater(() -> {
                MVHelpDialog mVHelpDialog = new MVHelpDialog(null, true, null, "Hilfe zum Programm");
                mVHelpDialog.setVisible(true);
                mVHelpDialog.dispose();
            });
        });
        miOver.setOnAction(e -> {
            SwingUtilities.invokeLater(() -> {
                MVAboutDialog aboutDialog = new MVAboutDialog(null, SystemInfo.isMacOSX());
                aboutDialog.setVisible(true);
                aboutDialog.dispose();
            });
        });

        try {
            FXFilm = (AnchorPane) FXMLLoader.load(getClass().getResource("/mediathek/fx/gui/FXFilm.fxml"));
            paneCont.getChildren().add(FXFilm);
            FXDownload = (AnchorPane) FXMLLoader.load(getClass().getResource("/mediathek/fx/gui/FXDownload.fxml"));
            paneCont.getChildren().add(FXDownload);
            FXAbo = (AnchorPane) FXMLLoader.load(getClass().getResource("/mediathek/fx/gui/FXAbo.fxml"));
            paneCont.getChildren().add(FXAbo);
        } catch (Exception ex) {
        }
        FXFilm.toFront();
    }
    //    public synchronized void addSwing() {
    //        final SwingNode sn = new SwingNode();
    //        SwingUtilities.invokeLater(() -> {
    //            final GuiFilme gf = new GuiFilme(MVFx.daten, null);
    //            sn.setContent(gf);
    //            sn.maxHeight(Double.MAX_VALUE);
    //            sn.maxWidth(Double.MAX_VALUE);
    //            sn.prefHeight(Double.MAX_VALUE);
    //            sn.prefWidth(Double.MAX_VALUE);
    //            gf.setPreferredSize(new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE));
    //            gf.setMaximumSize(new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE));
    //            FXFilm.widthProperty().addListener((w, o, n) -> gf.resize((int) n.intValue(), (int) FXFilm.getHeight()));
    //            FXFilm.heightProperty().addListener((w, o, n) -> gf.resize((int) FXFilm.getWidth(), (int) n.intValue()));
    //        });
    //        HBox fxControls = new HBox();
    //        fxControls.setSpacing(10);
    //        fxControls.getChildren().addAll(sn);
    //        FXFilm.getChildren().add(fxControls);
    //    }

    private synchronized void startConfig() {
        Platform.runLater(() -> {
            dialogEinstellungen.setVisible(true);
        });
        try {
            this.wait(1000);
        } catch (InterruptedException ex) {
        }
    }

}
