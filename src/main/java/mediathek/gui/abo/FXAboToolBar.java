package mediathek.gui.abo;

import ca.odell.glazedlists.javafx.EventObservableList;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ToolBar;
import mediathek.config.Daten;
import mediathek.tool.SenderList;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;


public class FXAboToolBar extends ToolBar implements Initializable {
    private static final Logger logger = LogManager.getLogger(FXAboToolBar.class);
    @FXML
    public ComboBox<String> cbSender;
    @FXML
    public Button btnOn;
    @FXML
    public Button btnOff;
    @FXML
    public Button btnDelete;
    @FXML
    public Button btnEdit;
    @FXML
    public Button btnNewAbo;

    public FXAboToolBar() {
        super();

        try {
            URL url = getClass().getResource("/mediathek/res/programm/fxml/abo_toolbar.fxml");
            FXMLLoader fxmlLoader = new FXMLLoader(url);
            fxmlLoader.setRoot(this);
            fxmlLoader.setController(this);
            fxmlLoader.load();
        } catch (IOException e) {
            logger.error("Failed to load FXML!", e);
        }
    }

    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {
        var senderList = new SenderList(Daten.getInstance().getListeFilme().getBaseSenderList());
        cbSender.setItems(new EventObservableList<>(senderList));
        cbSender.getSelectionModel().select(0);
    }
}
