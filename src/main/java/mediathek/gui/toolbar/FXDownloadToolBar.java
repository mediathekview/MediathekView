package mediathek.gui.toolbar;

import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.Button;
import javafx.scene.control.ToolBar;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.net.URL;

public class FXDownloadToolBar extends ToolBar {
    private static final Logger logger = LogManager.getLogger(FXDownloadToolBar.class);
    @FXML
    public Button btnRemoveDownload;
    @FXML
    public Button btnCleanup;
    @FXML
    public Button btnFilter;

    public FXDownloadToolBar() {
        super();

        try {
            URL url = getClass().getResource("/mediathek/res/programm/fxml/download_toolbar.fxml");
            FXMLLoader fxmlLoader = new FXMLLoader(url);
            fxmlLoader.setRoot(this);
            fxmlLoader.setController(this);
            fxmlLoader.load();
        } catch (IOException e) {
            logger.error("Failed to load FXML!", e);
        }
    }
}
