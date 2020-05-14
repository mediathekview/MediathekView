package mediathek.javafx.filterpanel;

import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.Button;
import javafx.scene.control.ToggleButton;
import javafx.scene.control.ToolBar;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.net.URL;

public class FXFilmToolBar extends ToolBar {
    @FXML
    public Button btnDownloadFilmList;

    @FXML
    public Button btnFilmInfo;

    @FXML
    Button btnPlay;

    @FXML
    Button btnRecord;

    @FXML
    Button btnBookmark;
    
    @FXML
    Button btnManageAbos;
    
    @FXML
    ToggleButton btnShowBookmarkedMovies;
    
    @FXML
    Button btnManageBookMarks;

    @FXML
    Button btnShowFilter;

    @FXML
    JFXSearchPanel jfxSearchField;

    @FXML
    ToggleButton btnSearchThroughDescription;

    public FXFilmToolBar() {
        super();
        try {
            URL url = getClass().getResource("/mediathek/res/programm/fxml/film_toolbar.fxml");
            FXMLLoader fxmlLoader = new FXMLLoader(url);
            fxmlLoader.setRoot(this);
            fxmlLoader.setController(this);
            fxmlLoader.load();
        } catch (IOException e) {
            Logger logger = LogManager.getLogger(FXFilmToolBar.class);
            logger.error("Failed to load FXML!");
        }
    }
}
