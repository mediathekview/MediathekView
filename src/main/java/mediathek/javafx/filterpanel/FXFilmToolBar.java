package mediathek.javafx.filterpanel;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.*;
import mediathek.tool.FilterConfiguration;
import mediathek.tool.FilterDTO;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.net.URL;

public class FXFilmToolBar extends ToolBar {
  @FXML public Button btnDownloadFilmList;

  @FXML public Button btnFilmInfo;

  @FXML Button btnPlay;

  @FXML Button btnRecord;

  @FXML Button btnBookmark;

  @FXML Button btnManageAbos;

  @FXML ToggleButton btnShowBookmarkedMovies;

  @FXML Button btnManageBookMarks;

  @FXML Button btnShowFilter;

  @FXML JFXSearchPanel jfxSearchField;

  @FXML ToggleButton btnSearchThroughDescription;

  @FXML ComboBox<FilterDTO> filterSelect;

  public FXFilmToolBar() {
    super();
    try {
      URL url = getClass().getResource("/mediathek/res/programm/fxml/film_toolbar.fxml");
      FXMLLoader fxmlLoader = new FXMLLoader(url);
      fxmlLoader.setRoot(this);
      fxmlLoader.setController(this);
      fxmlLoader.load();
      setUpFilterSelect();
    } catch (IOException e) {
      Logger logger = LogManager.getLogger(FXFilmToolBar.class);
      logger.error("Failed to load FXML!");
    }
  }

  private void setUpFilterSelect() {
    FilterConfiguration filterConfig = new FilterConfiguration();
    ObservableList<FilterDTO> availableFilters =
        FXCollections.observableArrayList(filterConfig.getAvailableFilters());
    FilterConfiguration.addAvailableFiltersObserver(
        () -> {
          availableFilters.clear();
          availableFilters.addAll(filterConfig.getAvailableFilters());
        });

    SingleSelectionModel<FilterDTO> selectionModel = filterSelect.getSelectionModel();
    FilterConfiguration.addCurrentFiltersObserver(selectionModel::select);
    filterSelect.setItems(availableFilters);
    selectionModel.select(filterConfig.getCurrentFilter());
    selectionModel
        .selectedItemProperty()
        .addListener(
            (observableValue, oldValue, newValue) -> {
              if (newValue != null && !newValue.equals(filterConfig.getCurrentFilter())) {
                filterConfig.setCurrentFilter(newValue);
              }
            });
  }
}
