package mediathek.javafx.filterpanel;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.ComboBox;
import javafx.scene.control.SingleSelectionModel;
import javafx.scene.layout.HBox;
import mediathek.tool.FilterConfiguration;
import mediathek.tool.FilterDTO;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.net.URL;

public class FXFilmToolBar extends HBox {
    @FXML
    ComboBox<FilterDTO> filterSelect;
    private ObservableList<FilterDTO> availableFilters;

    public FXFilmToolBar() {
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
        availableFilters = FXCollections.observableArrayList(filterConfig.getAvailableFilters());

        FilterConfiguration.addAvailableFiltersObserver(() -> {
            availableFilters.clear();
            availableFilters.addAll(filterConfig.getAvailableFilters());
        });

        SingleSelectionModel<FilterDTO> selectionModel = filterSelect.getSelectionModel();
        FilterConfiguration.addCurrentFiltersObserver(selectionModel::select);
        filterSelect.setItems(availableFilters);

        selectionModel.select(filterConfig.getCurrentFilter());
        selectionModel.selectedItemProperty().addListener((observableValue, oldValue, newValue) -> {
            if (newValue != null && !newValue.equals(filterConfig.getCurrentFilter())) {
                filterConfig.setCurrentFilter(newValue);
            }
        });
    }
}
