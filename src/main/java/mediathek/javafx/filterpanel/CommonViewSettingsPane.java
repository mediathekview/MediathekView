package mediathek.javafx.filterpanel;

import javafx.application.Platform;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.layout.VBox;
import mediathek.config.Daten;
import mediathek.gui.messages.TableModelChangeEvent;
import mediathek.tool.FilterDTO;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;
import java.util.function.Consumer;

public class CommonViewSettingsPane extends VBox implements Initializable {
    private static final Logger logger = LogManager.getLogger(CommonViewSettingsPane.class);
    @FXML
    public Button btnDeleteFilterSettings;
    @FXML
    public CheckBox cbShowOnlyHd;
    @FXML
    public CheckBox cbShowSubtitlesOnly;
    @FXML
    public CheckBox cbShowNewOnly;
    @FXML
    public CheckBox cbShowBookMarkedOnly;
    @FXML
    public CheckBox cbShowOnlyLivestreams;
    @FXML
    public CheckBox cbShowUnseenOnly;
    @FXML
    public CheckBox cbDontShowAbos;
    @FXML
    public CheckBox cbDontShowGebaerdensprache;
    @FXML
    public CheckBox cbDontShowTrailers;
    @FXML
    public CheckBox cbDontShowAudioVersions;
    @FXML
    public SenderBoxNode senderBoxNode;
    @FXML
    public ThemaComboBox _themaComboBox;
    @FXML
    public FilmLenghtSliderNode filmLengthSliderNode;
    @FXML
    public ZeitraumSpinner zeitraumSpinner;
    @FXML
    private Label _themaLabel;

    @FXML
    private ComboBox<FilterDTO> filterSelect;

    @FXML
    public Button btnDeleteCurrentFilter;

    public CommonViewSettingsPane() {
        super();

        try {
            URL url = getClass().getResource("/mediathek/res/programm/fxml/filter_settings_pane.fxml");
            FXMLLoader fxmlLoader = new FXMLLoader(url);
            fxmlLoader.setRoot(this);
            fxmlLoader.setController(this);
            fxmlLoader.load();
        } catch (IOException e) {
            logger.error("Failed to load FXML!", e);
        }
    }

    /**
     * Prevent user from changing filter settings while the swing table model gets updated.
     * @param evt the model event
     */
    @Handler
    private void handleTableModelChangeEvent(TableModelChangeEvent evt) {
        Platform.runLater(() -> {
            final boolean disable = evt.active;
            btnDeleteFilterSettings.setDisable(disable);
            cbShowOnlyHd.setDisable(disable);
            cbShowSubtitlesOnly.setDisable(disable);
            cbShowNewOnly.setDisable(disable);
            cbShowBookMarkedOnly.setDisable(disable);
            cbShowOnlyLivestreams.setDisable(disable);
            cbShowUnseenOnly.setDisable(disable);
            cbDontShowAbos.setDisable(disable);
            cbDontShowGebaerdensprache.setDisable(disable);
            cbDontShowTrailers.setDisable(disable);
            cbDontShowAudioVersions.setDisable(disable);
            senderBoxNode.setDisable(disable);
            _themaComboBox.setDisable(disable);
            filmLengthSliderNode.setDisable(disable);
            zeitraumSpinner.setDisable(disable);
            filterSelect.setDisable(disable);
            btnDeleteCurrentFilter.setDisable(disable);
        });
    }

    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {
        _themaLabel.setMinWidth(USE_PREF_SIZE);
        //font size is greater on tested ubuntu linux :(
        if (SystemUtils.IS_OS_LINUX)
            _themaLabel.setPrefWidth(50d);
        else
            _themaLabel.setPrefWidth(45d);

        Daten.getInstance().getMessageBus().subscribe(this);
    }

    public void setFilterSelectionEvent(EventHandler<ActionEvent> eventHandler)
    {
        filterSelect.setOnAction(eventHandler);
    }

    public FilterDTO getSelectedFilter()
    {
        return filterSelect.getValue();
    }

    public void setAvailableFilters(ObservableList<FilterDTO> filters)
    {
        filterSelect.setItems(filters);
    }

    public void selectFilter(FilterDTO filter)
    {
        filterSelect.getSelectionModel().select(filter);
    }
}
