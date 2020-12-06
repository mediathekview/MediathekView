package mediathek.javafx.filterpanel;

import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import javafx.scene.layout.VBox;
import javafx.util.StringConverter;
import mediathek.gui.messages.TableModelChangeEvent;
import mediathek.tool.FilterDTO;
import mediathek.tool.MessageBus;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

public class CommonViewSettingsPane extends VBox implements Initializable {
  private static final Logger logger = LogManager.getLogger(CommonViewSettingsPane.class);
  @FXML public Button btnDeleteFilterSettings;
  @FXML public CheckBox cbShowOnlyHd;
  @FXML public CheckBox cbShowSubtitlesOnly;
  @FXML public CheckBox cbShowNewOnly;
  @FXML public CheckBox cbShowBookMarkedOnly;
  @FXML public CheckBox cbShowOnlyLivestreams;
  @FXML public CheckBox cbShowUnseenOnly;
  @FXML public CheckBox cbDontShowAbos;
  @FXML public CheckBox cbDontShowGebaerdensprache;
  @FXML public CheckBox cbDontShowTrailers;
  @FXML public CheckBox cbDontShowAudioVersions;
  @FXML public SenderBoxNode senderBoxNode;
  @FXML public ThemaComboBox themaComboBox;
  @FXML public FilmLenghtSliderNode filmLengthSliderNode;
  @FXML public ZeitraumSpinner zeitraumSpinner;
  @FXML public Button btnDeleteCurrentFilter;
  @FXML private Label themaLabel;
  @FXML private ComboBox<FilterDTO> filterSelect;
  @FXML private Button btnAddNewFilter;
  @FXML private Button btnSaveFilterSettings;
  @FXML private Button btnRestoreFilterSettings;
  private boolean deleteCurrentFilterButtonDisabled;
  private boolean saveButtonDisabled = false;
    private boolean restoreButtonDisabled;

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

  public void setSaveButtonVisibility(boolean visible) {
        btnSaveFilterSettings.setVisible(visible);
  }

    public void setRestoreButtonVisibility(boolean visible) {
        btnRestoreFilterSettings.setVisible(visible);
    }

  public void setSaveButtonDisabled(boolean saveButtonDisabled) {
      btnSaveFilterSettings.setDisable(saveButtonDisabled);
    this.saveButtonDisabled = saveButtonDisabled;
  }

  public void registerSaveButtonListener(EventHandler<ActionEvent> eventHandler) {
      btnSaveFilterSettings.setOnAction(eventHandler);
  }

    public void setRestoreButtonDisabled(boolean restoreButtonDisabled) {
        btnRestoreFilterSettings.setDisable(restoreButtonDisabled);
        this.restoreButtonDisabled = restoreButtonDisabled;
    }

    public void registerRestoreButtonListener(EventHandler<ActionEvent> eventHandler) {
        btnRestoreFilterSettings.setOnAction(eventHandler);
    }

  /**
   * Prevent user from changing filter settings while the swing table model gets updated.
   *
   * @param evt the model event
   */
  @Handler
  private void handleTableModelChangeEvent(TableModelChangeEvent evt) {
    Platform.runLater(
        () -> {
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
          themaComboBox.setDisable(disable);
          filmLengthSliderNode.setDisable(disable);
          zeitraumSpinner.setDisable(disable);
          filterSelect.setDisable(disable);
          btnDeleteCurrentFilter.setDisable(disable || deleteCurrentFilterButtonDisabled);
          btnAddNewFilter.setDisable(disable);
          btnSaveFilterSettings.setDisable(disable || saveButtonDisabled);
          btnRestoreFilterSettings.setDisable(disable || restoreButtonDisabled);
        });
  }

  public void disableDeleteCurrentFilterButton(boolean disable) {
    deleteCurrentFilterButtonDisabled = disable;
    btnDeleteCurrentFilter.setDisable(disable);
  }

  @Override
  public void initialize(URL url, ResourceBundle resourceBundle) {
    themaLabel.setMinWidth(USE_PREF_SIZE);
    // font size is greater on tested ubuntu linux :(
    if (SystemUtils.IS_OS_LINUX) themaLabel.setPrefWidth(50d);
    else themaLabel.setPrefWidth(45d);

      MessageBus.getMessageBus().subscribe(this);
  }

  public void setFilterSelectionChangeListener(ChangeListener<FilterDTO> changeListener) {
    filterSelect.getSelectionModel().selectedItemProperty().addListener(changeListener);
  }

  public FilterDTO getSelectedFilter() {
    return filterSelect.getValue();
  }

  public void setAvailableFilters(ObservableList<FilterDTO> filters) {
    filterSelect.setItems(filters);
  }

  public void selectFilter(FilterDTO filter) {
    SingleSelectionModel<FilterDTO> selectionModel = filterSelect.getSelectionModel();
    if (!filter.equals(selectionModel.getSelectedItem())) {
      selectionModel.select(filter);
    }
  }

  public void setAddNewFilterButtonEventHandler(EventHandler<ActionEvent> eventHandler) {
    btnAddNewFilter.setOnAction(eventHandler);
  }

  public void setFilterSelectionStringConverter(StringConverter<FilterDTO> filterStringConverter) {
    filterSelect.setConverter(filterStringConverter);
  }
}
