package mediathek.gui.abo

import ca.odell.glazedlists.javafx.EventObservableList
import javafx.fxml.FXML
import javafx.fxml.FXMLLoader
import javafx.fxml.Initializable
import javafx.scene.control.Button
import javafx.scene.control.ComboBox
import javafx.scene.control.ToolBar
import mediathek.tool.SenderListModel
import org.apache.logging.log4j.LogManager
import java.io.IOException
import java.net.URL
import java.util.*

class FXAboToolBar : ToolBar(), Initializable {
    @FXML lateinit var cbSender: ComboBox<String>

    @FXML lateinit var btnOn: Button

    @FXML lateinit var btnOff: Button

    @FXML lateinit var btnDelete: Button

    @FXML lateinit var btnEdit: Button

    @FXML lateinit var btnNewAbo: Button

    override fun initialize(url: URL?, resourceBundle: ResourceBundle?) {
        cbSender.items = EventObservableList(SenderListModel())
        cbSender.selectionModel.select(0)
    }

    companion object {
        private val logger = LogManager.getLogger()
    }

    init {
        try {
            val url = FXAboToolBar::class.java.getResource("/mediathek/res/programm/fxml/abo/abo_toolbar.fxml")
            val fxmlLoader = FXMLLoader(url)
            fxmlLoader.setRoot(this)
            fxmlLoader.setController(this)
            fxmlLoader.load()
        } catch (e: IOException) {
            logger.error("Failed to load FXML!", e)
        }
    }
}