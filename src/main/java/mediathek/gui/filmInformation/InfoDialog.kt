package mediathek.gui.filmInformation

import javafx.application.Platform
import javafx.embed.swing.JFXPanel
import javafx.embed.swing.SwingFXUtils
import javafx.event.EventHandler
import javafx.geometry.Pos
import javafx.scene.Scene
import javafx.scene.control.*
import javafx.scene.image.ImageView
import mediathek.config.Daten
import mediathek.daten.DatenFilm
import mediathek.gui.actions.UrlHyperlinkAction
import mediathek.javafx.tool.JavaFxUtils
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.GuiFunktionen
import mediathek.tool.MVSenderIconCache
import net.miginfocom.layout.CC
import org.apache.commons.configuration2.sync.LockMode
import org.tbee.javafx.scene.layout.MigPane
import java.awt.BorderLayout
import java.awt.Point
import java.awt.Window
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import java.net.URISyntaxException
import java.util.*
import javax.swing.ImageIcon
import javax.swing.JDialog
import javax.swing.SwingUtilities

class InfoDialog(parent: Window?) : JDialog(parent) {
    private val config = ApplicationConfiguration.getConfiguration()
    private val senderIconCache: MVSenderIconCache = Daten.getInstance().senderIconCache
    private var currentFilm: DatenFilm? = null
    private val lblSender = Label()
    private val lblThema = Label()
    private val lblTitle = Label()
    private val lblDate = Label()
    private val lblUhrzeit = Label()
    private val lblDuration = Label()
    private val lblSize = Label()
    private val cbHq = DisabledCheckBox()
    private val cbSubtitle = DisabledCheckBox()
    private val lblGeo = Label()
    private val lblAbo = Label()
    private val hyperlink = Hyperlink("Hier klicken")
    private val lblDescription = TextArea()

    private fun installContextMenu(component: Label) {
        val ctMenu = ContextMenu()
        val mi = MenuItem("Text in die Zwischenablage kopieren")
        mi.onAction = EventHandler {
            GuiFunktionen.copyToClipboard(component.text)
        }
        ctMenu.items.add(mi)
        component.contextMenu = ctMenu
    }

    /**
     * Restore window position from config settings.
     */
    private fun restoreLocation() {
        config.lock(LockMode.READ)
        try {
            val newLocation = Point()
            newLocation.x = config.getInt(FILM_INFO_LOCATION_X)
            newLocation.y = config.getInt(FILM_INFO_LOCATION_Y)
            location = newLocation
        } catch (ignored: NoSuchElementException) {
        } finally {
            config.unlock(LockMode.READ)
        }
    }

    /**
     * Save window position to config.
     */
    private fun saveLocation() {
        //prevent strange OpenJDK bug on Linux where getLocationOnScreen will fail if not visible...
        if (!isVisible) return
        config.lock(LockMode.WRITE)
        try {
            val location = locationOnScreen
            config.setProperty(FILM_INFO_LOCATION_X, location.x)
            config.setProperty(FILM_INFO_LOCATION_Y, location.y)
        } finally {
            config.unlock(LockMode.WRITE)
        }
    }

    fun showInfo() {
        updateTextFields()
        if (!isVisible) isVisible = true
    }

    private fun clearControls() {
        Platform.runLater {
            lblDescription.text = ""
            lblAbo.text = ""
            lblGeo.text = ""
            lblSender.text = ""
            lblSender.graphic = null
            lblSize.text = ""
            lblThema.text = ""
            lblTitle.text = ""
            lblDate.text = ""
            lblUhrzeit.text = ""
            lblDuration.text = ""
            cbHq.isSelected = false
            cbSubtitle.isSelected = false
            hyperlink.tooltip = null
            hyperlink.isDisable = true
        }
    }

    private fun updateTextFields() {
        if (currentFilm == null) {
            clearControls()
        } else {
            Platform.runLater {
                val desc = currentFilm!!.description.trim { it <= ' ' }
                lblDescription.text = desc
                lblDescription.scrollTop = 0.0
                lblDescription.scrollLeft = 0.0
                senderIconCache[currentFilm!!.sender, true].ifPresent { icon: ImageIcon? ->
                    lblSender.text = ""
                    lblSender.graphic = ImageView(
                            SwingFXUtils.toFXImage(JavaFxUtils.toBufferedImage(icon), null))
                }
                lblGeo.text = currentFilm!!.geo.orElse("")
                lblSize.text = currentFilm!!.size
                lblThema.text = currentFilm!!.thema
                lblTitle.text = currentFilm!!.title
                lblDate.text = currentFilm!!.sendeDatum
                lblUhrzeit.text = currentFilm!!.sendeZeit
                lblDuration.text = currentFilm!!.dauer
                cbHq.isSelected = currentFilm!!.isHighQuality
                cbSubtitle.isSelected = currentFilm!!.hasSubtitle()
                hyperlink.tooltip = Tooltip(currentFilm!!.websiteLink)
                hyperlink.isDisable = false
                lblAbo.text = currentFilm!!.aboName
            }
        }
    }

    /**
     * This will set the display to the new film data.
     *
     * @param film the film data
     */
    fun updateCurrentFilm(film: DatenFilm?) {
        currentFilm = film
        if (isVisible) updateTextFields()
    }

    private fun buildLayout() {
        val contentPane = contentPane
        contentPane.layout = BorderLayout()
        val newFxPanel = JFXPanel()
        contentPane.add(newFxPanel, BorderLayout.CENTER)
        Platform.runLater {
            val migPane = MigPane(
                    "hidemode 3",  //columns
                    "[fill,shrink 0]" +
                            "[fill]",  //rows
                    "[]" +
                            "[]" +
                            "[]" +
                            "[]" +
                            "[]" +
                            "[]" +
                            "[]" +
                            "[]" +
                            "[]" +
                            "[]" +
                            "[]" +
                            "[]" +
                            "[]" +
                            "[fill,grow]")
            migPane.add(RightOrientedLabel("Sender:"), CC().cell(0, 0))
            migPane.add(lblSender, CC().cell(1, 0))

            migPane.add(RightOrientedLabel("Thema:"), CC().cell(0, 1))
            lblThema.isWrapText = true
            installContextMenu(lblThema)
            migPane.add(lblThema, CC().cell(1, 1).growY())

            migPane.add(RightOrientedLabel("Titel:"), CC().cell(0, 2))
            lblTitle.isWrapText = true
            installContextMenu(lblTitle)
            migPane.add(lblTitle, CC().cell(1, 2).growY())

            migPane.add(RightOrientedLabel("Datum:"), CC().cell(0, 3))
            migPane.add(lblDate, CC().cell(1, 3))
            migPane.add(RightOrientedLabel("Uhrzeit:"), CC().cell(0, 4))
            migPane.add(lblUhrzeit, CC().cell(1, 4))
            migPane.add(RightOrientedLabel("Dauer:"), CC().cell(0, 5))
            migPane.add(lblDuration, CC().cell(1, 5))
            migPane.add(RightOrientedLabel("Größe (MB):"), CC().cell(0, 6))
            migPane.add(lblSize, CC().cell(1, 6))
            migPane.add(RightOrientedLabel("HQ:"), CC().cell(0, 7))
            migPane.add(cbHq, CC().cell(1, 7))
            migPane.add(RightOrientedLabel("Untertitel:"), CC().cell(0, 8))
            migPane.add(cbSubtitle, CC().cell(1, 8))
            migPane.add(RightOrientedLabel("Geo:"), CC().cell(0, 9))
            migPane.add(lblGeo, CC().cell(1, 9))
            migPane.add(RightOrientedLabel("Abo:"), CC().cell(0, 10))
            migPane.add(lblAbo, CC().cell(1, 10))
            migPane.add(RightOrientedLabel("Website:"), CC().cell(0, 11))
            migPane.add(RightOrientedLabel("Beschreibung:"), CC().cell(0, 12))

            hyperlink.contextMenu = createCopyUrlContextMenu()
            hyperlink.isUnderline = true
            hyperlink.onAction = EventHandler {
                SwingUtilities.invokeLater {
                    if (currentFilm != null) {
                        try {
                            UrlHyperlinkAction.openURL(null, currentFilm!!.websiteLink)
                        } catch (ex: URISyntaxException) {
                            ex.printStackTrace()
                        }
                    }
                }
            }
            migPane.add(hyperlink, CC().cell(1, 11))

            lblDescription.isWrapText = true
            lblDescription.prefRowCount = 4
            lblDescription.isEditable = false
            migPane.add(lblDescription, CC().cell(0, 13).spanX(2).growY().growX().minHeight("60"))
            newFxPanel.scene = Scene(migPane)
        }
    }

    private fun createCopyUrlContextMenu() : ContextMenu {
        val contextMenu = ContextMenu()
        val mi = MenuItem("URL kopieren")
        mi.onAction = EventHandler { SwingUtilities.invokeLater { GuiFunktionen.copyToClipboard(currentFilm!!.websiteLink) } }
        contextMenu.items.add(mi)
        return contextMenu
    }
    
    internal class RightOrientedLabel(label: String?) : Label(label) {
        init {
            alignment = Pos.BASELINE_RIGHT
        }
    }
    
    internal class DisabledCheckBox : CheckBox() {
        init {
            isDisable = true
        }
    }

    companion object {
        private const val serialVersionUID = -890508930316467747L
        private const val FILM_INFO_VISIBLE = "film.information.visible"
        private const val FILM_INFO_LOCATION_X = "film.information.location.x"
        private const val FILM_INFO_LOCATION_Y = "film.information.location.y"
    }

    init {
        type = Type.UTILITY
        title = "Filminformation"
        isResizable = false
        //hardcode size as linux hates pack()
        setSize(325, 520)
        defaultCloseOperation = DISPOSE_ON_CLOSE
        buildLayout()
        updateTextFields()
        restoreLocation()
        val wasVisible = config.getBoolean(FILM_INFO_VISIBLE, false)
        if (wasVisible) {
            isVisible = true
        }
        addWindowListener(object : WindowAdapter() {
            override fun windowOpened(e: WindowEvent) {
                config.setProperty(FILM_INFO_VISIBLE, true)
            }

            override fun windowClosed(e: WindowEvent) {
                config.setProperty(FILM_INFO_VISIBLE, false)
            }
        })

        //addFilmlistLoadListener();
        addComponentListener(object : ComponentAdapter() {
            override fun componentMoved(e: ComponentEvent) {
                if (isVisible) saveLocation()
            }
        })
    }
}