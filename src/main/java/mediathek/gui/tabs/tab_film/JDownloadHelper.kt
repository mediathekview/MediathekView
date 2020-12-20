package mediathek.gui.tabs.tab_film

import javafx.application.Platform
import javafx.scene.control.Alert
import mediathek.config.Konstanten
import mediathek.controller.history.SeenHistoryController
import mediathek.daten.DatenFilm
import mediathek.daten.FilmResolution
import mediathek.tool.javafx.FXErrorDialog
import okhttp3.*
import okhttp3.HttpUrl.Companion.toHttpUrl
import org.apache.logging.log4j.LogManager
import java.net.ConnectException
import java.net.SocketTimeoutException
import java.util.concurrent.TimeUnit
import javax.swing.JMenu
import javax.swing.JMenuItem
import javax.swing.JPopupMenu

class JDownloadHelper {
    private val historyController = SeenHistoryController()
    private fun downloadUrl(url: HttpUrl, film: DatenFilm) {
        val formBody: RequestBody = FormBody.Builder()
                .add("urls", url.toString())
                .build()
        val request = Request.Builder()
                .url("http://127.0.0.1:9666/flash/add")
                .post(formBody)
                .build()
        try {
            val builder = OkHttpClient.Builder()
            builder.connectTimeout(125, TimeUnit.MILLISECONDS)
            val client = builder.build()
            client.newCall(request).execute().use {
                if (it.isSuccessful)
                    historyController.markSeen(film)
            }
        } catch (e: ConnectException) {
            showErrorMessage()
        } catch (e: SocketTimeoutException) {
            showErrorMessage()
        } catch (e: Exception) {
            logger.error("downloadUrl", e)
            Platform.runLater {
                FXErrorDialog.showErrorDialog(Konstanten.PROGRAMMNAME, "Download nicht möglich",
                        "Die URL konnte nicht mit JDownloader geladen werden.\n" +
                                "Bitte wenden Sie sich bei Bedarf an das Forum.", e)
            }
        }
    }

    private fun showErrorMessage() {
        Platform.runLater {
            val alert = Alert(Alert.AlertType.ERROR)
            alert.headerText = "Verbindung mit JDownloader nicht möglich"
            alert.contentText = "Bitte stellen Sie sicher dass JDownloader gestartet ist."
            alert.title = Konstanten.PROGRAMMNAME
            alert.show()
        }
    }

    fun installContextMenu(film: DatenFilm, jPopupMenu: JPopupMenu) {
        jPopupMenu.addSeparator()
        val miText = "Mit JDownloader herunterladen"
        if (film.isPlayList) {
            val miDownloadJD = JMenuItem(miText)
            miDownloadJD.addActionListener {
                val url = film.url.toHttpUrl()
                downloadUrl(url, film)
            }
            jPopupMenu.add(miDownloadJD)
        } else {
            val mJD = JMenu(miText)
            val uNormal = film.url.toHttpUrl()
            val uHq = film.getUrlFuerAufloesung(FilmResolution.Enum.HIGH_QUALITY).toHttpUrl()
            val uLow = film.getUrlFuerAufloesung(FilmResolution.Enum.LOW).toHttpUrl()
            if (uHq !== uNormal) {
                val miHq = JMenuItem("in bester Qualität")
                miHq.addActionListener { downloadUrl(uHq, film) }
                mJD.add(miHq)
            }
            val miNormal = JMenuItem("in normaler Qualität")
            miNormal.addActionListener { downloadUrl(uNormal, film) }
            mJD.add(miNormal)
            if (uLow !== uNormal) {
                val miLow = JMenuItem("in niedriger Qualität")
                miLow.addActionListener { downloadUrl(uLow, film) }
                mJD.add(miLow)
            }
            jPopupMenu.add(mJD)
        }
    }

    companion object {
        private val logger = LogManager.getLogger()
    }
}