package mediathek.gui.tabs.tab_film

import mediathek.config.Konstanten
import mediathek.controller.history.SeenHistoryController
import mediathek.daten.DatenFilm
import mediathek.daten.FilmResolution
import mediathek.mainwindow.MediathekGui
import mediathek.tool.SwingErrorDialog
import mediathek.tool.http.MVHttpClient
import okhttp3.FormBody
import okhttp3.HttpUrl
import okhttp3.HttpUrl.Companion.toHttpUrl
import okhttp3.Request
import okhttp3.RequestBody
import org.apache.logging.log4j.LogManager
import java.net.ConnectException
import java.net.SocketTimeoutException
import java.util.concurrent.TimeUnit
import javax.swing.JMenu
import javax.swing.JMenuItem
import javax.swing.JOptionPane
import javax.swing.JPopupMenu

class JDownloadHelper {
    private val historyController = SeenHistoryController()

    private fun downloadUrl(url: HttpUrl, film: DatenFilm) {
        val formBody: RequestBody = FormBody.Builder()
            .add("urls", url.toString())
            .build()
        val request = Request.Builder()
            .url("http://127.0.0.1:9666/flash/add")
            .header("Host", "mediathekview.de")
            .post(formBody)
            .build()
        try {
            val builder = MVHttpClient.getInstance().httpClient.newBuilder()
            builder.connectTimeout(125, TimeUnit.MILLISECONDS)
            val client = builder.build()
            client.newCall(request).execute().use {
                if (it.isSuccessful)
                    historyController.markSeen(film)
            }
        }
        catch (e: ConnectException) {
            showErrorMessage()
        }
        catch (e: SocketTimeoutException) {
            showErrorMessage()
        }
        catch (e: Exception) {
            logger.error("downloadUrl", e)
            SwingErrorDialog.showExceptionMessage(MediathekGui.ui(),
                                                  "<html>Die URL konnte nicht mit JDownloader geladen werden.<br>" +
                                                          "Bitte wenden Sie sich bei Bedarf an das Forum.</html>", e)
        }
    }

    private fun showErrorMessage() {
        JOptionPane.showMessageDialog(
            MediathekGui.ui(),
            "Verbindung mit JDownloader nicht möglich.\n" +
                    "Bitte stellen Sie sicher, dass JDownloader gestartet wurde.",
            Konstanten.PROGRAMMNAME,
            JOptionPane.ERROR_MESSAGE)
    }

    fun installContextMenu(film: DatenFilm, jPopupMenu: JPopupMenu) {
        jPopupMenu.addSeparator()

        val mJD = JMenu("Mit JDownloader herunterladen")
        val uNormal = film.urlNormalQuality.toHttpUrl()
        val uHq = film.getUrlFuerAufloesung(FilmResolution.Enum.HIGH_QUALITY).toHttpUrl()
        val uLow = film.getUrlFuerAufloesung(FilmResolution.Enum.LOW).toHttpUrl()

        if (film.isHighQuality) {
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

        val miWebsiteToJd = JMenuItem("Webseiten-URL an JDownloader übergeben")
        miWebsiteToJd.addActionListener {
            try {
                val webSiteUrl = film.websiteUrl.toHttpUrl()
                downloadUrl(webSiteUrl, film)
            }
            catch (e: IllegalArgumentException) {
                logger.error("Illegal Website URL found: {}", film.websiteUrl)
            }
        }
        jPopupMenu.add(miWebsiteToJd)
        if (film.websiteUrl.isBlank()) {
            miWebsiteToJd.isEnabled = false
        }
    }

    companion object {
        private val logger = LogManager.getLogger()
    }
}
