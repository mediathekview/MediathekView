package mediathek.gui.tabs.tab_film

import mediathek.config.Konstanten
import mediathek.config.application.ApplicationConfiguration
import mediathek.controller.history.SeenHistoryController
import mediathek.daten.DatenFilm
import mediathek.daten.FilmResolution
import mediathek.mainwindow.MediathekGui
import mediathek.tool.SwingErrorDialog
import mediathek.tool.http.MVHttpClient
import okhttp3.Credentials
import okhttp3.HttpUrl
import okhttp3.HttpUrl.Companion.toHttpUrl
import okhttp3.MediaType.Companion.toMediaType
import okhttp3.Request
import okhttp3.RequestBody.Companion.toRequestBody
import org.apache.logging.log4j.LogManager
import javax.swing.JMenu
import javax.swing.JMenuItem
import javax.swing.JOptionPane
import javax.swing.JPopupMenu
import kotlin.time.Duration.Companion.milliseconds
import kotlin.time.toJavaDuration

class PyLoadHelper {
    private val historyController = SeenHistoryController()

    private fun downloadUrl(url: HttpUrl, film: DatenFilm) {
        val config = ApplicationConfiguration.getInstance()
        val baseUrl = config.pyLoadUrl.trimEnd('/')
        val user = config.pyLoadUser
        val pass = config.pyLoadPassword

        val apiUrl = try {
            "$baseUrl/api/add_package".toHttpUrl()
        } catch (e: IllegalArgumentException) {
            logger.error("Invalid pyLoad URL in config: {}", baseUrl)
            SwingErrorDialog.showExceptionMessage(
                MediathekGui.ui(),
                "<html>Die konfigurierte pyLoad-URL ist ungültig:<br><br><b>$baseUrl</b></html>",
                e
            )
            return
        }

        val jsonBody = """
        {
          "name": "${film.title}",
          "dest": 1,
          "links": ["$url"]
        }
    """.trimIndent()

        val requestBody = jsonBody.toRequestBody("application/json".toMediaType())

        val request = Request.Builder()
            .url(apiUrl)
            .header(
                "Authorization",
                Credentials.basic(user, pass)
            )
            .post(requestBody)
            .build()

        try {
            val client = MVHttpClient
                .httpClient
                .newBuilder()
                .connectTimeout(500.milliseconds.toJavaDuration())
                .build()

            client.newCall(request).execute().use {
                if (it.isSuccessful) {
                    historyController.markSeen(film)
                } else {
                    throw RuntimeException("pyLoad HTTP ${it.code}")
                }
            }
        }
        catch (e: Exception) {
            logger.error("pyLoad downloadUrl", e)
            showErrorMessage()
        }
    }

    private fun showErrorMessage() {
        JOptionPane.showMessageDialog(
            MediathekGui.ui(),
            "Verbindung mit pyLoad nicht möglich.\n" +
                    "Bitte stellen Sie sicher, dass pyLoad gestartet wurde und die in den Einstellungen hinterlegten Daten (URL, Benutzer und Passwort) korrekt sind.",
            Konstanten.PROGRAMMNAME,
            JOptionPane.ERROR_MESSAGE)
    }

    fun installContextMenu(film: DatenFilm, jPopupMenu: JPopupMenu) {
        val pyLoadUrl = ApplicationConfiguration.getInstance().pyLoadUrl
        val pyLoadConfigured = pyLoadUrl.isNotBlank()

        val mJD = JMenu("Mit pyLoad herunterladen")
        mJD.isEnabled = pyLoadConfigured
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
    }

    companion object {
        private val logger = LogManager.getLogger()
    }
}
