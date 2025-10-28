/*
 * Copyright (c) 2025 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.tabs.tab_livestreams

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.SerializationFeature
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule
import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.Konstanten
import mediathek.gui.actions.UrlHyperlinkAction
import mediathek.gui.tabs.tab_livestreams.services.ShowService
import mediathek.gui.tabs.tab_livestreams.services.StreamService
import mediathek.mainwindow.MediathekGui
import mediathek.swing.OverlayPanel
import mediathek.tool.GermanStringSorter
import mediathek.tool.GuiFunktionenProgramme
import mediathek.tool.http.MVHttpClient
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger
import retrofit2.Retrofit
import retrofit2.converter.jackson.JacksonConverterFactory
import java.awt.BorderLayout
import java.awt.Desktop
import java.awt.Rectangle
import java.awt.event.*
import java.net.URI
import java.time.Instant
import java.util.concurrent.TimeUnit
import javax.swing.*

class LivestreamPanel : JPanel(BorderLayout()), CoroutineScope by MainScope() {

    private val listModel = LivestreamListModel()
    private val list = JList(listModel)
    private val streamService: StreamService
    private val showService: ShowService
    private val refreshTimer =
        Timer(TimeUnit.MILLISECONDS.convert(4, TimeUnit.SECONDS).toInt()) { checkForExpiredShows() } // alle 10s prüfen
    private val overlay = OverlayPanel("Livestreams konnten nicht geladen werden")


    init {
        val container = JLayeredPane().apply {
            layout = OverlayLayout(this)
            add(overlay)
            add(JScrollPane(list))
        }
        add(container, BorderLayout.CENTER)
        overlay.isVisible = false

        setupList()

        // load livestreams when list is empty and panel becomes visible
        this.addComponentListener(object : ComponentAdapter() {
            override fun componentShown(e: ComponentEvent?) {
                if (listModel.size == 0) {
                    SwingUtilities.invokeLater {
                        loadLivestreams()
                    }
                }
            }
        })

        val mapper = ObjectMapper().apply {
            registerModule(JavaTimeModule())
            disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS)
        }

        val client = MVHttpClient.getInstance().httpClient
        streamService = Retrofit.Builder()
            .baseUrl(Konstanten.ZAPP_API_URL)
            .addConverterFactory(JacksonConverterFactory.create(mapper))
            .client(client)
            .build()
            .create(StreamService::class.java)

        showService = Retrofit.Builder()
            .baseUrl(Konstanten.ZAPP_API_URL)
            .addConverterFactory(JacksonConverterFactory.create(mapper))
            .client(client)
            .build()
            .create(ShowService::class.java)

        refreshTimer.start()
    }

    private fun setupList() {
        list.cellRenderer = LivestreamRenderer()
        list.selectionMode = ListSelectionModel.SINGLE_SELECTION
        list.addMouseListener(object : MouseAdapter() {
            override fun mouseClicked(e: MouseEvent) {
                if (e.clickCount == 2) {
                    val selected = list.selectedValue ?: return
                    if (!SystemUtils.IS_OS_MAC_OSX) {
                        try {
                            //windows, linux
                            val vlcPath = GuiFunktionenProgramme.findExecutableOnPath("vlc")
                            val pb = ProcessBuilder(vlcPath.toAbsolutePath().toString(), selected.streamUrl)
                            pb.start()
                        }
                        catch (_: IllegalStateException) {
                            JOptionPane.showMessageDialog(MediathekGui.ui(),
                                "<html>Es konnte kein VLC auf dem System gefunden werden.<br/>" +
                                        "Es wird versucht, den Stream über den Browser zu öffnen.</html>")
                            UrlHyperlinkAction.openURL(selected.streamUrl)
                        }
                    }
                    else // macOS can safely open it via java
                        Desktop.getDesktop().browse(URI(selected.streamUrl))
                }
            }
        })
        list.addComponentListener(object : ComponentAdapter() {
            override fun componentResized(e: ComponentEvent?) {
                overlay.setSize(list.width, list.height)
            }
        })

        // Enable tooltip system
        list.toolTipText = ""
        list.addMouseMotionListener(object : MouseMotionAdapter() {
            override fun mouseMoved(e: MouseEvent) {
                val index = list.locationToIndex(e.point)
                if (index < 0) {
                    list.toolTipText = null
                    return
                }

                val bounds = list.getCellBounds(index, index)
                if (bounds == null || !bounds.contains(e.point)) {
                    list.toolTipText = null
                    return
                }

                val entry = list.model.getElementAt(index)

                // Calculate icon area relative to bounds
                val iconSize = LivestreamRenderer.ICON_SIZE
                val iconX = bounds.x + 2
                val iconY = bounds.y + (bounds.height - iconSize) / 2

                val iconArea = Rectangle(iconX, iconY, iconSize, iconSize)

                if (iconArea.contains(e.point)) {
                    list.toolTipText = SenderUtils.sanitizeName(entry.streamName)
                } else {
                    list.toolTipText = null
                }
            }
        })

    }

    private fun loadLivestreams() {
        launch(Dispatchers.IO) {
            try {
                val streams = streamService.getStreams()
                val entries = streams.map { (key, info) ->
                    LivestreamEntry(key, info.name, info.streamUrl)
                }.sortedWith(compareBy(GermanStringSorter.getInstance()) { it.streamName })

                withContext(Dispatchers.Swing) {
                    if (entries.isEmpty()) {
                        overlay.isVisible = true
                    } else {
                        overlay.isVisible = false
                        listModel.setData(entries)
                        loadAllShows()
                    }
                }
            } catch (ex: Exception) {
                LOG.error("Failed to load livestreams", ex)
                withContext(Dispatchers.Swing) {
                    overlay.isVisible = true
                }
            }
        }
    }

    private fun loadAllShows() {
        for (i in 0 until listModel.size) {
            val entry = listModel.getElementAt(i)
            launch { loadShowDetailsForEntry(entry, i) }
        }
    }

    private fun loadShowDetailsForEntry(entry: LivestreamEntry, index: Int) {
        launch(Dispatchers.IO) {
            try {
                val response = showService.getShow(entry.key)
                withContext(Dispatchers.Swing) {
                    entry.show = response.shows.firstOrNull().takeIf { response.error == null }
                    listModel.updateEntry(index, entry)
                }
            } catch (ex: Exception) {
                LOG.error("Failed to load show details", ex)
                withContext(Dispatchers.Swing) {
                    entry.show = null
                    listModel.updateEntry(index, entry)
                }
            }
        }
    }

    companion object {
        private val LOG: Logger = LogManager.getLogger()
    }

    private fun checkForExpiredShows() {
        val now = Instant.now()
        for (i in 0 until listModel.size) {
            val entry = listModel.getElementAt(i)
            val show = entry.show

            if (show != null) {
                if (show.endTime.isBefore(now)) {
                    loadShowDetailsForEntry(entry, i)
                } else if (show.startTime.isBefore(now)) {
                    listModel.updateEntry(i, entry) // Nur laufende Shows aktualisieren
                }
            }
        }
    }
}
