/*
 * Copyright (c) 2024-2026 derreisende77.
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

package mediathek.gui.filmInformation

import com.formdev.flatlaf.extras.FlatSVGIcon
import com.formdev.flatlaf.util.ScaledImageIcon
import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.Konstanten
import mediathek.daten.DatenFilm
import mediathek.gui.actions.UrlHyperlinkAction
import mediathek.gui.expiration.SenderExpirationService
import mediathek.mainwindow.MediathekGui
import mediathek.tool.*
import mediathek.tool.datum.DateUtil
import mediathek.tool.sender_icon_cache.MVSenderIconCache
import mediathek.tool.sender_icon_cache.SenderIconRenderUtil
import org.apache.commons.configuration2.sync.LockMode
import java.awt.*
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import java.awt.image.BufferedImage
import java.net.URI
import java.util.*
import javax.swing.*
import kotlin.coroutines.cancellation.CancellationException

class FilmInfoDialog(owner: Window) : JDialog(owner) {
    private val form = FilmInfoFormPanel()
    private val popupMenu = JPopupMenu()
    private var dialogJob = SupervisorJob()
    private var uiScope = CoroutineScope(dialogJob + Dispatchers.Swing)
    private var currentFilmOptional: Optional<DatenFilm> = Optional.empty()
    private var availableUntilJob: Job? = null

    init {
        type = Type.UTILITY
        title = "Filminformation"
        maximumSize = Dimension(500, 800)
        minimumSize = Dimension(320, 240)
        preferredSize = Dimension(400, 500)
        defaultCloseOperation = DISPOSE_ON_CLOSE
        contentPane = form
        pack()
        setLocationRelativeTo(owner)

        setupDescriptionPopupMenu()
        setupHyperlink()
        updateTextFields()
        restoreLocation()

        isVisible = ApplicationConfiguration.getConfiguration()
            .getBoolean(ApplicationConfiguration.FilmInfoDialog.VISIBLE, false)
        setupListeners()
    }

    private fun setupDescriptionPopupMenu() {
        val item = JMenuItem("Auswahl kopieren")
        item.addActionListener {
            val selected = form.lblDescription.selectionEnd - form.lblDescription.selectionStart > 0
            if (!selected) {
                JOptionPane.showMessageDialog(
                    this,
                    "Kein Text markiert!",
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.ERROR_MESSAGE
                )
            } else {
                GuiFunktionen.copyToClipboard(form.lblDescription.selectedText)
            }
        }
        popupMenu.add(item)
        form.lblDescription.componentPopupMenu = popupMenu
    }

    private fun setupListeners() {
        addWindowListener(object : WindowAdapter() {
            override fun windowOpened(e: WindowEvent) {
                ApplicationConfiguration.getConfiguration()
                    .setProperty(ApplicationConfiguration.FilmInfoDialog.VISIBLE, true)
            }

            override fun windowClosed(e: WindowEvent) {
                ApplicationConfiguration.getConfiguration()
                    .setProperty(ApplicationConfiguration.FilmInfoDialog.VISIBLE, false)
            }
        })
        addComponentListener(object : ComponentAdapter() {
            override fun componentResized(e: ComponentEvent) {
                saveLocation()
            }

            override fun componentMoved(e: ComponentEvent) {
                if (isVisible) {
                    saveLocation()
                }
            }
        })
    }

    private fun restoreLocation() {
        val config = ApplicationConfiguration.getConfiguration()
        config.lock(LockMode.READ)
        try {
            val newLocation = Point(
                config.getInt(ApplicationConfiguration.FilmInfoDialog.X),
                config.getInt(ApplicationConfiguration.FilmInfoDialog.Y)
            )
            location = newLocation

            val w = config.getInt(ApplicationConfiguration.FilmInfoDialog.WIDTH)
            val h = config.getInt(ApplicationConfiguration.FilmInfoDialog.HEIGHT)
            if (w > 50 && h > 50) {
                size = Dimension(w, h)
            }
        } catch (_: NoSuchElementException) {
        } finally {
            config.unlock(LockMode.READ)
        }
    }

    private fun saveLocation() {
        if (!isVisible) {
            return
        }
        val config = ApplicationConfiguration.getConfiguration()
        config.lock(LockMode.WRITE)
        try {
            val location = locationOnScreen
            config.setProperty(ApplicationConfiguration.FilmInfoDialog.X, location.x)
            config.setProperty(ApplicationConfiguration.FilmInfoDialog.Y, location.y)
            config.setProperty(ApplicationConfiguration.FilmInfoDialog.WIDTH, width)
            config.setProperty(ApplicationConfiguration.FilmInfoDialog.HEIGHT, height)
        } finally {
            config.unlock(LockMode.WRITE)
        }
    }

    private fun setupHyperlink() {
        form.hyperlink.addActionListener {
            val toolTipText = form.hyperlink.toolTipText
            if (!toolTipText.isNullOrEmpty()) {
                if (Desktop.isDesktopSupported()) {
                    val desktop = Desktop.getDesktop()
                    if (desktop.isSupported(Desktop.Action.BROWSE)) {
                        try {
                            desktop.browse(URI(toolTipText))
                        } catch (ex: Exception) {
                            SwingErrorDialog.showExceptionMessage(
                                MediathekGui.ui(),
                                "Es trat ein Fehler beim Öffnen des Links auf.\nSollte dies häufiger auftreten kontaktieren Sie bitte das Entwicklerteam.",
                                ex
                            )
                        }
                    } else {
                        UrlHyperlinkAction.openURL(toolTipText)
                    }
                } else {
                    UrlHyperlinkAction.openURL(toolTipText)
                }
            }
        }
    }

    fun showInfo() {
        updateTextFields()

        if (!isVisible) {
            isVisible = true
        }
    }

    fun updateCurrentFilm(film: DatenFilm?) {
        currentFilmOptional = if (film == null) {
            Optional.empty()
        } else {
            Optional.of(film)
        }

        if (isVisible) {
            updateTextFields()
        }
    }

    private fun clearControls() {
        availableUntilJob?.cancel()
        availableUntilJob = null
        form.lblSender.text = ""
        form.lblSender.icon = null
        form.lblThema.text = ""
        form.lblTitel.text = ""
        form.lblDate.text = ""
        form.lblUhrzeit.text = ""
        form.lblDuration.text = ""
        form.lblSize.text = ""
        form.lblGeo.text = ""
        form.cbHq.isSelected = false
        form.cbSubtitle.isSelected = false
        form.lblAbo.text = ""
        form.hyperlink.toolTipText = ""
        form.hyperlink.isEnabled = false
        form.hyperlink.componentPopupMenu = null
        form.lblDescription.text = ""
        form.lblSeason.text = ""
        form.lblEpisode.text = ""
        form.lblAvailableUntil.text = ""
    }

    private fun updateDurationLabel(film: DatenFilm) {
        val duration = film.filmLengthAsString
        try {
            form.lblDuration.text = DurationFormatter.from(duration).toDisplayText("")
        } catch (_: IllegalArgumentException) {
            form.lblDuration.text = duration
        }
    }

    private fun updateGeoLabel(film: DatenFilm) {
        form.lblGeo.text = if (film.hasCountries()) {
            film.countriesAsString
        } else {
            ""
        }
    }

    private fun updateTextFields() {
        currentFilmOptional.ifPresentOrElse({ currentFilm ->
            MVSenderIconCache[currentFilm.sender].ifPresentOrElse({ icon ->
                form.lblSender.text = ""
                val renderedIcon: Icon = if (icon is FlatSVGIcon) {
                    createSvgIconCroppedToHeight(icon, DEFAULT_SENDER_DIMENSION.height)
                } else {
                    val imageDim = Dimension(icon.iconWidth, icon.iconHeight)
                    val destDim = SenderIconRenderUtil.calculateFittedDimensionAllowUpscale(
                        imageDim,
                        DEFAULT_SENDER_HEIGHT_BOUNDARY
                    )
                    ScaledImageIcon(icon, destDim.width, destDim.height)
                }
                form.lblSender.icon = renderedIcon
            }, {
                form.lblSender.text = currentFilm.sender
                form.lblSender.icon = null
            })

            form.lblThema.text = currentFilm.thema
            form.lblTitel.text = currentFilm.title
            form.lblDate.text = currentFilm.sendeDatum
            form.lblUhrzeit.text = currentFilm.sendeZeit
            updateDurationLabel(currentFilm)
            form.lblSize.text = currentFilm.fileSize.toString()
            updateGeoLabel(currentFilm)
            form.cbHq.isSelected = currentFilm.isHighQuality
            form.cbSubtitle.isSelected = currentFilm.hasSubtitle()

            currentFilm.aboOptional.ifPresentOrElse({ abo -> form.lblAbo.text = abo.name }, { form.lblAbo.text = "" })

            prepareHyperlink(currentFilm.websiteUrl)

            form.lblDescription.text = currentFilm.description.trim()
            SwingUtilities.invokeLater { form.descScrollPane.verticalScrollBar.value = 0 }

            form.lblSeason.text = if (currentFilm.season != 0) currentFilm.season.toString() else ""
            form.lblEpisode.text = if (currentFilm.episode != 0) currentFilm.episode.toString() else ""

            updateAvailableUntil(currentFilm)
        }, this::clearControls)
    }

    private fun updateAvailableUntil(film: DatenFilm) {
        availableUntilJob?.cancel()
        availableUntilJob = null
        ensureCoroutineScope()

        val availableUntil = film.availableUntil
        if (availableUntil != null) {
            form.lblAvailableUntil.text = DateUtil.FORMATTER.format(availableUntil)
            return
        }

        availableUntilJob = uiScope.launch {
            form.lblAvailableUntil.text = "Suche..."
            try {
                val result = withContext(Dispatchers.IO) {
                    SenderExpirationService.fetchExpiryDate(film.sender, film.websiteUrl)
                }
                ensureActive()
                film.availableUntil = result
                form.lblAvailableUntil.text = result?.let(DateUtil.FORMATTER::format) ?: ""
            } catch (_: CancellationException) {
            } catch (_: Exception) {
            }
        }
    }

    private fun prepareHyperlink(url: String) {
        form.hyperlink.isEnabled = true
        form.hyperlink.toolTipText = url
        form.hyperlink.isClicked = false
        val urlPopupMenu = JPopupMenu()
        urlPopupMenu.add(CopyToClipboardAction(url))
        form.hyperlink.componentPopupMenu = urlPopupMenu
    }

    private fun ensureCoroutineScope() {
        if (!dialogJob.isActive) {
            dialogJob = SupervisorJob()
            uiScope = CoroutineScope(dialogJob + Dispatchers.Swing)
        }
    }

    override fun dispose() {
        availableUntilJob?.cancel()
        dialogJob.cancel()
        super.dispose()
    }

    private class CroppedDelegateIcon(
        private val delegate: Icon,
        private val offsetX: Int,
        private val offsetY: Int,
        private val width: Int,
        private val height: Int
    ) : Icon {
        override fun paintIcon(c: Component?, g: Graphics, x: Int, y: Int) {
            delegate.paintIcon(c, g, x - offsetX, y - offsetY)
        }

        override fun getIconWidth(): Int = width

        override fun getIconHeight(): Int = height
    }

    companion object {
        private val DEFAULT_SENDER_DIMENSION = Dimension(48, 48)
        private val DEFAULT_SENDER_HEIGHT_BOUNDARY = Dimension(4096, DEFAULT_SENDER_DIMENSION.height)

        private fun createSvgIconCroppedToHeight(svg: FlatSVGIcon, targetHeight: Int): Icon {
            val baseWidth = maxOf(1, svg.iconWidth)
            val baseHeight = maxOf(1, svg.iconHeight)
            val outHeight = maxOf(1, targetHeight)

            val scaleToTarget = outHeight / baseHeight.toFloat()
            val probeScale = maxOf(scaleToTarget * 4.0f, 0.05f)
            val probe = svg.derive(probeScale)
            val probeImage = BufferedImage(
                maxOf(1, probe.iconWidth),
                maxOf(1, probe.iconHeight),
                BufferedImage.TYPE_INT_ARGB
            )
            val pg: Graphics2D = probeImage.createGraphics()
            try {
                probe.paintIcon(null, pg, 0, 0)
            } finally {
                pg.dispose()
            }

            val bounds = opaqueBounds(probeImage)
            if (bounds == null) {
                val fallbackDim = SenderIconRenderUtil.calculateFittedDimensionAllowUpscale(
                    Dimension(baseWidth, baseHeight),
                    Dimension(DEFAULT_SENDER_HEIGHT_BOUNDARY.width, outHeight)
                )
                return svg.derive(fallbackDim.width, fallbackDim.height)
            }

            val invScale = 1.0 / probeScale
            val cropX = bounds.x * invScale
            val cropY = bounds.y * invScale
            val cropW = maxOf(1.0, bounds.width * invScale)
            val cropH = maxOf(1.0, bounds.height * invScale)

            val scale = outHeight / cropH
            val derivedWidth = maxOf(1, (baseWidth * scale).toIntRounded())
            val derivedHeight = maxOf(1, (baseHeight * scale).toIntRounded())
            val derived = svg.derive(derivedWidth, derivedHeight)

            val offsetX = maxOf(0, (cropX * scale).toIntRounded())
            val offsetY = maxOf(0, (cropY * scale).toIntRounded())
            val outWidth = maxOf(1, (cropW * scale).toIntRounded())

            return CroppedDelegateIcon(derived, offsetX, offsetY, outWidth, outHeight)
        }

        private fun opaqueBounds(image: BufferedImage): Rectangle? {
            var minX = image.width
            var minY = image.height
            var maxX = -1
            var maxY = -1

            for (y in 0 until image.height) {
                for (x in 0 until image.width) {
                    val alpha = image.getRGB(x, y) ushr 24 and 0xFF
                    if (alpha != 0) {
                        if (x < minX) minX = x
                        if (y < minY) minY = y
                        if (x > maxX) maxX = x
                        if (y > maxY) maxY = y
                    }
                }
            }

            return if (maxX < minX || maxY < minY) {
                null
            } else {
                Rectangle(minX, minY, maxX - minX + 1, maxY - minY + 1)
            }
        }

        private fun Double.toIntRounded(): Int = Math.round(this).toInt()
    }
}
