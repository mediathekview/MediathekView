/*
 * Copyright (c) 2026 derreisende77.
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

package mediathek.tool.cellrenderer

import com.formdev.flatlaf.extras.FlatSVGIcon
import mediathek.daten.DatenFilm
import mediathek.swing.CompoundIcon
import mediathek.swing.IconUtils
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.MessageBus
import mediathek.tool.SVGIconUtilities
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import org.kordamp.ikonli.swing.FontIcon
import java.awt.Color
import javax.swing.*
import javax.swing.event.ChangeEvent
import javax.swing.event.ListSelectionEvent
import javax.swing.event.TableColumnModelEvent
import javax.swing.event.TableColumnModelListener
import javax.swing.table.TableColumnModel

/**
 * CellRenderer base class for custom renderers associated with download run state.
 */
open class CellRendererBaseWithStart : CellRendererBase() {
    protected val lockedIcon: FontIcon = IconUtils.of(FontAwesomeSolid.LOCK)
    protected val lockedIconSelected: FontIcon = FontIcon.of(FontAwesomeSolid.LOCK, IconUtils.DEFAULT_SIZE, Color.WHITE)
    protected val unlockedIcon: FontIcon = IconUtils.of(FontAwesomeSolid.LOCK_OPEN)
    protected val unlockedIconSelected: FontIcon =
        FontIcon.of(FontAwesomeSolid.LOCK_OPEN, IconUtils.DEFAULT_SIZE, Color.WHITE)

    private val subtitleIcon: FontIcon = IconUtils.of(FontAwesomeSolid.CLOSED_CAPTIONING)
    private val subtitleIconSelected: FontIcon =
        FontIcon.of(FontAwesomeSolid.CLOSED_CAPTIONING, IconUtils.DEFAULT_SIZE, Color.WHITE)
    private val highQualityIcon: FlatSVGIcon = SVGIconUtilities.createSVGIcon("icons/derreisende77/high-quality.svg")
    private val highQualityIconSelected: FlatSVGIcon =
        SVGIconUtilities.createSVGIcon("icons/derreisende77/high-quality.svg").apply {
            colorFilter = FlatSVGIcon.ColorFilter { Color.WHITE }
        }
    private val liveStreamIcon: FontIcon = IconUtils.of(FontAwesomeSolid.BROADCAST_TOWER)
    private val liveStreamIconSelected: FontIcon =
        FontIcon.of(FontAwesomeSolid.BROADCAST_TOWER, IconUtils.DEFAULT_SIZE, Color.WHITE)
    private val audioDescription: FontIcon = IconUtils.of(FontAwesomeSolid.AUDIO_DESCRIPTION)
    private val audioDescriptionSelected: FontIcon =
        FontIcon.of(FontAwesomeSolid.AUDIO_DESCRIPTION, IconUtils.DEFAULT_SIZE, Color.WHITE)

    init {
        MessageBus.messageBus.subscribe(this)
    }

    protected fun drawGeolocationIcons(film: DatenFilm, isSelected: Boolean) {
        horizontalAlignment = SwingConstants.CENTER
        text = ""
        val curLocation = ApplicationConfiguration.getInstance().geographicLocation
        val lockedForCurrentLocation = film.isGeoBlockedForLocation(curLocation)
        when {
            lockedForCurrentLocation -> {
                toolTipText = if (film.hasCountries()) film.countriesAsString else "Gesperrt für $curLocation"
                icon = if (isSelected) lockedIconSelected else lockedIcon
            }

            !film.hasCountries() -> {
                toolTipText = "Keine Geoinformationen vorhanden"
                icon = if (isSelected) unlockedIconSelected else unlockedIcon
            }

            else -> {
                toolTipText = film.countriesAsString
                icon = if (isSelected) unlockedIconSelected else unlockedIcon
            }
        }
    }

    private fun filmIsCountryUnlocked(film: DatenFilm): Boolean {
        val curLocation = ApplicationConfiguration.getInstance().geographicLocation
        return !film.isGeoBlockedForLocation(curLocation)
    }

    protected fun resetComponent() {
        background = null
        foreground = null
        icon = null
        toolTipText = null
        horizontalAlignment = SwingConstants.LEADING
    }

    /**
     * Show "CC" and/or "HQ" icon(s) when supported by the film.
     *
     * @param datenFilm film information
     * @param isSelected is row selected.
     */
    protected fun setIndicatorIcons(table: JTable, datenFilm: DatenFilm, isSelected: Boolean) {
        val visibility = getIndicatorColumnVisibility(table)
        setIndicatorIcons(datenFilm, isSelected, visibility.hqColumnHidden, visibility.utColumnHidden)
    }

    protected fun setIndicatorIcons(
        datenFilm: DatenFilm,
        isSelected: Boolean,
        hqColumnHidden: Boolean,
        utColumnHidden: Boolean,
    ) {
        val iconList = mutableListOf<Icon>()
        if (!filmIsCountryUnlocked(datenFilm)) {
            iconList += if (isSelected) lockedIconSelected else lockedIcon
        }

        // if HQ column is NOT visible add icon
        if (hqColumnHidden && datenFilm.isHighQuality) {
            iconList += if (isSelected) highQualityIconSelected else highQualityIcon
        }

        if (datenFilm.isAudioVersion) {
            iconList += if (isSelected) audioDescriptionSelected else audioDescription
        }

        // if UT column is NOT visible
        if (utColumnHidden && datenFilm.hasSubtitle()) {
            iconList += if (isSelected) subtitleIconSelected else subtitleIcon
        }

        if (datenFilm.isLivestream) {
            iconList += if (isSelected) liveStreamIconSelected else liveStreamIcon
        }

        icon = if (iconList.size == 1) {
            iconList.first()
        } else {
            CompoundIcon(CompoundIcon.Axis.X_AXIS, 3, *iconList.toTypedArray())
        }

        horizontalTextPosition = if (ApplicationConfiguration.getConfiguration().getBoolean(ICON_POSITION_RIGHT, false)) {
            SwingConstants.LEADING
        } else {
            SwingConstants.TRAILING
        }
    }

    private fun getIndicatorColumnVisibility(table: JTable): IndicatorColumnVisibility {
        var cache = table.getClientProperty(INDICATOR_VISIBILITY_CACHE_KEY) as? IndicatorVisibilityCache
        if (cache == null || cache.columnModel !== table.columnModel) {
            cache = IndicatorVisibilityCache(table)
            table.putClientProperty(INDICATOR_VISIBILITY_CACHE_KEY, cache)
        }
        return cache.get()
    }

    private data class IndicatorColumnVisibility(
        val hqColumnHidden: Boolean,
        val utColumnHidden: Boolean,
    )

    private class IndicatorVisibilityCache(
        private val table: JTable,
    ) : TableColumnModelListener {
        val columnModel: TableColumnModel = table.columnModel
        private var dirty = true
        private var visibility = IndicatorColumnVisibility(hqColumnHidden = true, utColumnHidden = true)

        init {
            columnModel.addColumnModelListener(this)
        }

        fun get(): IndicatorColumnVisibility {
            if (dirty) {
                visibility = IndicatorColumnVisibility(
                    hqColumnHidden = isColumnHidden(table, "HQ"),
                    utColumnHidden = isColumnHidden(table, "UT"),
                )
                dirty = false
            }
            return visibility
        }

        private fun invalidate() {
            dirty = true
        }

        override fun columnAdded(event: TableColumnModelEvent) {
            invalidate()
        }

        override fun columnRemoved(event: TableColumnModelEvent) {
            invalidate()
        }

        override fun columnMoved(event: TableColumnModelEvent) {
            invalidate()
        }

        override fun columnMarginChanged(event: ChangeEvent) {
            invalidate()
        }

        override fun columnSelectionChanged(event: ListSelectionEvent) {
            // selection does not affect visibility
        }
    }

    companion object {
        const val ICON_POSITION_RIGHT: String = "ui.list.iconposition_right"
        private const val INDICATOR_VISIBILITY_CACHE_KEY = "mv.renderer.indicatorVisibilityCache"

        private fun isColumnHidden(table: JTable, identifier: String): Boolean =
            try {
                table.getColumn(identifier).width == 0
            } catch (_: IllegalArgumentException) {
                // If column does not exist in this table model, treat as hidden.
                true
            }
    }
}
