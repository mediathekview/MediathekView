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
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenFilm
import mediathek.swing.CompoundIcon
import mediathek.swing.IconUtils
import mediathek.tool.SVGIconUtilities
import mediathek.tool.models.FilmColumn
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import org.kordamp.ikonli.swing.FontIcon
import java.awt.Color
import javax.swing.Icon
import javax.swing.JTable

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

    protected fun drawGeolocationIcons(film: DatenFilm, isSelected: Boolean) {
        horizontalAlignment = CENTER
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
        horizontalAlignment = LEADING
    }

    /** Shows GEO, HQ, and subtitle indicators in the title when their dedicated columns are hidden. */
    protected fun setIndicatorIcons(table: JTable, datenFilm: DatenFilm, isSelected: Boolean) {
        val visibility = getIndicatorColumnVisibility(table)
        setIndicatorIcons(
            datenFilm,
            isSelected,
            visibility.geoColumnHidden,
            visibility.hqColumnHidden,
            visibility.utColumnHidden,
        )
    }

    private fun setIndicatorIcons(
        datenFilm: DatenFilm,
        isSelected: Boolean,
        geoColumnHidden: Boolean,
        hqColumnHidden: Boolean,
        utColumnHidden: Boolean,
    ) {
        val iconList = mutableListOf<Icon>()
        if (geoColumnHidden && !filmIsCountryUnlocked(datenFilm)) {
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

        icon = when (iconList.size) {
            0 -> null
            1 -> iconList.first()
            else -> CompoundIcon(CompoundIcon.Axis.X_AXIS, 3, *iconList.toTypedArray())
        }

        horizontalTextPosition = if (ApplicationConfiguration.getInstance().listIconPositionRight) {
            LEADING
        } else {
            TRAILING
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
        val geoColumnHidden: Boolean,
        val hqColumnHidden: Boolean,
        val utColumnHidden: Boolean,
    )

    private class IndicatorVisibilityCache(
        private val table: JTable,
    ) : TableColumnModelListener {
        val columnModel: TableColumnModel = table.columnModel
        private var dirty = true
        private var visibility = IndicatorColumnVisibility(
            geoColumnHidden = true,
            hqColumnHidden = true,
            utColumnHidden = true,
        )

        init {
            columnModel.addColumnModelListener(this)
        }

        fun get(): IndicatorColumnVisibility {
            if (dirty) {
                visibility = IndicatorColumnVisibility(
                    geoColumnHidden = isColumnHidden(table, FilmColumn.GEO),
                    hqColumnHidden = isColumnHidden(table, FilmColumn.HIGH_QUALITY),
                    utColumnHidden = isColumnHidden(table, FilmColumn.SUBTITLE),
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
        private const val INDICATOR_VISIBILITY_CACHE_KEY = "mv.renderer.indicatorVisibilityCache"

        private fun isColumnHidden(table: JTable, filmColumn: FilmColumn): Boolean {
            for (viewIndex in 0 until table.columnModel.columnCount) {
                val column = table.columnModel.getColumn(viewIndex)
                if (column.modelIndex == filmColumn.index) {
                    return column.width == 0
                }
            }
            return true
        }
    }
}
