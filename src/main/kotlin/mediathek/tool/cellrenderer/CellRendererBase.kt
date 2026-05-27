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
import com.formdev.flatlaf.util.ScaledImageIcon
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.sender_icon_cache.MVSenderIconCache
import mediathek.tool.sender_icon_cache.SenderIconRenderUtil
import org.apache.commons.lang3.SystemUtils
import java.awt.Dimension
import java.util.*
import javax.swing.Icon
import javax.swing.ImageIcon
import javax.swing.JTable
import javax.swing.JTextArea
import javax.swing.SwingConstants
import javax.swing.UIManager
import javax.swing.table.DefaultTableCellRenderer
import kotlin.math.max
import kotlin.math.roundToInt

/**
 * Base class for all cell renderer.
 */
open class CellRendererBase : DefaultTableCellRenderer() {
    /**
     * Stores the pre-scaled icon for a specific sender and a specific cell dimension.
     * Will get evicted automatically in order to not store too many useless objects.
     */
    private val senderCellIconCache = SelfEvictingSenderIconCache()

    protected fun createWrappedTextArea(content: String, useLabelFont: Boolean = false): JTextArea =
        JTextArea().apply {
            lineWrap = true
            wrapStyleWord = true
            text = content
            foreground = this@CellRendererBase.foreground
            background = this@CellRendererBase.background

            if (useLabelFont) {
                val fontSize = font.size2D
                font = UIManager.getFont("Label.font").deriveFont(fontSize)
            }
        }

    protected fun valueText(value: Any?): String = requireNotNull(value).toString()

    protected class RendererIconPair(
        private val normal: Icon,
        private val selected: Icon,
    ) {
        fun icon(isSelected: Boolean): Icon = if (isSelected) selected else normal
    }

    protected fun rendererIconPair(normal: Icon, selected: Icon): RendererIconPair =
        RendererIconPair(normal, selected)

    protected fun setSelectedIconAndToolTip(isSelected: Boolean, icons: RendererIconPair, tooltip: String) {
        toolTipText = tooltip
        icon = icons.icon(isSelected)
    }

    protected fun setSenderIcon(sender: String, targetDim: Dimension, isSelected: Boolean) {
        val normalizedSender = normalizeSender(sender)

        // make target dims for icon slightly smaller
        if (SystemUtils.IS_OS_LINUX) {
            targetDim.width -= 4
            targetDim.height -= 4
        }

        val useLocalSenderIcons = ApplicationConfiguration.getConfiguration()
            .getBoolean(MVSenderIconCache.CONFIG_USE_LOCAL_SENDER_ICONS, false)
        val key = SenderCacheKey(sender, targetDim, useLocalSenderIcons, isSelected)
        var cachedIcon = senderCellIconCache[key]
        if (cachedIcon == null) {
            val sourceIcon = MVSenderIconCache[sender].orElse(null)
            if (sourceIcon != null) {
                var renderedIcon = renderSenderIcon(sourceIcon, targetDim, normalizedSender)
                if (isSelected && shouldApplySelectionContrast(normalizedSender)) {
                    renderedIcon = SelectionContrastIcon(renderedIcon)
                }
                cachedIcon = renderedIcon
                senderCellIconCache[key] = renderedIcon
            }
        }

        if (cachedIcon != null) {
            text = ""
            icon = cachedIcon
        }
        verticalAlignment = SwingConstants.CENTER
        horizontalAlignment = SwingConstants.CENTER
    }

    private fun renderSenderIcon(icon: ImageIcon, targetDim: Dimension, normalizedSender: String): Icon =
        if (icon is FlatSVGIcon) {
            val autoFitted = SenderIconRenderUtil.deriveSvgFittedToOpaqueBounds(icon, targetDim)
            if (requiresExtraSvgBoost(normalizedSender)) {
                val boostedWidth = max(1, (autoFitted.iconWidth * SPECIAL_SVG_BOOST).roundToInt())
                val boostedHeight = max(1, (autoFitted.iconHeight * SPECIAL_SVG_BOOST).roundToInt())
                icon.derive(boostedWidth, boostedHeight)
            } else {
                autoFitted
            }
        } else {
            val destDim = SenderIconRenderUtil.calculateFittedDimensionAllowUpscale(
                Dimension(icon.iconWidth, icon.iconHeight),
                targetDim,
            )
            ScaledImageIcon(icon, destDim.width, destDim.height)
        }

    /**
     * Calculate the dimensions of a table cell for the sender icon.
     *
     * @param table where it will be displayed.
     * @param row the used row index.
     * @param column the used view column index.
     * @return the calculated dimension of the available table cell.
     */
    protected fun getSenderCellDimension(table: JTable, row: Int, column: Int): Dimension =
        Dimension().apply {
            height = table.getRowHeight(row)
            width = table.columnModel.getColumn(column).width
            height -= 4
            width -= 4
        }

    private companion object {
        private const val SPECIAL_SVG_BOOST = 1.35
        private val EXTRA_SVG_BOOST_SENDERS = setOf(
            "tagesschau24",
            "radio bremen",
            "radio bremen tv",
        )
        private val SELECTION_CONTRAST_EXCLUDE_SENDERS = setOf(
            "orf",
            "one",
            "srf",
            "srf.podcast",
            "zdf",
            "zdf-tivi",
            "3sat",
            "ard-alpha",
            "ard alpha",
            "funk.net",
            "funk",
        )

        private fun normalizeSender(sender: String): String = sender.lowercase(Locale.ROOT)

        private fun requiresExtraSvgBoost(normalizedSender: String): Boolean =
            normalizedSender in EXTRA_SVG_BOOST_SENDERS

        private fun shouldApplySelectionContrast(normalizedSender: String): Boolean =
            !normalizedSender.startsWith("arte") && normalizedSender !in SELECTION_CONTRAST_EXCLUDE_SENDERS
    }
}
