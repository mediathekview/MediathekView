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

package mediathek.tool.sender_icon_cache

import com.formdev.flatlaf.extras.FlatSVGIcon
import java.awt.Dimension
import java.awt.Rectangle
import java.awt.image.BufferedImage
import javax.swing.Icon
import kotlin.math.max
import kotlin.math.min
import kotlin.math.roundToInt

object SenderIconRenderUtil {
    fun calculateFittedDimensionAllowUpscale(
        imageSize: Dimension,
        boundary: Dimension,
    ): Dimension {
        val iw = max(1, imageSize.width)
        val ih = max(1, imageSize.height)
        val bw = max(1, boundary.width)
        val bh = max(1, boundary.height)

        val scale = min(bw.toDouble() / iw, bh.toDouble() / ih)
        val width = max(1, (iw * scale).roundToInt())
        val height = max(1, (ih * scale).roundToInt())
        return Dimension(width, height)
    }

    fun deriveSvgFittedToOpaqueBounds(
        svg: FlatSVGIcon,
        targetBounds: Dimension,
    ): Icon {
        val fitted = calculateFittedDimensionAllowUpscale(
            Dimension(svg.iconWidth, svg.iconHeight),
            targetBounds,
        )

        val renderWidth = max(1, fitted.width * 4)
        val renderHeight = max(1, fitted.height * 4)
        val probe = svg.derive(renderWidth, renderHeight)

        val rendered = BufferedImage(renderWidth, renderHeight, BufferedImage.TYPE_INT_ARGB)
        val g2 = rendered.createGraphics()
        try {
            probe.paintIcon(null, g2, 0, 0)
        } finally {
            g2.dispose()
        }

        val opaqueBounds = opaqueBounds(rendered) ?: return svg.derive(fitted.width, fitted.height)

        val opaqueWidth = max(1.0, opaqueBounds.width.toDouble())
        val opaqueHeight = max(1.0, opaqueBounds.height.toDouble())
        val boostW = targetBounds.width / (opaqueWidth / 4.0)
        val boostH = targetBounds.height / (opaqueHeight / 4.0)
        val boost = min(boostW, boostH).coerceIn(1.0, 3.0)

        val outWidth = max(1, (fitted.width * boost).roundToInt())
        val outHeight = max(1, (fitted.height * boost).roundToInt())
        return svg.derive(outWidth, outHeight)
    }

    private fun opaqueBounds(image: BufferedImage): Rectangle? {
        var minX = image.width
        var minY = image.height
        var maxX = -1
        var maxY = -1

        for (y in 0..<image.height) {
            for (x in 0..<image.width) {
                val alpha = (image.getRGB(x, y) ushr 24) and 0xFF
                if (alpha != 0) {
                    if (x < minX) minX = x
                    if (y < minY) minY = y
                    if (x > maxX) maxX = x
                    if (y > maxY) maxY = y
                }
            }
        }

        if (maxX < minX || maxY < minY) {
            return null
        }
        return Rectangle(minX, minY, (maxX - minX) + 1, (maxY - minY) + 1)
    }
}
