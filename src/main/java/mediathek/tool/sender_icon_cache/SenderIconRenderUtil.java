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

package mediathek.tool.sender_icon_cache;

import com.formdev.flatlaf.extras.FlatSVGIcon;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;

public final class SenderIconRenderUtil {
    private SenderIconRenderUtil() {
    }

    public static @NotNull Dimension calculateFittedDimensionAllowUpscale(
            @NotNull Dimension imageSize,
            @NotNull Dimension boundary
    ) {
        int iw = Math.max(1, imageSize.width);
        int ih = Math.max(1, imageSize.height);
        int bw = Math.max(1, boundary.width);
        int bh = Math.max(1, boundary.height);

        double scale = Math.min((double) bw / iw, (double) bh / ih);
        int w = Math.max(1, (int) Math.round(iw * scale));
        int h = Math.max(1, (int) Math.round(ih * scale));
        return new Dimension(w, h);
    }

    public static @NotNull Icon deriveSvgFittedToOpaqueBounds(
            @NotNull FlatSVGIcon svg,
            @NotNull Dimension targetBounds
    ) {
        Dimension fitted = calculateFittedDimensionAllowUpscale(
                new Dimension(svg.getIconWidth(), svg.getIconHeight()),
                targetBounds
        );

        int renderWidth = Math.max(1, fitted.width * 4);
        int renderHeight = Math.max(1, fitted.height * 4);
        FlatSVGIcon probe = svg.derive(renderWidth, renderHeight);

        BufferedImage rendered = new BufferedImage(renderWidth, renderHeight, BufferedImage.TYPE_INT_ARGB);
        Graphics2D g2 = rendered.createGraphics();
        try {
            probe.paintIcon(null, g2, 0, 0);
        } finally {
            g2.dispose();
        }

        Rectangle opaqueBounds = opaqueBounds(rendered);
        if (opaqueBounds == null) {
            return svg.derive(fitted.width, fitted.height);
        }

        double opaqueWidth = Math.max(1d, opaqueBounds.width);
        double opaqueHeight = Math.max(1d, opaqueBounds.height);
        double boostW = targetBounds.width / (opaqueWidth / 4d);
        double boostH = targetBounds.height / (opaqueHeight / 4d);
        double boost = Math.min(boostW, boostH);
        boost = Math.max(1.0d, Math.min(boost, 3.0d));

        int outWidth = Math.max(1, (int) Math.round(fitted.width * boost));
        int outHeight = Math.max(1, (int) Math.round(fitted.height * boost));
        return svg.derive(outWidth, outHeight);
    }

    private static Rectangle opaqueBounds(@NotNull BufferedImage image) {
        int minX = image.getWidth();
        int minY = image.getHeight();
        int maxX = -1;
        int maxY = -1;

        for (int y = 0; y < image.getHeight(); y++) {
            for (int x = 0; x < image.getWidth(); x++) {
                int alpha = (image.getRGB(x, y) >>> 24) & 0xFF;
                if (alpha != 0) {
                    if (x < minX) minX = x;
                    if (y < minY) minY = y;
                    if (x > maxX) maxX = x;
                    if (y > maxY) maxY = y;
                }
            }
        }

        if (maxX < minX || maxY < minY) {
            return null;
        }
        return new Rectangle(minX, minY, (maxX - minX) + 1, (maxY - minY) + 1);
    }
}
