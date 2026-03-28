/*
 * Copyright (c) 2025-2026 derreisende77.
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

package mediathek.gui.tabs.tab_film;

import com.formdev.flatlaf.extras.FlatSVGIcon;
import com.formdev.flatlaf.util.ScaledImageIcon;
import mediathek.tool.sender_icon_cache.MVSenderIconCache;
import mediathek.tool.sender_icon_cache.SenderIconRenderUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;

public class SenderIconLabel extends JLabel {
    private static final Dimension ICON_DIMENSION = new Dimension(96, 96);

    public SenderIconLabel() {
        setText("");
        setIcon(null);
    }

    private void sizeToIcon(@NotNull Icon icon) {
        int height = icon.getIconHeight();
        int width = icon.getIconWidth();

        Dimension d = new Dimension(width, height);
        setPreferredSize(d);
    }

    public void setSender(@Nullable String sender) {
        if (sender == null) {
            setIcon(null);
        }
        else {
            MVSenderIconCache.get(sender).ifPresentOrElse(icon -> {
                Icon origIcon;
                if (icon instanceof FlatSVGIcon svg) {
                    var destDim = SenderIconRenderUtil.calculateFittedDimensionAllowUpscale(
                            new Dimension(svg.getIconWidth(), svg.getIconHeight()),
                            ICON_DIMENSION
                    );
                    origIcon = svg.derive(destDim.width, destDim.height);
                } else {
                    var imageDim = new Dimension(icon.getIconWidth(), icon.getIconHeight());
                    var destDim = SenderIconRenderUtil.calculateFittedDimensionAllowUpscale(imageDim, ICON_DIMENSION);
                    origIcon = new ScaledImageIcon(icon, destDim.width, destDim.height);
                }
                setText("");
                setIcon(origIcon);
                sizeToIcon(origIcon);
            }, () -> {
                setIcon(null);
                setText(sender);
            });
        }
    }
}
