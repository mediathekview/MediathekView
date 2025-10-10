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

package mediathek.gui.tabs.tab_film.filter;

import com.jidesoft.swing.RangeSlider;
import mediathek.tool.FilterConfiguration;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.util.Hashtable;

public class FilmLengthSlider extends RangeSlider {
    private static final Logger logger = LogManager.getLogger();
    private static final int MAX_FILM_LENGTH = 240;
    private static final int TICK_SPACING = 30;
    public final static int UNLIMITED_VALUE = MAX_FILM_LENGTH;

    public FilmLengthSlider() {
        super(0, MAX_FILM_LENGTH);
        setPaintLabels(true);
        setPaintTicks(true);
        setPaintTrack(true);
        setMajorTickSpacing(TICK_SPACING);
        setLabelTable(new FilmlengthLabelTable());
    }

    public void restoreFilterConfig(@NotNull FilterConfiguration filterConfig) {
        try {
            setValueIsAdjusting(true);
            setLowValue((int) filterConfig.getFilmLengthMin());
            setHighValue((int) filterConfig.getFilmLengthMax());
            setValueIsAdjusting(false);
        } catch (Exception exception) {
            logger.error("Failed to restore filmlength config", exception);
        }
    }

    public String getHighValueText() {
        String res;
        var highValue = getHighValue();
        if (highValue == UNLIMITED_VALUE) {
            res = "∞";
        } else {
            res = String.valueOf(highValue);
        }
        return res;
    }

    private static class FilmlengthLabelTable extends Hashtable<Integer, JComponent> {
        public FilmlengthLabelTable() {
            for (int i = 0; i < MAX_FILM_LENGTH; i += TICK_SPACING) {
                put(i, new JLabel(String.valueOf(i)));
            }
            put(MAX_FILM_LENGTH, new JLabel("∞"));
        }
    }
}
