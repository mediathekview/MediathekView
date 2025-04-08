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
    public final static int UNLIMITED_VALUE = 110;
    private static final Logger logger = LogManager.getLogger();

    public FilmLengthSlider() {
        super(0, 110);
        setPaintLabels(true);
        setPaintTicks(true);
        setPaintTrack(true);
        setMajorTickSpacing(10);
        setLabelTable(new TestTable());
    }

    public void restoreFilterConfig(@NotNull FilterConfiguration filterConfig) {
        try {
            setValueIsAdjusting(true);
            setHighValue((int) filterConfig.getFilmLengthMax());
            setLowValue((int) filterConfig.getFilmLengthMin());
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

    private static class TestTable extends Hashtable<Integer, JComponent> {
        public TestTable() {
            put(0, new JLabel("0"));
            put(10, new JLabel("10"));
            put(20, new JLabel("20"));
            put(30, new JLabel("30"));
            put(40, new JLabel("40"));
            put(50, new JLabel("50"));
            put(60, new JLabel("60"));
            put(70, new JLabel("70"));
            put(80, new JLabel("80"));
            put(90, new JLabel("90"));
            put(100, new JLabel("100"));
            put(110, new JLabel("∞"));
        }
    }
}
