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

package mediathek.gui.tabs.tab_film.filter.zeitraum;

import javax.swing.*;
import java.text.ParseException;
import java.util.Objects;

public class ZeitraumSpinnerFormatter extends JFormattedTextField.AbstractFormatter {
    public static final String INFINITE_TEXT = "∞";
    public static final int INFINITE_VALUE = 0;

    @Override
    public Object stringToValue(final String text) throws ParseException {
        try {
            if (text.equals(INFINITE_TEXT))
                return INFINITE_VALUE;
            return Integer.valueOf(text);
        } catch (final NumberFormatException nfx) {
            //Find where in the string is the parsing error (so as to return ParseException accordingly).
            int i = 0;
            for (final int cp : text.codePoints().toArray()) {
                if (!Character.isDigit(cp))
                    throw new ParseException("Not a digit.", i);
                ++i;
            }
            //Should not happen:
            throw new ParseException("Failed to parse input \"" + text + "\".", 0);
        }
    }

    @Override
    public String valueToString(final Object value) {
        if (Objects.equals(value, INFINITE_VALUE))
            return INFINITE_TEXT;
        return Objects.toString(value);
    }
}
