/*
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mSearch.tool;

import org.jetbrains.annotations.NotNull;

import java.text.Collator;
import java.util.Comparator;
import java.util.Locale;

public class GermanStringSorter implements Comparator<String> {

    private static final Collator collator = Collator.getInstance(Locale.GERMANY);
    private static GermanStringSorter instance;

    private GermanStringSorter() {
        super();
    }

    public static GermanStringSorter getInstance() {
        if (instance == null) {
            instance = new GermanStringSorter();
            // ignore lower/upper case, but accept special characters in localised alphabetical order
            collator.setStrength(Collator.SECONDARY);
        }
        return instance;
    }

    @Override
    public int compare(@NotNull String o1, @NotNull String o2) {
        return collator.compare(o1, o2);
    }
}
