/*
 * MediathekView
 * Copyright (C) 2014 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.controller;

import msearch.tool.GermanStringSorter;

public class MVUsedUrl implements Comparable<MVUsedUrl> {

    private static final GermanStringSorter sorter = GermanStringSorter.getInstance();

    String[] uUrl;

    public MVUsedUrl(String[] uUrl) {
        this.uUrl = uUrl;
    }

    public String getString() {
        return uUrl[1] + "    " + uUrl[0] + "    " + uUrl[2];
    }

    @Override
    public int compareTo(MVUsedUrl arg0) {
        return sorter.compare(uUrl[1], arg0.uUrl[1]);
    }
}
