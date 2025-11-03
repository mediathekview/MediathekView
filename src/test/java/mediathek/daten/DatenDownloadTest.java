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

package mediathek.daten;

import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;

import static org.junit.jupiter.api.Assertions.assertEquals;

class DatenDownloadTest {

    @Test
    void stripDotsAndColons() {
        var str = "2025.09.27";
        var dd = new DatenDownload();
        var res = dd.stripDotsAndColons(str);
        assertEquals("20250927", res);

        str = "20:15:00";
        res = dd.stripDotsAndColons(str);
        assertEquals("201500", res);
    }

    @Test
    void formatTimeRemaining() {
        long restSekunden = 360;
        var dd = new DatenDownload();
        var res =  dd.formatTimeRemaining(restSekunden);
        assertEquals("6 Min.", res);

        restSekunden = 240;
        res =  dd.formatTimeRemaining(restSekunden);
        assertEquals("5 Min.", res);

        restSekunden = 180;
        res =  dd.formatTimeRemaining(restSekunden);
        assertEquals("4 Min.", res);

        restSekunden = 120;
        res =  dd.formatTimeRemaining(restSekunden);
        assertEquals("3 Min.", res);

        restSekunden = 70;
        res =  dd.formatTimeRemaining(restSekunden);
        assertEquals("2 Min.", res);

        restSekunden = 40;
        res =  dd.formatTimeRemaining(restSekunden);
        assertEquals("1 Min.", res);

        restSekunden = 25;
        res =  dd.formatTimeRemaining(restSekunden);
        assertEquals("30 s", res);

        restSekunden = 15;
        res =  dd.formatTimeRemaining(restSekunden);
        assertEquals("20 s", res);

        restSekunden = 8;
        res =  dd.formatTimeRemaining(restSekunden);
        assertEquals("10 s", res);
    }

    @Test
    void replaceYearParameter() {
        var film = new DatenFilm();
        var dd = new DatenDownload();

        //four character test
        var res = dd.replaceYearParameter("%3", film);
        LocalDateTime now = LocalDateTime.now();
        var strYear = String.valueOf(now.getYear());
        assertEquals(strYear, res);

        //two character test
        res = dd.replaceYearParameter("%3_2", film);
        assertEquals(strYear.substring(strYear.length() - 2), res);
    }
}