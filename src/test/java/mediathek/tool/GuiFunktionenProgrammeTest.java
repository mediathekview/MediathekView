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

package mediathek.tool;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class GuiFunktionenProgrammeTest {

    private static final String URL = "https://pd-videos.daserste.de/int/2025/12/05/54fb90c6-8f50-42ca-b0c0-3b488af1d2be/JOB_576107__sendeton_1920x1080-50p-5000kbit.mp4";

    @Test
    void checkPrefix() {
        assertTrue(GuiFunktionenProgramme.checkPrefix("http", URL));
        assertFalse(GuiFunktionenProgramme.checkPrefix("hurz", URL));
        assertTrue(GuiFunktionenProgramme.checkPrefix("http, https", URL));
        assertFalse(GuiFunktionenProgramme.checkPrefix("hurz, hurzs", URL));
    }

    @Test
    void checkSuffix() {
        assertTrue(GuiFunktionenProgramme.checkSuffix("mp4", URL));
        assertFalse(GuiFunktionenProgramme.checkSuffix("m4a", URL));
        assertTrue(GuiFunktionenProgramme.checkSuffix("mp4,mp3,m4v,flv,m4a", URL));
        assertFalse(GuiFunktionenProgramme.checkSuffix("hurz,json,dat", URL));
    }
}