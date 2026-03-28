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

package mediathek.daten;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DatenFilmGeoBlockingTest {

    @Test
    void filmWithoutCountriesIsNotGeoBlocked() {
        var film = new DatenFilm();

        assertFalse(film.isGeoBlockedForLocation(Country.DE));
    }

    @Test
    void euFilmIsNotBlockedForEuLocation() {
        var film = new DatenFilm();
        film.addCountry(Country.EU);

        assertFalse(film.isGeoBlockedForLocation(Country.DE));
    }

    @Test
    void euFilmIsBlockedOutsideEu() {
        var film = new DatenFilm();
        film.addCountry(Country.EU);

        assertTrue(film.isGeoBlockedForLocation(Country.OTHER));
    }

    @Test
    void countrySpecificFilmIsBlockedOutsideAllowedCountry() {
        var film = new DatenFilm();
        film.addCountry(Country.DE);

        assertFalse(film.isGeoBlockedForLocation(Country.DE));
        assertTrue(film.isGeoBlockedForLocation(Country.AT));
    }
}
