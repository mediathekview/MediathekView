/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
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
package mediathek.controller.filmeLaden.suchen.sender;

import mediathek.controller.filmeLaden.suchen.FilmeSuchenSender;

public class MediathekArte_fr extends MediathekArte_de implements Runnable {

    /**
     *
     * @param ddaten
     * @param dde
     */
    public MediathekArte_fr(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, startPrio);
        nameSenderMReader = SENDER_ARTE_FR;
        URL_ARTE = "http://www.arte.tv/papi/tvguide/epg/schedule/F/L3/";
    }
}
