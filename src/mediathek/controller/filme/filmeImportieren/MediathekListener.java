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
package mediathek.controller.filme.filmeImportieren;

import java.util.EventListener;

public class MediathekListener implements EventListener {

    public static final int EREIGNIS_BLACKLIST_ADD = 1;
    public static final int EREIGNIS_BLACKLIST_DEL = 2;
    public static final int EREIGNIS_LISTE_PGRUPPE = 3;
    public static final int EREIGNIS_FILMLISTE_NEU = 4;
    public static final int EREIGNIS_ANZAHL_DOWNLOADS = 5;
    public int ereignis = -1;
    public String klasse = "";

    public MediathekListener() {
    }

    public MediathekListener(int eereignis, String kklasse) {
        ereignis = eereignis;
        klasse = kklasse;
    }

    public void ping() {
    }

    public void ping(String from) {
    }

    public void ping(String fromm, String meldung) {
    }
}
