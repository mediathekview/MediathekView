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
package mediathek.controller.filmeLaden;

import java.util.EventListener;
import javax.swing.SwingUtilities;
import mediathek.tool.Log;

public class ListenerFilmeLaden implements EventListener {

    ListenerFilmeLadenEvent event;

    public void start(ListenerFilmeLadenEvent e) {
        event = e;
        run_(new Runnable() {
            @Override
            public void run() {
                // oder da
                start_(event);
            }
        });

    }

    public void progress(ListenerFilmeLadenEvent e) {
        event = e;
        run_(new Runnable() {
            @Override
            public void run() {
                // oder da
                progress_(event);
            }
        });
    }

    public void fertig(ListenerFilmeLadenEvent e) {
        event = e;
        run_(new Runnable() {
            @Override
            public void run() {
                // oder da
                fertig_(event);
            }
        });
    }

    public void start_(ListenerFilmeLadenEvent event) {
    }

    public void progress_(ListenerFilmeLadenEvent event) {
    }

    public void fertig_(ListenerFilmeLadenEvent event) {
    }

    private void run_(Runnable r) {
        try {
            if (SwingUtilities.isEventDispatchThread()) {
                // entweder hier
                r.run();
            } else {
                SwingUtilities.invokeLater(r);
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(461025879, "ListenerFilmeLaden.fertig", ex);
        }

    }
}
