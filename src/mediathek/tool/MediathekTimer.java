/*
 *     MediathekView
 *     Copyright (C) 2008 W. Xaver
 *     W.Xaver[at]googlemail.com
 *     http://zdfmediathk.sourceforge.net/
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *      the Free Software Foundation, either version 3 of the License, or
 *      any later version.
 *
 *      This program is distributed in the hope that it will be useful,
 *      but WITHOUT ANY WARRANTY; without even the implied warranty of
 *      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *      GNU General Public License for more details.
 *
 *      You should have received a copy of the GNU General Public License
 *      along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.tool;

import javax.swing.event.EventListenerList;

public class MediathekTimer {

    private final int WARTEZEIT = 1000; // 1 Sekunde
    private EventListenerList listeners = new EventListenerList();

    public MediathekTimer() {
        new Thread(new TimerClass()).start();
    }

    public void addAdListener(ListenerMediathekView listener) {
        listeners.add(ListenerMediathekView.class, listener);
    }

    private void notifyTakt() {
        for (ListenerMediathekView l : listeners.getListeners(ListenerMediathekView.class)) {
            l.ping();
        }
    }

    // ################################################
    // Timer-Thread
    // ################################################
    private class TimerClass implements Runnable {

        @Override
        public synchronized void run() {
            while (true) {
                try {
                    Thread.sleep(WARTEZEIT);
                } catch (InterruptedException e) {
                } finally {
                    notifyTakt();
                }
            }
        }
    }
}
