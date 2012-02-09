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
package mediathek.controller.filme.filmeImportieren;

import javax.swing.event.EventListenerList;
import mediathek.tool.GuiKonstanten;

public class MediathekTimer {

    private final int WARTEZEIT = 1000; // 1 Minuten
    public int neuLadenIn = GuiKonstanten.NEU_LADEN_IN;
    private EventListenerList listeners = new EventListenerList();

    public MediathekTimer() {
        new Thread(new TimerClass()).start();
    }

    public synchronized void resetTimer() {
        reset();
    }

    // #############################
    // listener
    // #############################
    public void addAdListener(MediathekListener listener) {
        listeners.add(MediathekListener.class, listener);
    }

    private void notifyTakt() {
        for (MediathekListener l : listeners.getListeners(MediathekListener.class)) {
            l.ping(neuLadenIn);
        }
    }
    // #########################################

    private void reset() {
        neuLadenIn = GuiKonstanten.NEU_LADEN_IN;

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
                    --neuLadenIn;
                    if (neuLadenIn <= 0) {
                        reset();
                    }
                    notifyTakt();
                }
            }
        }
    }
}
