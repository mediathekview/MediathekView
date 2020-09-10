package mediathek.tool;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import javax.swing.event.EventListenerList;
import java.util.EventListener;

public abstract class Listener implements EventListener {
    public static final int EREIGNIS_BLACKLIST_AUCH_FUER_ABOS = 27;
    public static final int EREIGNIS_BLACKLIST_START_GEAENDERT = 40;

    private static final EventListenerList listeners = new EventListenerList();
    private static final Logger logger = LogManager.getLogger(Listener.class);
    public int[] mvEreignis;
    public String klasse;

    public Listener(int eereignis, String kklasse) {
        mvEreignis = new int[]{eereignis};
        klasse = kklasse;
    }

    public static synchronized void addListener(Listener listener) {
        listeners.add(Listener.class, listener);
    }

    public static synchronized void notify(int ereignis, String klasse) {
        for (Listener l : listeners.getListeners(Listener.class)) {
            for (int er : l.mvEreignis) {
                if (er == ereignis) {
                    if (!l.klasse.equals(klasse)) {
                        // um einen Kreislauf zu verhindern
                        try {
                            l.pingen();
                        } catch (Exception ex) {
                            logger.warn("notify:", ex);
                        }
                    }
                }
            }
        }
    }

    public abstract void ping();

    private void pingen() {
        try {
            SwingUtilities.invokeLater(this::ping);
        } catch (Exception ex) {
            logger.error(ex);
        }
    }
}
