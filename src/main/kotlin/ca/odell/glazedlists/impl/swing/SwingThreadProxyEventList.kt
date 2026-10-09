package ca.odell.glazedlists.impl.swing

import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.impl.gui.ThreadProxyEventList
import java.awt.EventQueue

/** Proxies EventList changes to the Swing event-dispatch thread. */
internal class SwingThreadProxyEventList<E>(source: EventList<E>) : ThreadProxyEventList<E>(source) {
    override fun schedule(runnable: Runnable) {
        if (EventQueue.isDispatchThread()) {
            runnable.run()
        } else {
            EventQueue.invokeLater(runnable)
        }
    }
}
