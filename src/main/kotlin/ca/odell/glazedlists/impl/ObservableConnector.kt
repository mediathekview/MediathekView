package ca.odell.glazedlists.impl

import ca.odell.glazedlists.ObservableElementChangeHandler
import ca.odell.glazedlists.ObservableElementList
import java.beans.PropertyChangeEvent
import java.beans.PropertyChangeListener
import java.util.*

class ObservableConnector<E> : ObservableElementList.Connector<E>, PropertyChangeListener
        where E : ObservableConnector.PropertyChangeObservable {

    interface PropertyChangeObservable {
        fun addPropertyChangeListener(listener: PropertyChangeListener)
        fun removePropertyChangeListener(listener: PropertyChangeListener)
    }

    private var list: ObservableElementChangeHandler<out E>? = null

    override fun propertyChange(event: PropertyChangeEvent) {
        update(event)
    }

    @Suppress("UNCHECKED_CAST")
    fun update(event: PropertyChangeEvent) {
        list?.elementChanged(event.source as E)
    }

    override fun installListener(element: E): EventListener {
        element.addPropertyChangeListener(this)
        return this
    }

    override fun uninstallListener(element: E, listener: EventListener) {
        element.removePropertyChangeListener(this)
    }

    override fun setObservableElementList(list: ObservableElementChangeHandler<out E>?) {
        this.list = list
    }
}
