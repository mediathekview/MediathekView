/* Glazed Lists                                                 (c) 2003-2013 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.swing

import ca.odell.glazedlists.impl.swing.DefaultTableModelEventAdapterFactory
import ca.odell.glazedlists.impl.swing.ManyToOneTableModelEventAdapterFactory

/** A factory for creating objects to be used with Glazed Lists and Swing. */
object GlazedListsSwing {
    /** Returns the default factory for translating list events to table-model events. */
    fun <E> defaultEventAdapterFactory(): TableModelEventAdapter.Factory<E> =
        DefaultTableModelEventAdapterFactory.getInstance()

    /** Returns the factory that translates each list event to at most one table-model event. */
    fun <E> manyToOneEventAdapterFactory(): TableModelEventAdapter.Factory<E> =
        ManyToOneTableModelEventAdapterFactory.getInstance()
}
