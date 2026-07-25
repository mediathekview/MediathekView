package ca.odell.glazedlists.swing

import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.GlazedLists
import ca.odell.glazedlists.ThresholdList
import ca.odell.glazedlists.TransformedList
import ca.odell.glazedlists.gui.TableFormat
import ca.odell.glazedlists.impl.swing.LowerThresholdRangeModel
import ca.odell.glazedlists.impl.swing.SwingThreadProxyEventList
import ca.odell.glazedlists.impl.swing.UpperThresholdRangeModel
import javax.swing.BoundedRangeModel

/** Returns a view that publishes this list's changes on the Swing event dispatch thread. */
fun <E> EventList<E>.swingThreadProxyList(): TransformedList<E, E> =
    SwingThreadProxyEventList(this)

/** Returns whether this list publishes all changes on the Swing event dispatch thread. */
fun EventList<*>?.isSwingThreadProxyList(): Boolean =
    this is SwingThreadProxyEventList<*>

/** Creates a model that manipulates this list's lower threshold. */
fun ThresholdList<*>.lowerRangeModel(): BoundedRangeModel =
    LowerThresholdRangeModel(this)

/** Creates a model that manipulates this list's upper threshold. */
fun ThresholdList<*>.upperRangeModel(): BoundedRangeModel =
    UpperThresholdRangeModel(this)

/** Creates a table model backed directly by this list. */
fun <E : Any> EventList<E>.eventTableModel(
    tableFormat: TableFormat<in E>,
): AdvancedTableModel<E> =
    DefaultEventTableModel(this, tableFormat)

/** Creates a table model backed by a Swing-thread proxy of this list. */
fun <E : Any> EventList<E>.eventTableModelWithThreadProxyList(
    tableFormat: TableFormat<in E>,
): AdvancedTableModel<E> =
    DefaultEventTableModel(createSwingThreadProxyList(), true, tableFormat)

/** Creates a table model using [eventAdapterFactory] to translate list events. */
fun <E : Any> EventList<E>.eventTableModel(
    tableFormat: TableFormat<in E>,
    eventAdapterFactory: TableModelEventAdapter.Factory<E>,
): AdvancedTableModel<E> =
    DefaultEventTableModel(this, tableFormat).also {
        it.eventAdapter = eventAdapterFactory.create(it)
    }

/** Creates a proxied table model using [eventAdapterFactory] to translate list events. */
fun <E : Any> EventList<E>.eventTableModelWithThreadProxyList(
    tableFormat: TableFormat<in E>,
    eventAdapterFactory: TableModelEventAdapter.Factory<E>,
): AdvancedTableModel<E> =
    DefaultEventTableModel(createSwingThreadProxyList(), true, tableFormat).also {
        it.eventAdapter = eventAdapterFactory.create(it)
    }

/** Creates a reflective table model for the named bean properties. */
fun <E : Any> EventList<E>.eventTableModel(
    propertyNames: Array<String>,
    columnLabels: Array<String>,
    writable: BooleanArray,
): AdvancedTableModel<E> =
    eventTableModel(GlazedLists.tableFormat(propertyNames, columnLabels, writable))

/** Creates a proxied reflective table model for the named bean properties. */
fun <E : Any> EventList<E>.eventTableModelWithThreadProxyList(
    propertyNames: Array<String>,
    columnLabels: Array<String>,
    writable: BooleanArray,
): AdvancedTableModel<E> =
    eventTableModelWithThreadProxyList(GlazedLists.tableFormat(propertyNames, columnLabels, writable))

/** Creates a selection model backed directly by this list. */
fun <E> EventList<E>.eventSelectionModel(): AdvancedListSelectionModel<E> =
    DefaultEventSelectionModel(this)

/** Creates a selection model backed by a Swing-thread proxy of this list. */
fun <E> EventList<E>.eventSelectionModelWithThreadProxyList(): AdvancedListSelectionModel<E> =
    DefaultEventSelectionModel(createSwingThreadProxyList(), true)

/** Creates a list model backed directly by this list. */
fun <E> EventList<E>.eventListModel(): DefaultEventListModel<E> =
    DefaultEventListModel(this)

/** Creates a list model backed by a Swing-thread proxy of this list. */
fun <E> EventList<E>.eventListModelWithThreadProxyList(): DefaultEventListModel<E> =
    DefaultEventListModel(createSwingThreadProxyList(), true)

/** Creates a combo-box model backed directly by this list. */
fun <E> EventList<E>.eventComboBoxModel(): DefaultEventComboBoxModel<E> =
    DefaultEventComboBoxModel(this)

/** Creates a combo-box model backed by a Swing-thread proxy of this list. */
fun <E> EventList<E>.eventComboBoxModelWithThreadProxyList(): DefaultEventComboBoxModel<E> =
    DefaultEventComboBoxModel(createSwingThreadProxyList(), true)

private fun <E> EventList<E>.createSwingThreadProxyList(): EventList<E> {
    readWriteLock.readLock().lock()
    return try {
        swingThreadProxyList()
    } finally {
        readWriteLock.readLock().unlock()
    }
}
