/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.swing

import ca.odell.glazedlists.event.ListEvent
import javax.swing.event.TableModelEvent
import javax.swing.table.TableModel

/** A reusable table model event whose change information can be rewritten. */
class MutableTableModelEvent(source: TableModel) : TableModelEvent(source) {
    /** Sets the inclusive first and last row range. */
    fun setRange(firstRow: Int, lastRow: Int) {
        this.firstRow = firstRow
        this.lastRow = lastRow
    }

    /** Sets the Swing table event type. */
    fun setType(type: Int) {
        this.type = type
    }

    /** Configures this event to report a table structure change. */
    fun setStructureChanged() {
        firstRow = HEADER_ROW
        lastRow = HEADER_ROW
        column = ALL_COLUMNS
        type = UPDATE
    }

    /** Configures this event to report that all table data changed. */
    fun setAllDataChanged() {
        firstRow = 0
        lastRow = Int.MAX_VALUE
        column = ALL_COLUMNS
        type = UPDATE
    }

    /** Maps a Glazed Lists change to the corresponding Swing table event. */
    fun setValues(startIndex: Int, endIndex: Int, listChangeType: Int) {
        firstRow = startIndex
        lastRow = endIndex
        when (listChangeType) {
            ListEvent.INSERT -> type = INSERT
            ListEvent.DELETE -> type = DELETE
            ListEvent.UPDATE -> type = UPDATE
        }
        column = ALL_COLUMNS
    }
}
