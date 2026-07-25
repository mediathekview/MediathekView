/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.swing

import javax.swing.event.ListDataEvent

/**
 * A reusable list data event whose change type and inclusive index range can be rewritten.
 *
 * @author <a href="mailto:jesse@swank.ca">Jesse Wilson</a>
 */
class MutableListDataEvent(source: Any) : ListDataEvent(source, CONTENTS_CHANGED, 0, 0) {
    private var currentIndex0 = 0
    private var currentIndex1 = 0
    private var currentType = CONTENTS_CHANGED

    /** Sets the inclusive start and end range for this event. */
    fun setRange(index0: Int, index1: Int) {
        currentIndex0 = index0
        currentIndex1 = index1
    }

    /** Sets the type of change. */
    fun setType(type: Int) {
        currentType = type
    }

    override fun getIndex0(): Int = currentIndex0

    override fun getIndex1(): Int = currentIndex1

    override fun getType(): Int = currentType

    override fun toString(): String = "$currentType[$currentIndex0,$currentIndex1]"
}
