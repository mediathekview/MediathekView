/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.sort

/** Orders boolean values as `null`, `false`, `true`. */
internal class BooleanComparator : Comparator<Boolean?> {
    override fun compare(alpha: Boolean?, beta: Boolean?): Int = alpha.ordinal - beta.ordinal

    override fun equals(other: Any?): Boolean = other is BooleanComparator

    override fun hashCode(): Int = BooleanComparator::class.java.hashCode()

    private val Boolean?.ordinal: Int
        get() = when (this) {
            null -> 0
            false -> 1
            true -> 2
        }
}
