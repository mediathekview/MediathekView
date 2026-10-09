/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.sort

/** Orders comparable values naturally, with `null` before non-null values. */
internal class ComparableComparator<T> : Comparator<T?> where T : Comparable<T> {
    override fun compare(alpha: T?, beta: T?): Int = when {
        alpha != null && beta != null -> alpha.compareTo(beta)
        alpha == null && beta == null -> 0
        alpha == null -> -1
        else -> 1
    }

    override fun equals(other: Any?): Boolean = other is ComparableComparator<*>

    override fun hashCode(): Int = ComparableComparator::class.java.hashCode()
}
