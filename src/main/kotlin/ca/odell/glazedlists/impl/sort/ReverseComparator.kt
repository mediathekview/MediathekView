/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.sort

/** Reverses the ordering of [sourceComparator]. */
internal class ReverseComparator<T>(val sourceComparator: Comparator<T>) : Comparator<T> {
    override fun compare(alpha: T, beta: T): Int = sourceComparator.compare(beta, alpha)

    override fun equals(other: Any?): Boolean =
        this === other || other is ReverseComparator<*> && sourceComparator == other.sourceComparator

    override fun hashCode(): Int = sourceComparator.hashCode()
}
