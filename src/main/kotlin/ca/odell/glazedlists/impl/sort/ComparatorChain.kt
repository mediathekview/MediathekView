/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.sort

import java.util.*

/**
 * A comparator chain compares objects using a list of [Comparator]s. The
 * first comparison where the objects differ is returned.
 *
 * @author [Jesse Wilson](mailto:jesse@swank.ca)
 */
internal class ComparatorChain<T>(
    comparators: Array<Comparator<T>>,
) : Comparator<T> {
    private val comparatorArray = comparators.clone()

    /**
     * Creates a comparator chain that evaluates the specified comparators in
     * sequence. The comparator list is copied defensively.
     */
    constructor(comparators: List<Comparator<T>>) : this(comparators.toTypedArray())

    /** Compares the two objects with each comparator in sequence. */
    override fun compare(alpha: T, beta: T): Int {
        for (comparator in comparatorArray) {
            val result = comparator.compare(alpha, beta)
            if (result != 0) return result
        }
        return 0
    }

    /** Retrieves a defensive copy of the comparators composing this chain. */
    val comparators: Array<Comparator<T>>
        get() = comparatorArray.clone()

    /** Retains the former Java record component accessor. */
    fun comparators(): Array<Comparator<T>> = comparatorArray.clone()

    override fun equals(other: Any?): Boolean =
        this === other || other is ComparatorChain<*> && comparatorArray.contentEquals(other.comparatorArray)

    override fun hashCode(): Int = comparatorArray.contentHashCode()

    override fun toString(): String = "ComparatorChain[comparators=${Objects.toIdentityString(comparatorArray)}]"
}
