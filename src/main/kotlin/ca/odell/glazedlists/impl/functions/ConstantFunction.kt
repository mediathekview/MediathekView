/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.functions

/** A function that always returns the same value regardless of the input. */
internal class ConstantFunction<E, V>(private val value: V) : (E) -> V {
    @Suppress("UNUSED_PARAMETER")
    override fun invoke(sourceValue: E): V = value
}
