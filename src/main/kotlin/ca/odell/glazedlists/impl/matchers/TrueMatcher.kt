/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.matchers

import ca.odell.glazedlists.matchers.Matcher

/** A singleton [Matcher] that always matches. */
internal class TrueMatcher<E> private constructor() : Matcher<E> {
    @Suppress("UNUSED_PARAMETER")
    override fun matches(item: E): Boolean = true

    companion object {
        private val INSTANCE: Matcher<Any?> = TrueMatcher()

        @Suppress("UNCHECKED_CAST")
        fun <E> getInstance(): Matcher<E> = INSTANCE as Matcher<E>
    }
}
