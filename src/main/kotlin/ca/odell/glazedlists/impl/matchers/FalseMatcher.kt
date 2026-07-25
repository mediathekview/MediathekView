/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.matchers

import ca.odell.glazedlists.matchers.Matcher

/** A singleton [Matcher] that never matches. */
internal class FalseMatcher<E> private constructor() : Matcher<E> {
    @Suppress("UNUSED_PARAMETER")
    override fun matches(item: E): Boolean = false

    companion object {
        private val INSTANCE: Matcher<Any?> = FalseMatcher()

        @Suppress("UNCHECKED_CAST")
        fun <E> getInstance(): Matcher<E> = INSTANCE as Matcher<E>
    }
}
