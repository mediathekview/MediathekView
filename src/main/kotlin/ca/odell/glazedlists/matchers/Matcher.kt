/* Glazed Lists                                                 (c) 2003-2014 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.matchers

/** Determines whether a value matches a filter. */
fun interface Matcher<E> {
    fun matches(item: E): Boolean
}
