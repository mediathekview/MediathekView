/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.matchers

/** A [MatcherEditor] whose [Matcher] never changes. */
internal class FixedMatcherEditor<E>(matcher: Matcher<E>) : AbstractMatcherEditor<E>() {
    init {
        fireChanged(matcher)
    }
}
