/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.matchers

import ca.odell.glazedlists.impl.beans.BeanProperty
import ca.odell.glazedlists.matchers.Matcher

/** Matches beans whose named property equals [value]. */
internal class BeanPropertyMatcher<E>(
    beanClass: Class<E>,
    propertyName: String,
    private val value: Any?,
) : Matcher<E?> {
    private val beanProperty = BeanProperty(beanClass, propertyName, readable = true, writable = false)

    override fun matches(item: E?): Boolean = item != null && beanProperty[item] == value
}
