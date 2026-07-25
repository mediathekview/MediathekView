/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.filter

import ca.odell.glazedlists.TextFilterator

/**
 * [TextFilterator] that uses an object's [toString] value.
 *
 * @author James Lemieux
 * @author [Jesse Wilson](mailto:jesse@swank.ca)
 */
internal open class StringTextFilterator<E> : TextFilterator<E> {
    override fun getFilterStrings(baseList: MutableList<String>, element: E) {
        if (element != null) baseList.add(element.toString())
    }
}
