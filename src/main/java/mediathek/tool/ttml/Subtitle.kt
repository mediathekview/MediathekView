package mediathek.tool.ttml

import java.util.*

internal class Subtitle {
    @JvmField
    var begin: Date? = null
    @JvmField
    var end: Date? = null
    @JvmField
    val listOfStrings: List<StyledString> = ArrayList()
}
