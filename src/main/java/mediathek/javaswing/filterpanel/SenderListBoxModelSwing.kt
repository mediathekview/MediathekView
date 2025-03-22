package mediathek.javaswing.filterpanel

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.TransformedList
import ca.odell.glazedlists.event.ListEvent
import mediathek.tool.GermanStringSorter

/**
 * The base model object for all available senders that the client can process.
 */
object SenderListBoxModelSwing {
    @JvmStatic
    val providedSenderList: EventList<String> = BasicEventList()

    @JvmStatic
    val readOnlySenderList = ReadOnlySenderListBoxModel()

    class ReadOnlySenderListBoxModel : TransformedList<String, String>(providedSenderList) {
        /*init {
            source.addListEventListener(this)
        }*/

        override fun isWritable(): Boolean {
            return false
        }

        override fun listChanged(listChanges: ListEvent<String>?) {
            //updates.forwardEvent(listChanges)
        }
    }

    init {
        providedSenderList.add("3Sat")
        providedSenderList.add("ARD")
        providedSenderList.add("ARTE.DE")
        providedSenderList.add("ARTE.EN")
        providedSenderList.add("ARTE.ES")
        providedSenderList.add("ARTE.FR")
        providedSenderList.add("ARTE.IT")
        providedSenderList.add("ARTE.PL")
        providedSenderList.add("BR")
        providedSenderList.add("DW")
        providedSenderList.add("Funk.net")
        providedSenderList.add("HR")
        providedSenderList.add("KiKA")
        providedSenderList.add("MDR")
        providedSenderList.add("NDR")
        providedSenderList.add("ORF")
        providedSenderList.add("PHOENIX")
        providedSenderList.add("Radio Bremen TV")
        providedSenderList.add("RBB")
        providedSenderList.add("SR")
        providedSenderList.add("SRF")
        providedSenderList.add("SRF.Podcast")
        providedSenderList.add("SWR")
        providedSenderList.add("WDR")
        providedSenderList.add("ZDF")
        providedSenderList.add("ZDF-tivi")
        providedSenderList.sortWith(GermanStringSorter.getInstance())
    }
}