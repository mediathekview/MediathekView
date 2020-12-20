package mediathek.controller

import mediathek.tool.ApplicationConfiguration
import java.util.concurrent.ConcurrentHashMap

/**
 * Approve or deny the load of a sender from a filmlist
 */
object SenderFilmlistLoadApprover {
    val senderSet: ConcurrentHashMap.KeySetView<String, Boolean> = ConcurrentHashMap.newKeySet()
    private const val SENDER_KEY = "filmlist.approved_for_load"
    private val config = ApplicationConfiguration.getConfiguration()

    init {
        //load settings from config
        val storedSenderList = config.getList(String::class.java, SENDER_KEY)
        if (storedSenderList == null || storedSenderList.isEmpty()) {
            //manually approve all of them and store in config :(
            initializeSenderList()
            config.setProperty(SENDER_KEY, senderSet)
        } else {
            senderSet.addAll(storedSenderList)
        }
    }

    /**
     * Check if a sender is approved to be loaded into the program.
     */
    fun isApproved(sender: String): Boolean {
        return senderSet.contains(sender)
    }

    /**
     * Approve that a sender may be loaded from filmlist.
     */
    fun approve(sender: String) {
        if (!senderSet.contains(sender)) {
            senderSet.add(sender)
            config.setProperty(SENDER_KEY, senderSet)
        }
    }

    /**
     * Deny a sender from being loaded.
     */
    fun deny(sender: String) {
        if (senderSet.contains(sender)) {
            senderSet.remove(sender)
            config.setProperty(SENDER_KEY, senderSet)
        }
    }

    private fun initializeSenderList() {
        senderSet.add("3Sat")
        senderSet.add("ARD")
        senderSet.add("ARTE.DE")
        senderSet.add("ARTE.FR")
        senderSet.add("BR")
        senderSet.add("DW")
        senderSet.add("HR")
        senderSet.add("KiKA")
        senderSet.add("MDR")
        senderSet.add("NDR")
        senderSet.add("ORF")
        senderSet.add("PHOENIX")
        senderSet.add("Radio Bremen TV")
        senderSet.add("RBB")
        senderSet.add("SR")
        senderSet.add("SRF")
        senderSet.add("SRF.Podcast")
        senderSet.add("SWR")
        senderSet.add("WDR")
        senderSet.add("ZDF")
        senderSet.add("ZDF-tivi")
        senderSet.add("Funk.net")
    }
}