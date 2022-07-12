package mediathek.controller

import mediathek.javafx.filterpanel.SenderListBoxModel
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
            senderSet.addAll(SenderListBoxModel.readOnlySenderList)
            config.setProperty(SENDER_KEY, senderSet)
        } else {
            senderSet.addAll(storedSenderList)
        }
    }

    /**
     * Check if a sender is approved to be loaded into the program.
     */
    @JvmStatic
    fun isApproved(sender: String): Boolean {
        return senderSet.contains(sender)
    }

    /**
     * Approve that a sender may be loaded from filmlist.
     */
    @JvmStatic
    fun approve(sender: String) {
        if (!senderSet.contains(sender)) {
            senderSet.add(sender)
            config.setProperty(SENDER_KEY, senderSet)
        }
    }

    /**
     * Deny a sender from being loaded.
     */
    @JvmStatic
    fun deny(sender: String) {
        if (senderSet.contains(sender)) {
            senderSet.remove(sender)
            config.setProperty(SENDER_KEY, senderSet)
        }
    }
}