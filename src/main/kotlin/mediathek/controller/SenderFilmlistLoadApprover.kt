package mediathek.controller

import mediathek.config.application.ApplicationConfiguration
import mediathek.tool.SenderListBoxModel
import java.util.concurrent.ConcurrentHashMap

/**
 * Approve or deny the load of a sender from a filmlist
 */
object SenderFilmlistLoadApprover {
    val senderSet: ConcurrentHashMap.KeySetView<String, Boolean> = ConcurrentHashMap.newKeySet()
    private val applicationConfiguration = ApplicationConfiguration.getInstance()

    init {
        //load settings from config
        val storedSenderList = applicationConfiguration.approvedFilmlistLoadSenders
        if (storedSenderList.isEmpty()) {
            //manually approve all of them and store in config :(
            senderSet.addAll(SenderListBoxModel.providedSenderList)
            applicationConfiguration.setApprovedFilmlistLoadSenders(senderSet)
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
            applicationConfiguration.setApprovedFilmlistLoadSenders(senderSet)
        }
    }

    /**
     * Approve that all senders may be loaded from filmlist.
     * This will overwrite any manual approvals.
     */
    fun approveAll() {
        senderSet.clear()
        senderSet.addAll(SenderListBoxModel.providedSenderList)
        applicationConfiguration.setApprovedFilmlistLoadSenders(senderSet)
    }

    /**
     * Deny a sender from being loaded.
     */
    fun deny(sender: String) {
        if (senderSet.contains(sender)) {
            senderSet.remove(sender)
            applicationConfiguration.setApprovedFilmlistLoadSenders(senderSet)
        }
    }
}
