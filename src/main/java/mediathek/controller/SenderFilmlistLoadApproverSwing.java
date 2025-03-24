package mediathek.controller;

import mediathek.config.Konstanten;
import mediathek.javafx.filterpanel.SenderListBoxModel;
import mediathek.javaswing.filterpanel.SenderListBoxModelSwing;
import mediathek.tool.ApplicationConfiguration;
import org.apache.commons.configuration2.Configuration;
import org.apache.commons.configuration2.sync.LockMode;

import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Approve or deny the load of a sender from a filmlist
 */
public class SenderFilmlistLoadApproverSwing {

    private static final ConcurrentHashMap.KeySetView<String, Boolean> senderSet = ConcurrentHashMap.newKeySet();

    private static final String SENDER_KEY = "filmlist.approved_for_load";

    private static final Configuration config = ApplicationConfiguration.getConfiguration();

    static {
        // Load settings from config
        List<String> storedSenderList = config.getList(String.class, SENDER_KEY);
        if (storedSenderList == null || storedSenderList.isEmpty()) {
            // Manually approve all of them and store in config :(
            senderSet.addAll(SenderListBoxModelSwing.getProvidedSenderList());
            config.setProperty(SENDER_KEY, senderSet);
        } else {
            senderSet.addAll(storedSenderList);
        }
    }

    /**
     * Check if a sender is approved to be loaded into the program.
     */
    public static boolean isApproved(String sender) {

        return senderSet.contains(sender);
    }

    /**
     * Approve that a sender may be loaded from filmlist.
     */
    public static void approve(String sender) {

        if (!senderSet.contains(sender)) {
            senderSet.add(sender);
            config.setProperty(SENDER_KEY, senderSet);
        }
    }

    /**
     * Deny a sender from being loaded.
     */
    public static void deny(String sender) {

        if (senderSet.contains(sender)) {
            senderSet.remove(sender);
            config.setProperty(SENDER_KEY, senderSet);
        }
    }

}
