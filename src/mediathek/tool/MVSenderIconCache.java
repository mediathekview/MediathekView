package mediathek.tool;

import javax.swing.*;
import java.util.HashMap;
import java.util.Map;

/**
 * This class will load only one instance for all used sender icons.
 */
public class MVSenderIconCache {

    private static Map<String, ImageIcon> iconCache = null;
    private final static String PFAD = "/mediathek/res/sender/";

    static {
        iconCache = new HashMap<>();
        iconCache.put("3Sat", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "3Sat.png")));
        iconCache.put("ARD", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "ARD.png")));
        iconCache.put("ARD.Podcast", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "ARD.png")));
        ImageIcon arteIcon = new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "ARTE.png"));
        iconCache.put("ARTE.DE", arteIcon);
        iconCache.put("ARTE.FR", arteIcon);
        iconCache.put("BR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "BR.png")));
        iconCache.put("HR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "HR.png")));
        iconCache.put("KiKA", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "KiKA.png")));
        iconCache.put("MDR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "MDR.png")));
        iconCache.put("NDR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "NDR.png")));
        iconCache.put("ORF", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "ORF.png")));
        iconCache.put("RBB", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "RBB.png")));
        iconCache.put("SRF", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "SRF.png")));
        iconCache.put("SRF.Podcast", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "SRF.png")));
        iconCache.put("SWR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "SWR.png")));
        iconCache.put("WDR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "WDR.png")));
        iconCache.put("ZDF", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "ZDF.png")));
        iconCache.put("ZDF-tivi", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "ZDF.png")));
    }

    /**
     * Get the icon for a specific sender.
     *
     * @param sender The name of the supported sender.
     * @return The {@link javax.swing.ImageIcon} for the sender or null.
     */
    public ImageIcon get(String sender) {
        return iconCache.get(sender);
    }
}
