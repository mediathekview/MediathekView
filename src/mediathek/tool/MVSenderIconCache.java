package mediathek.tool;

import java.util.HashMap;
import java.util.Map;
import javax.swing.ImageIcon;

/**
 * This class will load only one instance for all used sender icons.
 */
public class MVSenderIconCache {

    private static Map<String, ImageIcon> iconCache = null;
    private static Map<String, ImageIcon> iconCache_small = null;
    private final static String PFAD = "/mediathek/res/sender/";
    private final static String PFAD_SMALL = "/mediathek/res/sender_small/";

    static {
        iconCache = new HashMap<>();
        iconCache.put("3Sat", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "3Sat.png")));
        iconCache.put("ARD", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "ARD.png")));
        iconCache.put("ARD.Podcast", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "ARD.png")));
//        ImageIcon arteIcon = new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "ARTE.png"));
//        iconCache.put("ARTE.DE", arteIcon);
//        iconCache.put("ARTE.FR", arteIcon);
        iconCache.put("ARTE.DE", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "arte_de.png")));
        iconCache.put("ARTE.FR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "arte_fr.png")));
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

    static {
        iconCache_small = new HashMap<>();
        iconCache_small.put("3Sat", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "3Sat.png")));
        iconCache_small.put("ARD", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "ARD.png")));
        iconCache_small.put("ARD.Podcast", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "ARD.png")));
//        ImageIcon arteIcon = new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "ARTE.png"));
//        iconCache_small.put("ARTE.DE", arteIcon);
//        iconCache_small.put("ARTE.FR", arteIcon);
        iconCache_small.put("ARTE.DE", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "arte_de.png")));
        iconCache_small.put("ARTE.FR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "arte_fr.png")));
        iconCache_small.put("BR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "BR.png")));
        iconCache_small.put("HR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "HR.png")));
        iconCache_small.put("KiKA", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "KiKA.png")));
        iconCache_small.put("MDR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "MDR.png")));
        iconCache_small.put("NDR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "NDR.png")));
        iconCache_small.put("ORF", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "ORF.png")));
        iconCache_small.put("RBB", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "RBB.png")));
        iconCache_small.put("SRF", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "SRF.png")));
        iconCache_small.put("SRF.Podcast", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "SRF.png")));
        iconCache_small.put("SWR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "SWR.png")));
        iconCache_small.put("WDR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "WDR.png")));
        iconCache_small.put("ZDF", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "ZDF.png")));
        iconCache_small.put("ZDF-tivi", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "ZDF.png")));
    }

    /**
     * Get the icon for a specific sender.
     *
     * @param sender The name of the supported sender.
     * @param small
     * @return The {@link javax.swing.ImageIcon} for the sender or null.
     */
    public ImageIcon get(String sender, boolean small) {
        if (small) {
            return iconCache_small.get(sender);
        } else {
            return iconCache.get(sender);
        }
    }
}
