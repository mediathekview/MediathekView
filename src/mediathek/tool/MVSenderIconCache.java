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
        iconCache.put("3Sat", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "3sat.png")));
        iconCache.put("ARD", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "ard.png")));
        iconCache.put("ARD.Podcast", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "ard.png")));
//        ImageIcon arteIcon = new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "ARTE.png"));
//        iconCache.put("ARTE.DE", arteIcon);
//        iconCache.put("ARTE.FR", arteIcon);
        iconCache.put("ARTE.DE", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "arte-de.png")));
        iconCache.put("ARTE.FR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "arte-fr.png")));
        iconCache.put("BR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "br.png")));
        iconCache.put("HR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "hr.png")));
        iconCache.put("KiKA", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "kika.png")));
        iconCache.put("MDR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "mdr.png")));
        iconCache.put("NDR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "ndr.png")));
        iconCache.put("ORF", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "orf.png")));
        iconCache.put("RBB", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "rbb.png")));
        iconCache.put("SRF", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "srf.png")));
        iconCache.put("SRF.Podcast", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "srf.png")));
        iconCache.put("SWR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "swr.png")));
        iconCache.put("WDR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "wdr.png")));
        iconCache.put("ZDF", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "zdf.png")));
        iconCache.put("ZDF-tivi", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "zdf.png")));
    }

    static {
        iconCache_small = new HashMap<>();
        iconCache_small.put("3Sat", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "3sat.png")));
        iconCache_small.put("ARD", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "ard.png")));
        iconCache_small.put("ARD.Podcast", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "ard.png")));
//        ImageIcon arteIcon = new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "ARTE.png"));
//        iconCache_small.put("ARTE.DE", arteIcon);
//        iconCache_small.put("ARTE.FR", arteIcon);
        iconCache_small.put("ARTE.DE", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "arte-de.png")));
        iconCache_small.put("ARTE.FR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "arte-fr.png")));
        iconCache_small.put("BR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "br.png")));
        iconCache_small.put("HR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "hr.png")));
        iconCache_small.put("KiKA", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "kika.png")));
        iconCache_small.put("MDR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "mdr.png")));
        iconCache_small.put("NDR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "ndr.png")));
        iconCache_small.put("ORF", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "orf.png")));
        iconCache_small.put("RBB", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "rbb.png")));
        iconCache_small.put("SRF", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "srf.png")));
        iconCache_small.put("SRF.Podcast", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "srf.png")));
        iconCache_small.put("SWR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "swr.png")));
        iconCache_small.put("WDR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "wdr.png")));
        iconCache_small.put("ZDF", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "zdf.png")));
        iconCache_small.put("ZDF-tivi", new ImageIcon(MVSenderIconCache.class.getResource(PFAD_SMALL + "zdf.png")));
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
