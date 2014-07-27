package mediathek.tool;

import java.awt.Image;
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
    final static int height = 32;
    final static int height_small = 15;

    static {
        iconCache = new HashMap<>();
//        iconCache.put("3Sat", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "3sat.png")));
//        iconCache.put("ARD", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "ard.png")));
//        iconCache.put("ARD.Podcast", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "ard.png")));
//        iconCache.put("ARTE.DE", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "arte-de.png")));
//        iconCache.put("ARTE.FR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "arte-fr.png")));
//        iconCache.put("BR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "br.png")));
//        iconCache.put("HR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "hr.png")));
//        iconCache.put("KiKA", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "kika.png")));
//        iconCache.put("MDR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "mdr.png")));
//        iconCache.put("NDR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "ndr.png")));
//        iconCache.put("ORF", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "orf.png")));
//        iconCache.put("RBB", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "rbb.png")));
//        iconCache.put("SRF", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "srf.png")));
//        iconCache.put("SRF.Podcast", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "srf.png")));
//        iconCache.put("SWR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "swr.png")));
//        iconCache.put("WDR", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "wdr.png")));
//        iconCache.put("ZDF", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "zdf.png")));
//        iconCache.put("ZDF-tivi", new ImageIcon(MVSenderIconCache.class.getResource(PFAD + "zdf.png")));

        iconCache.put("3Sat", scaleImage(PFAD + "3sat.png", height));
        iconCache.put("ARD", scaleImage(PFAD + "ard.png", height));
        iconCache.put("ARD.Podcast", scaleImage(PFAD + "ard.png", height));
        iconCache.put("ARTE.DE", scaleImage(PFAD + "arte-de.png", height));
        iconCache.put("ARTE.FR", scaleImage(PFAD + "arte-fr.png", height));
        iconCache.put("BR", scaleImage(PFAD + "br.png", height));
        iconCache.put("HR", scaleImage(PFAD + "hr.png", height));
        iconCache.put("KiKA", scaleImage(PFAD + "kika.png", height));
        iconCache.put("MDR", scaleImage(PFAD + "mdr.png", height));
        iconCache.put("NDR", scaleImage(PFAD + "ndr.png", height));
        iconCache.put("ORF", scaleImage(PFAD + "orf.png", height));
        iconCache.put("RBB", scaleImage(PFAD + "rbb.png", height));
        iconCache.put("SRF", scaleImage(PFAD + "srf.png", height));
        iconCache.put("SRF.Podcast", scaleImage(PFAD + "srf.png", height));
        iconCache.put("SWR", scaleImage(PFAD + "swr.png", height));
        iconCache.put("WDR", scaleImage(PFAD + "wdr.png", height));
        iconCache.put("ZDF", scaleImage(PFAD + "zdf.png", height));
        iconCache.put("ZDF-tivi", scaleImage(PFAD + "zdf.png", height));

    }

    static {
        iconCache_small = new HashMap<>();
        iconCache_small.put("3Sat", scaleImage(PFAD + "3sat.png", height_small));
        iconCache_small.put("ARD", scaleImage(PFAD + "ard.png", height_small));
        iconCache_small.put("ARD.Podcast", scaleImage(PFAD + "ard.png", height_small));
        iconCache_small.put("ARTE.DE", scaleImage(PFAD + "arte-de.png", height_small));
        iconCache_small.put("ARTE.FR", scaleImage(PFAD + "arte-fr.png", height_small));
        iconCache_small.put("BR", scaleImage(PFAD + "br.png", height_small));
        iconCache_small.put("HR", scaleImage(PFAD + "hr.png", height_small));
        iconCache_small.put("KiKA", scaleImage(PFAD + "kika.png", height_small));
        iconCache_small.put("MDR", scaleImage(PFAD + "mdr.png", height_small));
        iconCache_small.put("NDR", scaleImage(PFAD + "ndr.png", height_small));
        iconCache_small.put("ORF", scaleImage(PFAD + "orf.png", height_small));
        iconCache_small.put("RBB", scaleImage(PFAD + "rbb.png", height_small));
        iconCache_small.put("SRF", scaleImage(PFAD + "srf.png", height_small));
        iconCache_small.put("SRF.Podcast", scaleImage(PFAD + "srf.png", height_small));
        iconCache_small.put("SWR", scaleImage(PFAD + "swr.png", height_small));
        iconCache_small.put("WDR", scaleImage(PFAD + "wdr.png", height_small));
        iconCache_small.put("ZDF", scaleImage(PFAD + "zdf.png", height_small));
        iconCache_small.put("ZDF-tivi", scaleImage(PFAD + "zdf.png", height_small));
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

    private static ImageIcon scaleImage(String source, int maxHeight) {

        int newHeight = 0, newWidth = 0;        // Variables for the new height and width
        int priorHeight = 0, priorWidth = 0;
        Image image;
        ImageIcon sizeImage;

        image = new ImageIcon(MVSenderIconCache.class.getResource(source)).getImage();
        sizeImage = new ImageIcon(image);

        priorHeight = sizeImage.getIconHeight();
        priorWidth = sizeImage.getIconWidth();

        newHeight = maxHeight;
        newWidth = (int) (((float) priorWidth / (float) priorHeight) * (float) newHeight);

        return new ImageIcon(image.getScaledInstance(newWidth, newHeight, Image.SCALE_AREA_AVERAGING));
    }

}
