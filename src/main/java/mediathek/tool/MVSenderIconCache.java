package mediathek.tool;

import java.awt.Image;
import java.util.HashMap;
import java.util.Map;
import javax.swing.ImageIcon;

/**
 * This class will load only one instance for all used sender icons.
 */
public class MVSenderIconCache {

    private final static Map<String, ImageIcon> iconCache = new HashMap<>();
    private final static Map<String, ImageIcon> iconCache_small = new HashMap<>();
    private final static String PFAD = "/mediathek/res/sender/";
    final static int height = 32;
    final static int height_small = 15;

    static {
        iconCache.put("3Sat", scaleImage(PFAD + "3sat.png", height));
        iconCache.put("ARD", scaleImage(PFAD + "ard.png", height));
        iconCache.put("ARD.Podcast", scaleImage(PFAD + "ard.png", height));
        iconCache.put("ARTE.DE", scaleImage(PFAD + "arte-de.png", height));
        iconCache.put("ARTE.FR", scaleImage(PFAD + "arte-fr.png", height));
        iconCache.put("BR", scaleImage(PFAD + "br.png", height));
        iconCache.put("HR", scaleImage(PFAD + "hr.png", height));
        iconCache.put("KiKA", scaleImage(PFAD + "kika.png", height));
        iconCache.put("MDR", scaleImage(PFAD + "mdr.png", height));
        iconCache.put("DW", scaleImage(PFAD + "dw.png", height));
        iconCache.put("NDR", scaleImage(PFAD + "ndr.png", height));
        iconCache.put("ORF", scaleImage(PFAD + "orf.png", height));
        iconCache.put("RBB", scaleImage(PFAD + "rbb.png", height));
        iconCache.put("SR", scaleImage(PFAD + "sr.png", height));
        iconCache.put("SRF", scaleImage(PFAD + "srf.png", height));
        iconCache.put("SRF.Podcast", scaleImage(PFAD + "srf-podcast.png", height));
        iconCache.put("SWR", scaleImage(PFAD + "swr.png", height));
        iconCache.put("WDR", scaleImage(PFAD + "wdr.png", height));
        iconCache.put("ZDF", scaleImage(PFAD + "zdf.png", height));
        iconCache.put("ZDF-tivi", scaleImage(PFAD + "zdf-tivi.png", height));
        iconCache.put("PHOENIX", scaleImage(PFAD + "phoenix.png", height));
    }

    static {
        iconCache_small.put("3Sat", scaleImage(PFAD + "3sat.png", height_small));
        iconCache_small.put("ARD", scaleImage(PFAD + "ard.png", height_small));
        iconCache_small.put("ARD.Podcast", scaleImage(PFAD + "ard.png", height_small));
        iconCache_small.put("ARTE.DE", scaleImage(PFAD + "arte-de.png", height_small));
        iconCache_small.put("ARTE.FR", scaleImage(PFAD + "arte-fr.png", height_small));
        iconCache_small.put("BR", scaleImage(PFAD + "br.png", height_small));
        iconCache_small.put("HR", scaleImage(PFAD + "hr.png", height_small));
        iconCache_small.put("KiKA", scaleImage(PFAD + "kika.png", height_small));
        iconCache_small.put("MDR", scaleImage(PFAD + "mdr.png", height_small));
        iconCache_small.put("DW", scaleImage(PFAD + "dw.png", height_small));
        iconCache_small.put("NDR", scaleImage(PFAD + "ndr.png", height_small));
        iconCache_small.put("ORF", scaleImage(PFAD + "orf.png", height_small));
        iconCache_small.put("RBB", scaleImage(PFAD + "rbb.png", height_small));
        iconCache_small.put("SR", scaleImage(PFAD + "sr.png", height_small));
        iconCache_small.put("SRF", scaleImage(PFAD + "srf.png", height_small));
        iconCache_small.put("SRF.Podcast", scaleImage(PFAD + "srf-podcast.png", height_small));
        iconCache_small.put("SWR", scaleImage(PFAD + "swr.png", height_small));
        iconCache_small.put("WDR", scaleImage(PFAD + "wdr.png", height_small));
        iconCache_small.put("ZDF", scaleImage(PFAD + "zdf.png", height_small));
        iconCache_small.put("ZDF-tivi", scaleImage(PFAD + "zdf-tivi.png", height_small));
        iconCache_small.put("PHOENIX", scaleImage(PFAD + "phoenix.png", height_small));
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

        int newWidth, priorHeight, priorWidth; // Variables for the old - new height and width
        Image image;
        ImageIcon sizeImage;

        image = new ImageIcon(MVSenderIconCache.class.getResource(source)).getImage();
        sizeImage = new ImageIcon(image);

        priorHeight = sizeImage.getIconHeight();
        priorWidth = sizeImage.getIconWidth();

        newWidth = (int) (((float) priorWidth / (float) priorHeight) * (float) maxHeight);

        return new ImageIcon(image.getScaledInstance(newWidth, maxHeight, Image.SCALE_AREA_AVERAGING));
    }

}
