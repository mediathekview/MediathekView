package mediathek.tool;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.util.Optional;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * This class will load only one instance for all used sender icons.
 */
public class MVSenderIconCache {

    private final static int ICON_SIZE_LARGE = 32;
    private final static int ICON_SIZE_SMALL = 16;
    private final LoadingCache<String, Optional<ImageIcon>> senderCache_small = CacheBuilder.newBuilder()
            .expireAfterAccess(2, TimeUnit.MINUTES)
            .build(new IconCacheLoader(ICON_SIZE_SMALL));
    private final LoadingCache<String, Optional<ImageIcon>> senderCache = CacheBuilder.newBuilder()
            .expireAfterAccess(2, TimeUnit.MINUTES)
            .build(new IconCacheLoader(ICON_SIZE_LARGE));

    public MVSenderIconCache() {
        setupCleanupScheduler();
    }

    private void setupCleanupScheduler() {
        ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(1);
        scheduler.scheduleAtFixedRate(() -> {
            senderCache.cleanUp();
            senderCache_small.cleanUp();
        }, 2, 2, TimeUnit.MINUTES);
    }

    /**
     * Get the icon for a specific sender.
     *
     * @param sender The name of the supported sender.
     * @param small  large or small icon requested.
     * @return The {@link javax.swing.ImageIcon} for the sender or null.
     */
    public ImageIcon get(String sender, boolean small) {
        Optional<ImageIcon> icon;
        try {
            if (small)
                icon = senderCache_small.get(sender);
            else
                icon = senderCache.get(sender);
        } catch (CacheLoader.InvalidCacheLoadException | ExecutionException ex) {
            ex.printStackTrace();
            icon = Optional.empty();
        }

        return icon.orElse(null);
    }

    class IconCacheLoader extends CacheLoader<String, Optional<ImageIcon>> {
        private final int height;

        public IconCacheLoader(int height) {
            this.height = height;
        }

        private ImageIcon scaleImage(String source, int maxHeight) {

            int newWidth, priorHeight, priorWidth; // Variables for the old - new ICON_SIZE_LARGE and width
            Image image;
            ImageIcon sizeImage;

            image = new ImageIcon(MVSenderIconCache.class.getResource(source)).getImage();
            sizeImage = new ImageIcon(image);

            priorHeight = sizeImage.getIconHeight();
            priorWidth = sizeImage.getIconWidth();

            newWidth = (int) (((float) priorWidth / (float) priorHeight) * (float) maxHeight);

            return new ImageIcon(image.getScaledInstance(newWidth, maxHeight, Image.SCALE_AREA_AVERAGING));
        }

        @Override
        public Optional<ImageIcon> load(@NotNull String sender) {
            ImageIcon icon;

            switch (sender) {
                case "3Sat":
                    icon = scaleImage("/mediathek/res/sender/3sat.png", height);
                    break;

                case "ARD":
                case "ARD.Podcast":
                    icon = scaleImage("/mediathek/res/sender/ard.png", height);
                    break;

                case "ARTE.DE":
                    icon = scaleImage("/mediathek/res/sender/arte-de.png", height);
                    break;

                case "ARTE.FR":
                    icon = scaleImage("/mediathek/res/sender/arte-fr.png", height);
                    break;

                case "BR":
                    icon = scaleImage("/mediathek/res/sender/br.png", height);
                    break;

                case "HR":
                    icon = scaleImage("/mediathek/res/sender/hr.png", height);
                    break;

                case "KiKA":
                    icon = scaleImage("/mediathek/res/sender/kika.png", height);
                    break;

                case "MDR":
                    icon = scaleImage("/mediathek/res/sender/mdr.png", height);
                    break;

                case "DW":
                    icon = scaleImage("/mediathek/res/sender/dw.png", height);
                    break;

                case "NDR":
                    icon = scaleImage("/mediathek/res/sender/ndr.png", height);
                    break;

                case "ORF":
                    icon = scaleImage("/mediathek/res/sender/orf.png", height);
                    break;

                case "RBB":
                    icon = scaleImage("/mediathek/res/sender/rbb.png", height);
                    break;

                case "SR":
                    icon = scaleImage("/mediathek/res/sender/sr.png", height);
                    break;

                case "SRF":
                    icon = scaleImage("/mediathek/res/sender/srf.png", height);
                    break;

                case "SRF.Podcast":
                    icon = scaleImage("/mediathek/res/sender/srf-podcast.png", height);
                    break;

                case "SWR":
                    icon = scaleImage("/mediathek/res/sender/swr.png", height);
                    break;

                case "WDR":
                    icon = scaleImage("/mediathek/res/sender/wdr.png", height);
                    break;

                case "ZDF":
                    icon = scaleImage("/mediathek/res/sender/zdf.png", height);
                    break;

                case "ZDF-tivi":
                    icon = scaleImage("/mediathek/res/sender/zdf-tivi.png", height);
                    break;

                case "PHOENIX":
                    icon = scaleImage("/mediathek/res/sender/phoenix.png", height);
                    break;

                default:
                    icon = null;
                    break;
            }

            return Optional.ofNullable(icon);
        }
    }
}
