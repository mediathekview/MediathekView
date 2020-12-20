package mediathek.tool;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import mediathek.config.Daten;
import mediathek.gui.messages.SenderIconStyleChangedEvent;
import net.engio.mbassy.listener.Handler;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.Optional;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * This class will load only one instance for all used sender icons.
 */
public class MVSenderIconCache {
    public static final String CONFIG_USE_LOCAL_SENDER_ICONS = "application.sender_icons.use_local";
    private final static int ICON_SIZE_LARGE = 32;
    private final static int ICON_SIZE_SMALL = 16;
    private static final Logger logger = LogManager.getLogger(MVSenderIconCache.class);
    private final AtomicBoolean useLocalIcons = new AtomicBoolean(false);
    private final LoadingCache<String, Optional<ImageIcon>> senderCache_small = CacheBuilder.newBuilder()
            .expireAfterAccess(2, TimeUnit.HOURS)
            .build(new IconCacheLoader(ICON_SIZE_SMALL));
    private final LoadingCache<String, Optional<ImageIcon>> senderCache = CacheBuilder.newBuilder()
            .expireAfterAccess(2, TimeUnit.HOURS)
            .build(new IconCacheLoader(ICON_SIZE_LARGE));

    public MVSenderIconCache(Daten d) {
        setupCleanupScheduler();

        d.getMessageBus().subscribe(this);
        useLocalIcons.set(ApplicationConfiguration.getConfiguration().getBoolean(CONFIG_USE_LOCAL_SENDER_ICONS, false));
    }

    @Handler
    private void handleSenderIconStyleChangedEvent(SenderIconStyleChangedEvent e) {
        logger.trace("invalidating caches due to sender icon style change");
        useLocalIcons.set(ApplicationConfiguration.getConfiguration().getBoolean(CONFIG_USE_LOCAL_SENDER_ICONS, false));
        senderCache_small.invalidateAll();
        senderCache.invalidateAll();
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
    public Optional<ImageIcon> get(String sender, boolean small) {
        Optional<ImageIcon> icon;
        try {
            if (small)
                icon = senderCache_small.get(sender);
            else
                icon = senderCache.get(sender);
        } catch (CacheLoader.InvalidCacheLoadException | ExecutionException ex) {
            icon = Optional.empty();
        }

        return icon;
    }

    class IconCacheLoader extends CacheLoader<String, Optional<ImageIcon>> {
        private static final String WIKI_BASE_URL = "https://upload.wikimedia.org/wikipedia/commons/thumb";
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

            return new ImageIcon(image.getScaledInstance(newWidth, maxHeight, Image.SCALE_SMOOTH));
        }

        private ImageIcon scaleImage(@NotNull BufferedImage b_img, final int maxHeight) {
            final float priorHeight = (float) b_img.getHeight();
            final float priorWidth = (float) b_img.getWidth();

            final int newWidth = Math.round(((priorWidth / priorHeight) * (float) maxHeight));
            final Image scaledImage = b_img.getScaledInstance(newWidth, maxHeight, Image.SCALE_SMOOTH);

            return new ImageIcon(scaledImage);
        }

        /**
         * Download an icon from network or use local resourcec
         *
         * @param networkResource network address to image
         * @param localResource   resource address
         * @return the scaled image
         */
        private ImageIcon getIcon(@NotNull String networkResource, @NotNull String localResource) {
            ImageIcon icon = null;

            if (!useLocalIcons.get()) {
                final Request request = new Request.Builder()
                        .url(networkResource)
                        .get()
                        .build();

                try (Response response = MVHttpClient.getInstance().getReducedTimeOutClient().newCall(request).execute();
                     ResponseBody body = response.body()) {
                    if (response.isSuccessful() && body != null) {
                        BufferedImage b_img = ImageIO.read(body.byteStream());
                        icon = scaleImage(b_img, height);
                    } else
                        icon = null;
                } catch (Exception ex) {
                    icon = null;
                }
            }

            //if network is unreachable we get an image with size -1...
            if (icon == null || icon.getIconWidth() < 0 || icon.getIconHeight() < 0)
                icon = scaleImage(localResource, height);

            return icon;
        }

        @Override
        public Optional<ImageIcon> load(@NotNull String sender) {
            ImageIcon icon = switch (sender) {
                case "3Sat" -> getIcon(WIKI_BASE_URL + "/f/f2/3sat-Logo.svg/775px-3sat-Logo.svg.png", "/mediathek/res/sender/3sat.png");
                case "ARD", "ARD.Podcast" -> getIcon(WIKI_BASE_URL + "/6/68/ARD_logo.svg/320px-ARD_logo.svg.png", "/mediathek/res/sender/ard.png");
                case "ARTE.DE" -> getIcon(WIKI_BASE_URL + "/0/0e/Arte_Logo_2011.svg/320px-Arte_Logo_2011.svg.png", "/mediathek/res/sender/arte-de.png");
                case "ARTE.FR" -> scaleImage("/mediathek/res/sender/arte-fr.png", height);
                case "BR" -> getIcon(WIKI_BASE_URL + "/9/98/BR_Dachmarke.svg/320px-BR_Dachmarke.svg.png", "/mediathek/res/sender/br.png");
                case "HR" -> getIcon(WIKI_BASE_URL + "/6/63/HR_Logo.svg/519px-HR_Logo.svg.png", "/mediathek/res/sender/hr.png");
                case "KiKA" -> getIcon(WIKI_BASE_URL + "/f/f5/Kika_2012.svg/320px-Kika_2012.svg.png", "/mediathek/res/sender/kika.png");
                case "MDR" -> getIcon(WIKI_BASE_URL + "/6/61/MDR_Logo_2017.svg/800px-MDR_Logo_2017.svg.png", "/mediathek/res/sender/mdr.png");
                case "DW" -> getIcon(WIKI_BASE_URL + "/6/69/Deutsche_Welle_Logo.svg/743px-Deutsche_Welle_Logo.svg.png", "/mediathek/res/sender/dw.png");
                case "NDR" -> getIcon(WIKI_BASE_URL + "/0/08/NDR_Dachmarke.svg/308px-NDR_Dachmarke.svg.png", "/mediathek/res/sender/ndr.png");
                case "ORF" -> getIcon(WIKI_BASE_URL + "/d/dd/ORF_logo.svg/709px-ORF_logo.svg.png", "/mediathek/res/sender/orf.png");
                case "RBB" -> getIcon(WIKI_BASE_URL + "/7/79/Rbb_Logo_2017.08.svg/320px-Rbb_Logo_2017.08.svg.png", "/mediathek/res/sender/rbb.png");
                case "SR" -> getIcon(WIKI_BASE_URL + "/8/83/SR_Dachmarke.svg/602px-SR_Dachmarke.svg.png", "/mediathek/res/sender/sr.png");
                case "SRF" -> getIcon(WIKI_BASE_URL + "/8/84/Schweizer_Radio_und_Fernsehen_Logo.svg/559px-Schweizer_Radio_und_Fernsehen_Logo.svg.png", "/mediathek/res/sender/srf.png");
                case "SRF.Podcast" -> scaleImage("/mediathek/res/sender/srf-podcast.png", height);
                case "SWR" -> getIcon(WIKI_BASE_URL + "/6/6f/SWR_Dachmarke.svg/320px-SWR_Dachmarke.svg.png", "/mediathek/res/sender/swr.png");
                case "WDR" -> getIcon(WIKI_BASE_URL + "/9/9b/WDR_Dachmarke.svg/320px-WDR_Dachmarke.svg.png", "/mediathek/res/sender/wdr.png");
                case "ZDF" -> getIcon(WIKI_BASE_URL + "/c/c1/ZDF_logo.svg/320px-ZDF_logo.svg.png", "/mediathek/res/sender/zdf.png");
                case "ZDF-tivi" -> scaleImage("/mediathek/res/sender/zdf-tivi.png", height);
                case "PHOENIX" -> getIcon(WIKI_BASE_URL + "/d/de/Phoenix_Logo_2018_ohne_Claim.svg/640px-Phoenix_Logo_2018_ohne_Claim.svg.png", "/mediathek/res/sender/phoenix.png");
                case "Funk.net" -> getIcon(WIKI_BASE_URL + "/9/99/Funk_Logo.svg/454px-Funk_Logo.svg.png", "/mediathek/res/sender/funk_net.png");
                case "Radio Bremen TV" -> getIcon(WIKI_BASE_URL + "/7/73/Logo_Radio_Bremen_TV.svg/320px-Logo_Radio_Bremen_TV.svg.png", "/mediathek/res/sender/rbtv.jpg");
                default -> null;
            };

            final Optional<ImageIcon> optIcon;
            if (icon == null)
                optIcon = Optional.empty();
            else
                optIcon = Optional.of(icon);

            return optIcon;
        }
    }
}
