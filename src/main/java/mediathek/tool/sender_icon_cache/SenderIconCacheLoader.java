package mediathek.tool.sender_icon_cache;

import com.google.common.cache.CacheLoader;
import mediathek.tool.http.MVHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.image.BufferedImage;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;

class SenderIconCacheLoader extends CacheLoader<String, Optional<ImageIcon>> {
    private static final String WIKI_BASE_URL = "https://upload.wikimedia.org/wikipedia/commons/thumb";
    private static final Logger logger = LogManager.getLogger();
    private final AtomicBoolean useLocalIcons;

    public SenderIconCacheLoader(@NotNull AtomicBoolean useLocalIcons) {
        this.useLocalIcons = useLocalIcons;
    }

    private @Nullable ImageIcon getLocalImageIcon(@NotNull String source) {
        var url = SenderIconCacheLoader.class.getResource(source);
        if (url != null) {
            return new ImageIcon(url);
        } else
            return null;
    }

    /**
     * Download an icon from network or use local resourcec
     *
     * @param networkResource network address to image
     * @param localResource   resource address
     * @return the scaled image
     */
    private @Nullable ImageIcon getIcon(@NotNull String networkResource, @NotNull String localResource) {
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
                    icon = new ImageIcon(b_img);
                }
            } catch (Exception ex) {
                icon = null;
            }
        }

        //if network is unreachable we get an image with size -1...
        if (icon == null || icon.getIconWidth() < 0 || icon.getIconHeight() < 0) {
            var url = SenderIconCacheLoader.class.getResource(localResource);
            if (url != null)
                icon = new ImageIcon(url);
            else {
                logger.error("Could not load icon from local jar");
                icon = null;
            }
        }

        return icon;
    }

    @Override
    public @NotNull Optional<ImageIcon> load(@NotNull String sender) {
        ImageIcon icon = switch (sender) {
            case "3Sat" -> getIcon(WIKI_BASE_URL + "/f/f2/3sat-Logo.svg/775px-3sat-Logo.svg.png", "/mediathek/res/sender/3sat.png");
            case "ARD" -> getIcon(WIKI_BASE_URL + "/6/68/ARD_logo.svg/320px-ARD_logo.svg.png", "/mediathek/res/sender/ard.png");
            case "ARTE.DE" -> getIcon(WIKI_BASE_URL + "/0/0e/Arte_Logo_2011.svg/320px-Arte_Logo_2011.svg.png", "/mediathek/res/sender/arte-de.png");
            case "ARTE.EN" -> getIcon(WIKI_BASE_URL + "/0/0e/Arte_Logo_2011.svg/320px-Arte_Logo_2011.svg.png", "/mediathek/res/sender/arte-en.png");
            case "ARTE.ES" -> getIcon(WIKI_BASE_URL + "/0/0e/Arte_Logo_2011.svg/320px-Arte_Logo_2011.svg.png", "/mediathek/res/sender/arte-es.png");
            case "ARTE.FR" -> getIcon(WIKI_BASE_URL + "/0/0e/Arte_Logo_2011.svg/320px-Arte_Logo_2011.svg.png", "/mediathek/res/sender/arte-fr.png");
            case "ARTE.IT" -> getIcon(WIKI_BASE_URL + "/0/0e/Arte_Logo_2011.svg/320px-Arte_Logo_2011.svg.png", "/mediathek/res/sender/arte-it.png");
            case "ARTE.PL" -> getIcon(WIKI_BASE_URL + "/0/0e/Arte_Logo_2011.svg/320px-Arte_Logo_2011.svg.png", "/mediathek/res/sender/arte-pl.png");
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
            case "SRF.Podcast" -> getLocalImageIcon("/mediathek/res/sender/srf-podcast.png");
            case "SWR" -> getIcon(WIKI_BASE_URL + "/6/6f/SWR_Dachmarke.svg/320px-SWR_Dachmarke.svg.png", "/mediathek/res/sender/swr.png");
            case "WDR" -> getIcon(WIKI_BASE_URL + "/9/9b/WDR_Dachmarke.svg/320px-WDR_Dachmarke.svg.png", "/mediathek/res/sender/wdr.png");
            case "ZDF" -> getIcon(WIKI_BASE_URL + "/c/c1/ZDF_logo.svg/200px-ZDF_logo.svg.png", "/mediathek/res/sender/zdf.png");
            case "ZDF-tivi" -> getLocalImageIcon("/mediathek/res/sender/zdf-tivi.png");
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
