package mediathek.tool.cellrenderer;

import com.formdev.flatlaf.extras.FlatSVGIcon;
import mediathek.controller.history.SeenHistoryController;
import mediathek.tool.SVGIconUtilities;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CellRendererBookmark extends CellRendererBaseWithStart {
    private static final Logger logger = LogManager.getLogger(CellRendererBookmark.class);
    private final SeenHistoryController history = new SeenHistoryController();
    private final FlatSVGIcon selectedDownloadIcon;
    private final FlatSVGIcon normalDownloadIcon;
    private final FlatSVGIcon selectedPlayIcon;
    private final FlatSVGIcon normalPlayIcon;

    public CellRendererBookmark(){
        selectedDownloadIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/download.svg");

        normalDownloadIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/download.svg");

        selectedPlayIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/play.svg");

        normalPlayIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/play.svg");
    }


}
