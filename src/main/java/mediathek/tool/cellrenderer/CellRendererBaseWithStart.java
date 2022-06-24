package mediathek.tool.cellrenderer;

import com.formdev.flatlaf.extras.FlatSVGIcon;
import mediathek.config.MVColor;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenFilm;
import mediathek.gui.messages.GeoStateChangedEvent;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.MessageBus;
import mediathek.tool.SVGIconUtilities;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.configuration2.Configuration;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

/**
 * CellRenderer base class for all custom renderer associated with a Start.
 */
public class CellRendererBaseWithStart extends CellRendererBase {
    protected final Configuration config = ApplicationConfiguration.getConfiguration();
    protected final FlatSVGIcon lockedIcon;
    protected final FlatSVGIcon lockedIconSelected;
    protected final FlatSVGIcon unlockedIcon;
    protected final FlatSVGIcon unlockedIconSelected;
    protected boolean geoMelden;
    protected FlatSVGIcon.ColorFilter whiteColorFilter = new FlatSVGIcon.ColorFilter(color -> Color.WHITE);

    public CellRendererBaseWithStart() {
        MessageBus.getMessageBus().subscribe(this);
        geoMelden = config.getBoolean(ApplicationConfiguration.GEO_REPORT, false);

        lockedIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/lock.svg");

        lockedIconSelected = SVGIconUtilities.createSVGIcon("icons/fontawesome/lock.svg");
        lockedIconSelected.setColorFilter(whiteColorFilter);

        unlockedIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/lock-open.svg");

        unlockedIconSelected = SVGIconUtilities.createSVGIcon("icons/fontawesome/lock-open.svg");
        unlockedIconSelected.setColorFilter(whiteColorFilter);
    }

    protected void drawGeolocationIcons(@NotNull DatenFilm film, boolean isSelected) {
        setHorizontalAlignment(SwingConstants.CENTER);
        setText("");
        film.getGeo().ifPresentOrElse(geoString -> {
            setToolTipText(geoString);
            if (geoString.contains(config.getString(ApplicationConfiguration.GEO_LOCATION))) {
                // we are unlocked
                if (isSelected)
                    setIcon(unlockedIconSelected);
                else
                    setIcon(unlockedIcon);
            } else {
                //locked
                if (isSelected)
                    setIcon(lockedIconSelected);
                else
                    setIcon(lockedIcon);
            }
        }, () -> {
            setToolTipText("Keine Geoinformationen vorhanden");
            if (isSelected)
                setIcon(unlockedIconSelected);
            else
                setIcon(unlockedIcon);
        });
    }

    @Handler
    private void handleGeoStateChanged(GeoStateChangedEvent e) {
        SwingUtilities.invokeLater(() -> geoMelden = config.getBoolean(ApplicationConfiguration.GEO_REPORT, false));
    }

    protected void resetComponent() {
        setBackground(null);
        setForeground(null);
        setIcon(null);
        setToolTipText(null);
        setHorizontalAlignment(SwingConstants.LEADING);
    }

    protected void setBackgroundColor(final Component c, final Start s, final boolean isSelected) {
        if (s != null) {
            Color color = null;
            switch (s.status) {
                case Start.STATUS_INIT:
                    if (isSelected)
                        color = MVColor.DOWNLOAD_WAIT_SEL.color;
                    else
                        color = MVColor.DOWNLOAD_WAIT.color;
                    break;

                case Start.STATUS_RUN:
                    if (isSelected)
                        color = MVColor.DOWNLOAD_RUN_SEL.color;
                    else
                        color = MVColor.DOWNLOAD_RUN.color;
                    break;

                case Start.STATUS_FERTIG:
                    if (isSelected)
                        color = MVColor.DOWNLOAD_FERTIG_SEL.color;
                    else
                        color = MVColor.DOWNLOAD_FERTIG.color;
                    break;

                case Start.STATUS_ERR:
                    if (isSelected)
                        color = MVColor.DOWNLOAD_FEHLER_SEL.color;
                    else
                        color = MVColor.DOWNLOAD_FEHLER.color;
                    break;
            }
            c.setBackground(color);
        }
    }

    private void setGeoblockingBackgroundColor(final Component c, final boolean isSelected) {
        final Color color;
        if (isSelected)
            color = MVColor.FILM_GEOBLOCK_BACKGROUND_SEL.color;
        else
            color = MVColor.FILM_GEOBLOCK_BACKGROUND.color;

        c.setBackground(color);
    }

    protected void setupGeoblockingBackground(final Component c, final String geo, final boolean isSelected) {
        if (!geo.isEmpty()) {
            if (!geo.contains(config.getString(ApplicationConfiguration.GEO_LOCATION)))
                setGeoblockingBackgroundColor(c, isSelected);
        }
    }
}
