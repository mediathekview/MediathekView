package mediathek.tool.cellrenderer;

import com.formdev.flatlaf.extras.FlatSVGIcon;
import mediathek.config.MVColor;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenFilm;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.CompoundIcon;
import mediathek.tool.MessageBus;
import mediathek.tool.SVGIconUtilities;
import org.apache.commons.configuration2.Configuration;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;

/**
 * CellRenderer base class for all custom renderer associated with a Start.
 */
public class CellRendererBaseWithStart extends CellRendererBase {
    protected final Configuration config = ApplicationConfiguration.getConfiguration();
    protected final FlatSVGIcon lockedIcon;
    protected final FlatSVGIcon lockedIconSelected;
    protected final FlatSVGIcon unlockedIcon;
    protected final FlatSVGIcon unlockedIconSelected;
    /**
     * Temporary storage for the icons that will be assembled to a compound icon.
     */
    private final List<Icon> iconList = new ArrayList<>();
    private final FlatSVGIcon subtitleIcon;
    private final FlatSVGIcon subtitleIconSelected;
    private final FlatSVGIcon highQualityIcon;
    private final FlatSVGIcon highQualityIconSelected;
    private final FlatSVGIcon liveStreamIcon;
    private final FlatSVGIcon liveStreamIconSelected;
    private final FlatSVGIcon audioDescription;
    private final FlatSVGIcon audioDescriptionSelected;
    protected FlatSVGIcon.ColorFilter whiteColorFilter = new FlatSVGIcon.ColorFilter(color -> Color.WHITE);

    public CellRendererBaseWithStart() {
        MessageBus.getMessageBus().subscribe(this);

        lockedIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/lock.svg");
        lockedIconSelected = SVGIconUtilities.createSVGIcon("icons/fontawesome/lock.svg");
        lockedIconSelected.setColorFilter(whiteColorFilter);

        unlockedIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/lock-open.svg");
        unlockedIconSelected = SVGIconUtilities.createSVGIcon("icons/fontawesome/lock-open.svg");
        unlockedIconSelected.setColorFilter(whiteColorFilter);

        subtitleIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/closed-captioning.svg");
        subtitleIconSelected = SVGIconUtilities.createSVGIcon("icons/fontawesome/closed-captioning.svg");
        subtitleIconSelected.setColorFilter(whiteColorFilter);

        highQualityIcon = SVGIconUtilities.createSVGIcon("icons/derreisende77/high-quality.svg");
        highQualityIconSelected = SVGIconUtilities.createSVGIcon("icons/derreisende77/high-quality.svg");
        highQualityIconSelected.setColorFilter(whiteColorFilter);

        liveStreamIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/tower-cell.svg");
        liveStreamIconSelected = SVGIconUtilities.createSVGIcon("icons/fontawesome/tower-cell.svg");
        liveStreamIconSelected.setColorFilter(whiteColorFilter);

        audioDescription = SVGIconUtilities.createSVGIcon("icons/fontawesome/audio-description.svg");
        audioDescriptionSelected = SVGIconUtilities.createSVGIcon("icons/fontawesome/audio-description.svg");
        audioDescriptionSelected.setColorFilter(whiteColorFilter);
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

    /**
     * Show "CC" and/or "HQ" icon(s) when supported by the film.
     *
     * @param datenFilm  film information
     * @param isSelected is row selected.
     */
    protected void setIndicatorIcons(@NotNull DatenFilm datenFilm, boolean isSelected) {
        datenFilm.getGeo().ifPresent(geoString -> {
            if (!geoString.contains(config.getString(ApplicationConfiguration.GEO_LOCATION))) {
                //locked
                if (isSelected)
                    iconList.add(lockedIconSelected);
                else
                    iconList.add(lockedIcon);
            }
        });

        if (datenFilm.isHighQuality()) {
            if (isSelected)
                iconList.add(highQualityIconSelected);
            else
                iconList.add(highQualityIcon);
        }

        if (datenFilm.isAudioVersion()) {
            if (isSelected)
                iconList.add(audioDescriptionSelected);
            else
                iconList.add(audioDescription);
        }

        if (datenFilm.hasSubtitle()) {
            if (isSelected)
                iconList.add(subtitleIconSelected);
            else
                iconList.add(subtitleIcon);
        }

        if (datenFilm.isLivestream()) {
            if (isSelected)
                iconList.add(liveStreamIconSelected);
            else
                iconList.add(liveStreamIcon);
        }

        Icon icon;
        if (iconList.size() == 1)
            icon = iconList.get(0);
        else
            icon = new CompoundIcon(CompoundIcon.Axis.X_AXIS, 3, iconList.toArray(new Icon[0]));
        setIcon(icon);

        //always clear at the end
        iconList.clear();
    }
}
