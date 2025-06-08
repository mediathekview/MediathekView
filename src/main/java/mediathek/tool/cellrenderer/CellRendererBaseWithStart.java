package mediathek.tool.cellrenderer;

import com.formdev.flatlaf.extras.FlatSVGIcon;
import mediathek.config.MVColor;
import mediathek.controller.starter.Start;
import mediathek.daten.Country;
import mediathek.daten.DatenFilm;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.MessageBus;
import mediathek.tool.SVGIconUtilities;
import mediathek.tool.swing.CompoundIcon;
import org.apache.commons.configuration2.Configuration;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.stream.Collectors;

/**
 * CellRenderer base class for all custom renderer associated with a Start.
 */
public class CellRendererBaseWithStart extends CellRendererBase {
    public static final String ICON_POSITION_RIGHT = "ui.list.iconposition_right";
    private static final EnumSet<Country> euCountryList = EnumSet.of(Country.DE, Country.AT, Country.FR);
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
    protected final FlatSVGIcon.ColorFilter whiteColorFilter = new FlatSVGIcon.ColorFilter(color -> Color.WHITE);

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
        if (film.countrySet.isEmpty()) {
            setToolTipText("Keine Geoinformationen vorhanden");
            if (isSelected)
                setIcon(unlockedIconSelected);
            else
                setIcon(unlockedIcon);
        }
        else {
            var geoString = film.countrySet.stream().map(Country::toString).collect(Collectors.joining("-"));
            setToolTipText(geoString);
            if (filmIsCountryUnlocked(film)) {
                //we are unlocked
                if (isSelected)
                    setIcon(unlockedIconSelected);
                else
                    setIcon(unlockedIcon);
            }
            else {
                // locked
                if (isSelected)
                    setIcon(lockedIconSelected);
                else
                    setIcon(lockedIcon);
            }
        }
    }

    private boolean filmIsCountryUnlocked(@NotNull DatenFilm film) {
        var curLocation = ApplicationConfiguration.getInstance().getGeographicLocation();
        //EU consists of many states therefore we have to extend the country test...
        if (film.countrySet.contains(Country.EU)) {
            return film.countrySet.contains(curLocation) || euCountryList.contains(curLocation);
        }
        else {
            return film.countrySet.contains(curLocation);
        }
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
                case Start.STATUS_INIT -> {
                    if (isSelected)
                        color = MVColor.DOWNLOAD_WAIT_SEL.color;
                    else
                        color = MVColor.DOWNLOAD_WAIT.color;
                }
                case Start.STATUS_RUN -> {
                    if (isSelected)
                        color = MVColor.DOWNLOAD_RUN_SEL.color;
                    else
                        color = MVColor.DOWNLOAD_RUN.color;
                }
                case Start.STATUS_FERTIG -> {
                    if (isSelected)
                        color = MVColor.DOWNLOAD_FERTIG_SEL.color;
                    else
                        color = MVColor.DOWNLOAD_FERTIG.color;
                }
                case Start.STATUS_ERR -> {
                    if (isSelected)
                        color = MVColor.DOWNLOAD_FEHLER_SEL.color;
                    else
                        color = MVColor.DOWNLOAD_FEHLER.color;
                }
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
    protected void setIndicatorIcons(@NotNull JTable table, @NotNull DatenFilm datenFilm, boolean isSelected) {
        if (!datenFilm.countrySet.isEmpty()) {
            if (!filmIsCountryUnlocked(datenFilm)) {
                //locked
                if (isSelected)
                    iconList.add(lockedIconSelected);
                else
                    iconList.add(lockedIcon);
            }
        }

        var tc = table.getColumn("HQ");
        // if HQ column is NOT visible add icon
        if (tc.getWidth() == 0) {
            if (datenFilm.isHighQuality()) {
                if (isSelected)
                    iconList.add(highQualityIconSelected);
                else
                    iconList.add(highQualityIcon);
            }
        }

        if (datenFilm.isAudioVersion()) {
            if (isSelected)
                iconList.add(audioDescriptionSelected);
            else
                iconList.add(audioDescription);
        }

        tc = table.getColumn("UT");
        //if UT column is NOT visible
        if (tc.getWidth() == 0) {
            if (datenFilm.hasSubtitle()) {
                if (isSelected)
                    iconList.add(subtitleIconSelected);
                else
                    iconList.add(subtitleIcon);
            }
        }

        if (datenFilm.isLivestream()) {
            if (isSelected)
                iconList.add(liveStreamIconSelected);
            else
                iconList.add(liveStreamIcon);
        }

        Icon icon;
        if (iconList.size() == 1)
            icon = iconList.getFirst();
        else
            icon = new CompoundIcon(CompoundIcon.Axis.X_AXIS, 3, iconList.toArray(new Icon[0]));
        setIcon(icon);

        final int position = ApplicationConfiguration.getConfiguration()
                .getBoolean(ICON_POSITION_RIGHT, false) ? SwingConstants.LEADING : SwingConstants.TRAILING;
        setHorizontalTextPosition(position);
        //always clear at the end
        iconList.clear();
    }
}
