/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.tool.cellrenderer;

import com.formdev.flatlaf.extras.FlatSVGIcon;
import mediathek.daten.DatenFilm;
import mediathek.swing.CompoundIcon;
import mediathek.swing.IconUtils;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.MessageBus;
import mediathek.tool.SVGIconUtilities;
import org.apache.commons.configuration2.Configuration;
import org.jspecify.annotations.NonNull;
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid;
import org.kordamp.ikonli.swing.FontIcon;

import javax.swing.*;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;

/**
 * CellRenderer base class for all custom renderer associated with a Start.
 */
public class CellRendererBaseWithStart extends CellRendererBase {
    public static final String ICON_POSITION_RIGHT = "ui.list.iconposition_right";
    private static final String INDICATOR_VISIBILITY_CACHE_KEY = "mv.renderer.indicatorVisibilityCache";
    protected final Configuration config = ApplicationConfiguration.getConfiguration();
    protected final FontIcon lockedIcon;
    protected final FontIcon lockedIconSelected;
    protected final FontIcon unlockedIcon;
    protected final FontIcon unlockedIconSelected;
    /**
     * Temporary storage for the icons that will be assembled to a compound icon.
     */
    private final List<Icon> iconList = new ArrayList<>();
    private final FontIcon subtitleIcon;
    private final FontIcon subtitleIconSelected;
    private final FlatSVGIcon highQualityIcon;
    private final FlatSVGIcon highQualityIconSelected;
    private final FontIcon liveStreamIcon;
    private final FontIcon liveStreamIconSelected;
    private final FontIcon audioDescription;
    private final FontIcon audioDescriptionSelected;

    public CellRendererBaseWithStart() {
        MessageBus.getMessageBus().subscribe(this);

        lockedIcon = IconUtils.of(FontAwesomeSolid.LOCK);
        lockedIconSelected = FontIcon.of(FontAwesomeSolid.LOCK, IconUtils.DEFAULT_SIZE, Color.WHITE);

        unlockedIcon = IconUtils.of(FontAwesomeSolid.LOCK_OPEN);
        unlockedIconSelected = FontIcon.of(FontAwesomeSolid.LOCK_OPEN, IconUtils.DEFAULT_SIZE, Color.WHITE);

        subtitleIcon = IconUtils.of(FontAwesomeSolid.CLOSED_CAPTIONING);
        subtitleIconSelected = FontIcon.of(FontAwesomeSolid.CLOSED_CAPTIONING, IconUtils.DEFAULT_SIZE, Color.WHITE);

        highQualityIcon = SVGIconUtilities.createSVGIcon("icons/derreisende77/high-quality.svg");
        highQualityIconSelected = SVGIconUtilities.createSVGIcon("icons/derreisende77/high-quality.svg");
        highQualityIconSelected.setColorFilter(new FlatSVGIcon.ColorFilter(_ -> Color.WHITE));

        liveStreamIcon = IconUtils.of(FontAwesomeSolid.BROADCAST_TOWER);
        liveStreamIconSelected = FontIcon.of(FontAwesomeSolid.BROADCAST_TOWER, IconUtils.DEFAULT_SIZE, Color.WHITE);

        audioDescription = IconUtils.of(FontAwesomeSolid.AUDIO_DESCRIPTION);
        audioDescriptionSelected = FontIcon.of(FontAwesomeSolid.AUDIO_DESCRIPTION, IconUtils.DEFAULT_SIZE, Color.WHITE);
    }

    private static boolean isColumnHidden(@NonNull JTable table, @NonNull String identifier) {
        try {
            return table.getColumn(identifier).getWidth() == 0;
        } catch (IllegalArgumentException ignored) {
            // If column does not exist in this table model, treat as hidden.
            return true;
        }
    }

    protected void drawGeolocationIcons(@NonNull DatenFilm film, boolean isSelected) {
        setHorizontalAlignment(SwingConstants.CENTER);
        setText("");
        var curLocation = ApplicationConfiguration.getInstance().getGeographicLocation();
        var lockedForCurrentLocation = film.isGeoBlockedForLocation(curLocation);
        if (lockedForCurrentLocation) {
            setToolTipText(film.hasCountries() ? film.getCountriesAsString() : "Gesperrt für " + curLocation);
            if (isSelected)
                setIcon(lockedIconSelected);
            else
                setIcon(lockedIcon);
        }
        else if (!film.hasCountries()) {
            setToolTipText("Keine Geoinformationen vorhanden");
            if (isSelected)
                setIcon(unlockedIconSelected);
            else
                setIcon(unlockedIcon);
        }
        else {
            setToolTipText(film.getCountriesAsString());
            if (isSelected)
                setIcon(unlockedIconSelected);
            else
                setIcon(unlockedIcon);
        }
    }

    private boolean filmIsCountryUnlocked(@NonNull DatenFilm film) {
        var curLocation = ApplicationConfiguration.getInstance().getGeographicLocation();
        return !film.isGeoBlockedForLocation(curLocation);
    }

    protected void resetComponent() {
        setBackground(null);
        setForeground(null);
        setIcon(null);
        setToolTipText(null);
        setHorizontalAlignment(SwingConstants.LEADING);
    }

    /**
     * Show "CC" and/or "HQ" icon(s) when supported by the film.
     *
     * @param datenFilm  film information
     * @param isSelected is row selected.
     */
    protected void setIndicatorIcons(@NonNull JTable table, @NonNull DatenFilm datenFilm, boolean isSelected) {
        var visibility = getIndicatorColumnVisibility(table);
        setIndicatorIcons(datenFilm, isSelected, visibility.hqColumnHidden, visibility.utColumnHidden);
    }

    protected void setIndicatorIcons(@NonNull DatenFilm datenFilm, boolean isSelected, boolean hqColumnHidden, boolean utColumnHidden) {
        if (!filmIsCountryUnlocked(datenFilm)) {
            //locked
            if (isSelected)
                iconList.add(lockedIconSelected);
            else
                iconList.add(lockedIcon);
        }

        // if HQ column is NOT visible add icon
        if (hqColumnHidden) {
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

        //if UT column is NOT visible
        if (utColumnHidden) {
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

    private IndicatorColumnVisibility getIndicatorColumnVisibility(@NonNull JTable table) {
        var cache = (IndicatorVisibilityCache) table.getClientProperty(INDICATOR_VISIBILITY_CACHE_KEY);
        if (cache == null || cache.columnModel != table.getColumnModel()) {
            cache = new IndicatorVisibilityCache(table);
            table.putClientProperty(INDICATOR_VISIBILITY_CACHE_KEY, cache);
        }
        return cache.get();
    }

    private record IndicatorColumnVisibility(boolean hqColumnHidden, boolean utColumnHidden) {
    }

    private static final class IndicatorVisibilityCache implements TableColumnModelListener {
        private final JTable table;
        private final javax.swing.table.TableColumnModel columnModel;
        private boolean dirty = true;
        private IndicatorColumnVisibility visibility = new IndicatorColumnVisibility(true, true);

        private IndicatorVisibilityCache(@NonNull JTable table) {
            this.table = table;
            this.columnModel = table.getColumnModel();
            this.columnModel.addColumnModelListener(this);
        }

        private IndicatorColumnVisibility get() {
            if (dirty) {
                visibility = new IndicatorColumnVisibility(
                        isColumnHidden(table, "HQ"),
                        isColumnHidden(table, "UT")
                );
                dirty = false;
            }
            return visibility;
        }

        private void invalidate() {
            dirty = true;
        }

        @Override
        public void columnAdded(TableColumnModelEvent e) {
            invalidate();
        }

        @Override
        public void columnRemoved(TableColumnModelEvent e) {
            invalidate();
        }

        @Override
        public void columnMoved(TableColumnModelEvent e) {
            invalidate();
        }

        @Override
        public void columnMarginChanged(javax.swing.event.ChangeEvent e) {
            invalidate();
        }

        @Override
        public void columnSelectionChanged(javax.swing.event.ListSelectionEvent e) {
            // selection does not affect visibility
        }
    }
}
