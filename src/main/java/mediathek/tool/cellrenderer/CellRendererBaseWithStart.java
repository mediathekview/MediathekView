package mediathek.tool.cellrenderer;

import mediathek.config.Daten;
import mediathek.config.MVColor;
import mediathek.controller.starter.Start;
import mediathek.gui.messages.GeoStateChangedEvent;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.MVSenderIconCache;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.configuration2.Configuration;

import javax.swing.*;
import java.awt.*;

/**
 * CellRenderer base class for all custom renderer associated with a Start.
 */
public class CellRendererBaseWithStart extends CellRendererBase {
    private static final long serialVersionUID = 1659689253119935809L;
    protected final Configuration config = ApplicationConfiguration.getConfiguration();
    protected boolean geoMelden;

    public CellRendererBaseWithStart(MVSenderIconCache cache) {
        super(cache);

        Daten.getInstance().getMessageBus().subscribe(this);

        geoMelden = config.getBoolean(ApplicationConfiguration.GEO_REPORT, false);
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

    protected void setGeoblockingBackgroundColor(final Component c, final boolean isSelected) {
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
