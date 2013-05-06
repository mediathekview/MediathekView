package mediathek.gui.dialog;

import com.explodingpixels.macwidgets.HudWidgetFactory;
import com.explodingpixels.macwidgets.HudWindow;
import com.explodingpixels.macwidgets.plaf.HudPaintingUtils;
import com.jgoodies.forms.layout.ColumnSpec;
import com.jgoodies.forms.layout.FormLayout;
import com.jgoodies.forms.layout.FormSpecs;
import com.jgoodies.forms.layout.RowSpec;
import mediathek.daten.DatenFilm;
import org.jdesktop.swingx.JXHyperlink;

import javax.swing.*;
import java.awt.*;
import java.net.URISyntaxException;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import mediathek.daten.DDaten;
import mediathek.tool.EscBeenden;
import mediathek.tool.UrlHyperlinkAction;

/**
 * Display the current film information in a Apple-style HUD window.
 */
public class MVFilmInformation implements ChangeListener {

    private HudWindow hud;
    private JLabel lblNrField;
    private JLabel lblSenderField;
    private JLabel lblThemaField;
    private JLabel lblTitleField;
    private JLabel lblDatumField;
    private JLabel lblTimeField;
    private JLabel lblUrlField;
    private JLabel lblUrlRtmpField;
    private JLabel lblUrlAuthField;
    private JXHyperlink lblUrlThemaField;
    private JLabel lblAboField;
    private DDaten ddaten;

    public MVFilmInformation(Frame owner, JTabbedPane tabbedPane, DDaten ddaten) {
        hud = new HudWindow("Filminformation", owner);
        hud.makeResizeable();
        this.ddaten = ddaten;
        JComponent content = buildContent();
        //prevents flickering in JDK7, JDK6 is still buggy :(
        content.setOpaque(false);
        hud.setContentPane(content);
        JDialog dialog = hud.getJDialog();
        dialog.pack();
        Dimension size = dialog.getSize();
        size.width = 500;
        dialog.setSize(size);
        calculateHudPosition();

        tabbedPane.addChangeListener(this);

        new EscBeenden(dialog) {
            @Override
            public void beenden_(JDialog d) {
                d.dispose();
            }
        };
    }

    private void calculateHudPosition() {
        //FIXME calculate the HUD position
    }

    public void show() {
        hud.getJDialog().setVisible(true);
    }

    public boolean isVisible() {
        return hud.getJDialog().isVisible();
    }

    public void updateCurrentFilm(DatenFilm film) {
        //update data
        //TODO überprüfen ob Nummeranzeige richtig geschrieben ist! Manchmal falsche Nummern
        lblNrField.setText(film.arr[DatenFilm.FILM_NR_NR]);
        lblSenderField.setText(film.arr[DatenFilm.FILM_SENDER_NR]);
        lblThemaField.setText(film.arr[DatenFilm.FILM_THEMA_NR]);
        lblTitleField.setText(film.arr[DatenFilm.FILM_TITEL_NR]);
        lblDatumField.setText(film.arr[DatenFilm.FILM_DATUM_NR]);
        lblTimeField.setText(film.arr[DatenFilm.FILM_ZEIT_NR]);
        lblUrlField.setText(film.arr[DatenFilm.FILM_URL_NR]);
        lblUrlRtmpField.setText(film.arr[DatenFilm.FILM_URL_RTMP_NR]);
        lblUrlAuthField.setText(film.arr[DatenFilm.FILM_URL_AUTH_NR]);
        // setup Hyperlink
        // FIXME ist das nicht besser zu lösen?
        try {
            lblUrlThemaField.setAction(new UrlHyperlinkAction(ddaten, film.arr[DatenFilm.FILM_URL_THEMA_NR]));
            lblUrlThemaField.setForeground(Color.WHITE);
        } catch (URISyntaxException ignored) {
        }
        lblAboField.setText(film.arr[DatenFilm.FILM_ABO_NAME_NR]);

        hud.getJDialog().repaint();
    }

    private JComponent buildContent() {
        JPanel panel = new JPanel();
        panel.setLayout(new FormLayout(new ColumnSpec[]{
            FormSpecs.RELATED_GAP_COLSPEC,
            FormSpecs.DEFAULT_COLSPEC,
            FormSpecs.RELATED_GAP_COLSPEC,
            FormSpecs.DEFAULT_COLSPEC,},
                new RowSpec[]{
            FormSpecs.RELATED_GAP_ROWSPEC,
            FormSpecs.DEFAULT_ROWSPEC,
            FormSpecs.RELATED_GAP_ROWSPEC,
            FormSpecs.DEFAULT_ROWSPEC,
            FormSpecs.RELATED_GAP_ROWSPEC,
            FormSpecs.DEFAULT_ROWSPEC,
            FormSpecs.RELATED_GAP_ROWSPEC,
            FormSpecs.DEFAULT_ROWSPEC,
            FormSpecs.RELATED_GAP_ROWSPEC,
            FormSpecs.DEFAULT_ROWSPEC,
            FormSpecs.RELATED_GAP_ROWSPEC,
            FormSpecs.DEFAULT_ROWSPEC,
            FormSpecs.RELATED_GAP_ROWSPEC,
            FormSpecs.DEFAULT_ROWSPEC,
            FormSpecs.RELATED_GAP_ROWSPEC,
            FormSpecs.DEFAULT_ROWSPEC,
            FormSpecs.RELATED_GAP_ROWSPEC,
            FormSpecs.DEFAULT_ROWSPEC,
            FormSpecs.RELATED_GAP_ROWSPEC,
            FormSpecs.DEFAULT_ROWSPEC,
            FormSpecs.RELATED_GAP_ROWSPEC,
            FormSpecs.DEFAULT_ROWSPEC,}));


        JLabel label = HudWidgetFactory.createHudLabel("Nr:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        panel.add(label, "2, 2");

        lblNrField = HudWidgetFactory.createHudLabel("");
        panel.add(lblNrField, "4, 2");

        label = HudWidgetFactory.createHudLabel("Sender:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        panel.add(label, "2, 4");

        lblSenderField = HudWidgetFactory.createHudLabel("");
        panel.add(lblSenderField, "4, 4");

        label = HudWidgetFactory.createHudLabel("Thema:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        panel.add(label, "2, 6");

        lblThemaField = HudWidgetFactory.createHudLabel("");
        panel.add(lblThemaField, "4, 6");

        label = HudWidgetFactory.createHudLabel("Titel:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        panel.add(label, "2, 8");

        lblTitleField = HudWidgetFactory.createHudLabel("");
        panel.add(lblTitleField, "4, 8");

        label = HudWidgetFactory.createHudLabel("Datum:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        panel.add(label, "2, 10");

        lblDatumField = HudWidgetFactory.createHudLabel("");
        panel.add(lblDatumField, "4, 10");

        label = HudWidgetFactory.createHudLabel("Zeit:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        panel.add(label, "2, 12");

        lblTimeField = HudWidgetFactory.createHudLabel("");
        panel.add(lblTimeField, "4, 12");

        label = HudWidgetFactory.createHudLabel("URL:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        panel.add(label, "2, 14");

        lblUrlField = HudWidgetFactory.createHudLabel("");
        panel.add(lblUrlField, "4, 14");

        label = HudWidgetFactory.createHudLabel("UrlRTMP:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        panel.add(label, "2, 16");

        lblUrlRtmpField = HudWidgetFactory.createHudLabel("");
        panel.add(lblUrlRtmpField, "4, 16");

        label = HudWidgetFactory.createHudLabel("UrlAuth:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        panel.add(label, "2, 18");

        lblUrlAuthField = HudWidgetFactory.createHudLabel("");
        panel.add(lblUrlAuthField, "4, 18");

        label = HudWidgetFactory.createHudLabel("UrlThema:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        panel.add(label, "2, 20");

        lblUrlThemaField = new JXHyperlink();
        lblUrlThemaField.setForeground(Color.WHITE);
        try {
            lblUrlThemaField.setAction(new UrlHyperlinkAction(ddaten, ""));
        } catch (URISyntaxException ignored) {
        }
        lblUrlThemaField.setFont(HudPaintingUtils.getHudFont());
        panel.add(lblUrlThemaField, "4, 20");

        label = HudWidgetFactory.createHudLabel("Abo-Name:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        panel.add(label, "2, 22");

        lblAboField = HudWidgetFactory.createHudLabel("");
        panel.add(lblAboField, "4, 22");

        panel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

        return panel;
    }

    @Override
    public void stateChanged(ChangeEvent changeEvent) {
        //Whenever there is a change event, reset HUD info to nothing
        DatenFilm emptyFilm = new DatenFilm();
        updateCurrentFilm(emptyFilm);
    }
}
