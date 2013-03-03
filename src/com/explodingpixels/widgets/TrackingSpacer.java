package com.explodingpixels.widgets;

import java.awt.Dimension;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;

import javax.swing.JComponent;

/**
 * A {@link JComponent} that tracks the width or height of another component.
 */
public class TrackingSpacer extends JComponent {

    private final JComponent fComponent;
    private final TrackingDimension fTrackingDimension;
    private final int fDelta;

    /**
     * Creates a spacer component that adjusts it's width or height to the
     * given component.
     *
     * @param componentToTrack  the component to track the width or height of.
     * @param trackingDimension the dimension of the given compoonent to track.
     * @param delta             the amount to add or subtract from the given components
     *                          size (helps accomodate padding).
     */
    public TrackingSpacer(JComponent componentToTrack,
                          TrackingDimension trackingDimension,
                          int delta) {
        fComponent = componentToTrack;
        fTrackingDimension = trackingDimension;
        fDelta = delta;
        // listen for the given component to be resized.
        fComponent.addComponentListener(createComponentListner());
        // update the initial preferred size of the spacer.
        doTrackedComponentResized();
    }

    private void doTrackedComponentResized() {
        setPreferredSize(fTrackingDimension.createDimension(fComponent, fDelta));
        revalidate();
    }

    private ComponentListener createComponentListner() {
        return new ComponentAdapter() {
            @Override
            public void componentResized(ComponentEvent e) {
                doTrackedComponentResized();
            }
        };
    }

    /**
     * An enumeration representing the dimension of a component to track.
     */
    public enum TrackingDimension {
        WIDTH {
            // return a Dimension based on the given components width dimension.
            Dimension createDimension(JComponent component, int delta) {
                return new Dimension(component.getWidth() + delta, 1);
            }},
        HEIGHT {
            // return a Dimension based on the given components height dimension.
            Dimension createDimension(JComponent component, int delta) {
                return new Dimension(1, component.getHeight() + delta);
            }};

        abstract Dimension createDimension(JComponent component, int delta);
    }
}
