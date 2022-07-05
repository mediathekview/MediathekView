package org.jdesktop.swingx.graphics;

import org.jdesktop.swingx.util.Contract;

import java.awt.*;
import java.awt.image.*;

/**
 * A {@code FilterComposite} allows the inclusion of arbitrary image filters during the paint
 * processing of {@link java.awt.Graphics2D} events. By adding a filter composite, the src and
 * destination images are render using a delegated {@code Composite}, then post-processed with the
 * filters before returning the result back to the graphics context. This process adds overhead to
 * the painting both is terms of time (the actual processing time) and memory (as a temporary raster
 * must be created to store the intermediate state). Since it is possible to delegate to a filter
 * composite from a filter composite, this may result slow or unresponsive painting. If you are
 * attempting to render with many different filters, it may be better to have one filter composite
 * with many filters (using a compound filter).
 * <p>
 * It was decided to use {@link BufferedImageOp} as the filter because many of these filters already
 * exist. This gives high reusability to code.
 * 
 * @author Karl Schaefer
 * @see org.jdesktop.swingx.image.AbstractFilter
 */
@SuppressWarnings("nls")
public class FilterComposite implements Composite {
    private static class FilterContext implements CompositeContext {
        private ColorModel dstModel;
        private CompositeContext ctx;
        private BufferedImageOp filter;
        
        public FilterContext(ColorModel dstModel, CompositeContext ctx, BufferedImageOp filter) {
            Contract.asNotNull(dstModel, "dstModel cannot be null");
            Contract.asNotNull(ctx, "context cannot be null");
            this.dstModel = dstModel;
            this.ctx = ctx;
            this.filter = filter;
        }

        @Override
        public void compose(Raster src, Raster dstIn, WritableRaster dstOut) {
            if (filter == null) {
                ctx.compose(src, dstIn, dstOut);
            } else {
                WritableRaster tempOut = dstModel.createCompatibleWritableRaster(dstOut.getWidth(), dstOut.getHeight());
                ctx.compose(src, dstIn, tempOut);
                
                filter.filter(new BufferedImage(dstModel, tempOut, dstModel.isAlphaPremultiplied(), null),
                        new BufferedImage(dstModel, dstOut, dstModel.isAlphaPremultiplied(), null));
            }
        }
        
        @Override
        public void dispose() {
            ctx = null;
        }
    }
    
    private final Composite composite;
    private BufferedImageOp filter;
    
    /**
     * Creates an empty filter composite for the specified composite.
     * 
     * @param composite
     *            the composite operation to perform prior to filtering
     * @throws NullPointerException
     *             if {@code composite} is {@code null}
     */
    public FilterComposite(Composite composite) {
        this(composite, null);
    }
    
    /**
     * Creates a filter for the specified composite.
     * 
     * @param composite
     *            the composite operation to perform prior to filtering
     * @param filter
     *            the filter to apply to the composite result
     * @throws NullPointerException
     *             if {@code composite} is {@code null}
     */
    public FilterComposite(Composite composite, BufferedImageOp filter) {
        Contract.asNotNull(composite, "composite cannot be null");
        this.composite = composite;
        this.filter = filter;
    }
    
    /**
     * The filter to apply to the graphics context.
     * 
     * @return the current filter
     */
    public BufferedImageOp getFilter() {
        return filter;
    }
    
    /**
     * Sets the filter for manipulating the graphics composites.
     * <p>
     * A {@code null} filter will result in no filtering.
     * 
     * @param filter
     *            the new filter
     */
    public void setFilter(BufferedImageOp filter) {
        this.filter = filter;
    }
    
    /**
     * {@inheritDoc}
     */
    @Override
    public CompositeContext createContext(ColorModel srcColorModel, ColorModel dstColorModel,
            RenderingHints hints) {
        return new FilterContext(dstColorModel, composite.createContext(srcColorModel, dstColorModel, hints), filter);
    }
}
