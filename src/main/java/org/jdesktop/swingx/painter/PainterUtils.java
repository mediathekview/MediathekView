package org.jdesktop.swingx.painter;

import java.awt.*;

final class PainterUtils {
    private PainterUtils() {
        //prevent instantiation
    }
    
    static Paint getForegroundPaint(Paint current, Object o) {
        if (current == null) {
            if (o instanceof Component) {
                return ((Component) o).getForeground();
            }
        }
        
        return current;
    }
    
    static Paint getBackgroundPaint(Paint current, Object o) {
        if (current == null) {
            if (o instanceof Component) {
                return ((Component) o).getBackground();
            }
        }
        
        return current;
    }
    
    static Font getComponentFont(Font current, Object o) {
        if (current == null) {
            if (o instanceof Component) {
                return ((Component) o).getFont();
            }
        }
        
        return current;
    }
}
