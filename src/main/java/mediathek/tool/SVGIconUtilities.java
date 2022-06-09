package mediathek.tool;

import com.formdev.flatlaf.extras.FlatSVGIcon;

public class SVGIconUtilities {
    public static FlatSVGIcon createSVGIcon(String resource) {
        FlatSVGIcon icon = new FlatSVGIcon(resource);
        float scaleFactor = (1f / icon.getIconHeight()) * 15f;
        return icon.derive(scaleFactor);
    }
}
