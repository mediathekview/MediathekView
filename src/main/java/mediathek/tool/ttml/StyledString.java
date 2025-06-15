/*
 * Copyright (c) 2025 derreisende77.
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

package mediathek.tool.ttml;

/* no modifier = package-private, like Kotlin's 'internal' */
public class StyledString {
    private String text;
    private String color;
    private String backgroundColor;

    /**
     * No-arg constructor with default values
     */
    public StyledString() {
        this("", "", "");
    }

    /**
     * Full constructor
     */
    public StyledString(String text, String color, String backgroundColor) {
        this.text = text;
        this.color = color;
        this.backgroundColor = backgroundColor;
    }

    // --- Getters ---
    public String getText() {
        return text;
    }

    // --- Setters ---
    public void setText(String text) {
        this.text = text;
    }

    public String getColor() {
        return color;
    }

    public void setColor(String color) {
        this.color = color;
    }

    public String getBackgroundColor() {
        return backgroundColor;
    }

    public void setBackgroundColor(String backgroundColor) {
        this.backgroundColor = backgroundColor;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof StyledString that))
            return false;
        return text.equals(that.text)
                && color.equals(that.color)
                && backgroundColor.equals(that.backgroundColor);
    }

    @Override
    public int hashCode() {
        int result = text.hashCode();
        result = 31 * result + color.hashCode();
        result = 31 * result + backgroundColor.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "StyledString{" +
                "text='" + text + '\'' +
                ", color='" + color + '\'' +
                ", backgroundColor='" + backgroundColor + '\'' +
                '}';
    }
}
