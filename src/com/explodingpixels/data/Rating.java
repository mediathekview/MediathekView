package com.explodingpixels.data;

public enum Rating {

    NO_RATING, ONE_STAR, TWO_STARS, THREE_STARS, FOUR_STARS, FIVE_STARS;

    public static Rating getRating(int ratingInteger) {

        if (ratingInteger < 0 || 100 < ratingInteger) {
            throw new IllegalArgumentException("Rating must be between " +
                    "1 and 100");
        }

        return Rating.values()[Math.round(ratingInteger / 20)];
    }

}
