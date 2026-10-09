package mediathek.gui.tabs.tab_film.lifecycle

import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

internal class BookmarkStartupReloadCoordinatorTest {

    @Test
    fun nonBookmarkedFilterReloadsImmediatelyAfterFilmListLoad() {
        val coordinator = BookmarkStartupReloadCoordinator()

        coordinator.onFilmListLoadingStarted()

        assertTrue(coordinator.onFilmListLoaded(false))
        assertFalse(coordinator.onBookmarkRefreshCompleted(false))
    }

    @Test
    fun bookmarkedOnlyFilterWaitsForBookmarkRefreshBeforeReload() {
        val coordinator = BookmarkStartupReloadCoordinator()

        coordinator.onFilmListLoadingStarted()

        assertFalse(coordinator.onFilmListLoaded(true))
        assertTrue(coordinator.onBookmarkRefreshCompleted(true))
    }

    @Test
    fun unrelatedBookmarkRefreshDoesNotReloadWithoutPendingStartupWait() {
        val coordinator = BookmarkStartupReloadCoordinator()

        assertFalse(coordinator.onBookmarkRefreshCompleted(true))
        coordinator.onFilmListLoadingStarted()
        assertFalse(coordinator.onBookmarkRefreshCompleted(true))
    }

    @Test
    fun bookmarkedOnlyStartupReloadFiresOnlyOncePerLoadCycle() {
        val coordinator = BookmarkStartupReloadCoordinator()

        coordinator.onFilmListLoadingStarted()

        assertFalse(coordinator.onFilmListLoaded(true))
        assertTrue(coordinator.onBookmarkRefreshCompleted(true))
        assertFalse(coordinator.onBookmarkRefreshCompleted(true))
    }
}
