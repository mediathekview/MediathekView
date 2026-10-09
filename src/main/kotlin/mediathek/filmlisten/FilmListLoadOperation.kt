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

package mediathek.filmlisten

import kotlinx.coroutines.CompletableDeferred

internal class FilmListLoadOperation private constructor(
    private val loadState: FilmListLoadState?,
    private val completion: CompletableDeferred<FilmListLoadResult>?,
    val handle: FilmListLoadHandle,
) {
    fun startPostLoad() {
        loadState?.startPostLoad()
    }

    fun finish(progress: FilmListLoadProgress) {
        completion?.complete(FilmListLoadResult.finished(progress.failed))
        loadState?.finish()
    }

    fun completeExceptionally(throwable: Throwable) {
        completion?.completeExceptionally(throwable)
    }

    companion object {
        fun begin(loadState: FilmListLoadState): FilmListLoadOperation {
            if (!loadState.tryBegin()) {
                return FilmListLoadOperation(
                    loadState = null,
                    completion = null,
                    handle = FilmListLoadHandle.skipped(),
                )
            }

            val completion = CompletableDeferred<FilmListLoadResult>()
            return FilmListLoadOperation(
                loadState = loadState,
                completion = completion,
                handle = FilmListLoadHandle.started(completion),
            )
        }
    }
}
