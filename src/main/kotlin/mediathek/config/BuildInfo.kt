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

package mediathek.config

import kotlinx.coroutines.*
import java.io.IOException
import java.net.URISyntaxException
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.*
import java.util.concurrent.TimeUnit

class BuildInfo internal constructor(branch: String?, commitId: String?) {
    private val branchValue = normalize(branch)
    private val commitIdValue = normalize(commitId)

    fun branch(): String = branchValue

    fun commitId(): String = commitIdValue

    fun hasGitMetadata(): Boolean = branchValue != UNKNOWN && commitIdValue != UNKNOWN

    fun formatForDisplay(): String {
        if (Konstanten.APPLICATION_TYPE == ApplicationType.PRODUCTION) {
            return commitIdValue
        }
        return "$branchValue @ $commitIdValue"
    }

    companion object {
        private const val UNKNOWN = "unknown"
        private const val GIT_PROPERTIES_PATH = "/git.properties"
        private const val GIT_COMMAND_TIMEOUT_SECONDS = 3L

        private val currentBuildInfo: BuildInfo by lazy(::load)

        @JvmStatic
        fun current(): BuildInfo = currentBuildInfo

        @JvmStatic
        internal fun fromProperties(properties: Properties): BuildInfo = BuildInfo(
            branch = properties.getProperty("git.branch"),
            commitId = properties.getProperty("git.commit.id.abbrev"),
        )

        private fun load(): BuildInfo {
            val gitBuildInfo = runBlocking { fromGitRepository() }
            if (gitBuildInfo.hasGitMetadata()) {
                return gitBuildInfo
            }

            return try {
                BuildInfo::class.java.getResourceAsStream(GIT_PROPERTIES_PATH)?.use { inputStream ->
                    fromProperties(Properties().apply { load(inputStream) })
                } ?: BuildInfo(null, null)
            } catch (_: IOException) {
                BuildInfo(null, null)
            }
        }

        private suspend fun fromGitRepository(): BuildInfo {
            for (candidate in repositoryCandidates()) {
                val repositoryRoot = findGitRepositoryRoot(candidate) ?: continue
                val buildInfo = coroutineScope {
                    val branch = async { executeGitCommand(repositoryRoot, "rev-parse", "--abbrev-ref", "HEAD") }
                    val commitId = async { executeGitCommand(repositoryRoot, "rev-parse", "--short", "HEAD") }
                    BuildInfo(branch.await(), commitId.await())
                }
                if (buildInfo.hasGitMetadata()) {
                    return buildInfo
                }
            }
            return BuildInfo(null, null)
        }

        private fun findGitRepositoryRoot(start: Path): Path? {
            var current: Path? = start
            while (current != null) {
                if (Files.exists(current.resolve(".git"))) {
                    return current
                }
                current = current.parent
            }
            return null
        }

        private fun repositoryCandidates(): List<Path> {
            val cwd = Paths.get("").toAbsolutePath()
            val classLocation = classLocationPath()
            return if (classLocation == null || classLocation == cwd) {
                listOf(cwd)
            } else {
                listOf(cwd, classLocation)
            }
        }

        private fun classLocationPath(): Path? = try {
            val location = BuildInfo::class.java.protectionDomain.codeSource?.location ?: return null
            val locationPath = Paths.get(location.toURI()).toAbsolutePath()
            if (Files.isRegularFile(locationPath)) locationPath.parent else locationPath
        } catch (_: URISyntaxException) {
            null
        } catch (_: IllegalArgumentException) {
            null
        } catch (_: SecurityException) {
            null
        }

        private suspend fun executeGitCommand(repositoryRoot: Path, vararg command: String): String? = withContext(Dispatchers.IO) {
            try {
                val process = ProcessBuilder(listOf("git", *command))
                    .directory(repositoryRoot.toFile())
                    .redirectErrorStream(true)
                    .start()

                val finished = process.waitFor(GIT_COMMAND_TIMEOUT_SECONDS, TimeUnit.SECONDS)
                if (!finished || process.exitValue() != 0) {
                    process.destroyForcibly()
                    return@withContext null
                }

                val output = process.inputStream.use { inputStream ->
                    String(inputStream.readAllBytes(), StandardCharsets.UTF_8).trim()
                }
                output.takeIf { it.isNotEmpty() }
            } catch (_: InterruptedException) {
                Thread.currentThread().interrupt()
                null
            } catch (_: IOException) {
                null
            }
        }

        private fun normalize(value: String?): String = value?.trim()?.takeIf { it.isNotEmpty() } ?: UNKNOWN
    }
}
