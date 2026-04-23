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

package mediathek.audiothek.ui.table

import mediathek.audiothek.model.AudioEntry
import org.apache.lucene.document.Document
import org.apache.lucene.document.StoredField
import org.apache.lucene.document.StringField
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.index.Term
import org.apache.lucene.search.*
import org.apache.lucene.store.ByteBuffersDirectory
import org.apache.lucene.store.Directory
import java.io.Closeable
import java.time.format.DateTimeFormatter

class AudiothekLuceneIndex : Closeable {
    private val directory: Directory = ByteBuffersDirectory()
    private var reader: DirectoryReader? = null
    private var entries: List<AudioEntry> = emptyList()

    fun replaceEntries(entries: List<AudioEntry>) {
        this.entries = entries
        rebuildIndex(entries)
    }

    fun search(query: String, visibleFields: List<String>): List<AudioEntry> {
        val normalized = query.trim()
        if (normalized.isEmpty()) {
            return entries
        }

        val searcher = reader?.let(::IndexSearcher) ?: return emptyList()
        val luceneQuery = buildLuceneQuery(normalized, visibleFields)
        val hits = searcher.search(luceneQuery, entries.size).scoreDocs
        return hits.mapNotNull { hit ->
            val document = searcher.storedFields().document(hit.doc)
            val rowIndex = document.getField(FIELD_ROW_INDEX)?.numericValue()?.toInt() ?: return@mapNotNull null
            entries.getOrNull(rowIndex)
        }
    }

    override fun close() {
        reader?.close()
        reader = null
        directory.close()
    }

    private fun rebuildIndex(entries: List<AudioEntry>) {
        reader?.close()
        reader = null

        val writerConfig = IndexWriterConfig().apply {
            openMode = IndexWriterConfig.OpenMode.CREATE
        }
        IndexWriter(directory, writerConfig).use { writer ->
            entries.forEachIndexed { index, entry ->
                writer.addDocument(buildDocument(index, entry))
            }
            writer.commit()
        }

        reader = DirectoryReader.open(directory)
    }

    private fun buildDocument(index: Int, entry: AudioEntry): Document {
        return Document().apply {
            add(StoredField(FIELD_ROW_INDEX, index))
            addSearchField(FIELD_SOURCE, entry.sourceLabel)
            addSearchField(FIELD_SENDER, entry.channel)
            addSearchField(FIELD_GENRE, entry.genre)
            addSearchField(FIELD_THEME, entry.theme)
            addSearchField(FIELD_TITLE, entry.title)
            addSearchField(FIELD_DATE, entry.publishedAt?.format(DATE_FORMAT).orEmpty())
            addSearchField(FIELD_TIME, entry.publishedAt?.format(TIME_FORMAT).orEmpty())
            addSearchField(FIELD_DURATION, entry.durationMinutes?.toString().orEmpty())
            addSearchField(FIELD_SIZE, entry.sizeMb?.toString().orEmpty())
        }
    }

    private fun Document.addSearchField(name: String, value: String) {
        add(StringField(name, value.lowercase(), org.apache.lucene.document.Field.Store.NO))
    }

    private fun buildLuceneQuery(query: String, visibleFields: List<String>): Query {
        val clauses = tokenizeQuery(query)
            .mapNotNull { buildClauseQuery(it, visibleFields) }

        if (clauses.isEmpty()) {
            return MatchAllDocsQuery.INSTANCE
        }

        return BooleanQuery.Builder().apply {
            clauses.forEach { add(it, BooleanClause.Occur.MUST) }
        }.build()
    }

    private fun buildClauseQuery(token: String, visibleFields: List<String>): Query? {
        val separatorIndex = token.indexOf(':')
        if (separatorIndex <= 0 || separatorIndex == token.lastIndex) {
            return buildVisibleFieldQuery(token, visibleFields)
        }

        val fieldKey = token.substring(0, separatorIndex).lowercase()
        val value = token.substring(separatorIndex + 1)
        val luceneField = SEARCH_FIELD_ALIASES[fieldKey] ?: return buildVisibleFieldQuery(token, visibleFields)
        return buildFieldQuery(luceneField, value)
    }

    private fun buildVisibleFieldQuery(value: String, visibleFields: List<String>): Query? {
        val wildcard = wildcardValue(value) ?: return null
        if (visibleFields.isEmpty()) {
            return null
        }
        return BooleanQuery.Builder().apply {
            visibleFields.forEach { field ->
                add(WildcardQuery(Term(field, wildcard)), BooleanClause.Occur.SHOULD)
            }
        }.build()
    }

    private fun buildFieldQuery(field: String, value: String): Query? {
        exactValue(value)?.let { return TermQuery(Term(field, it)) }
        val wildcard = wildcardValue(value) ?: return null
        return WildcardQuery(Term(field, wildcard))
    }

    private fun tokenizeQuery(query: String): List<String> {
        return TOKEN_REGEX.findAll(query)
            .map { it.value.trim() }
            .filter { it.isNotEmpty() }
            .toList()
    }

    private fun exactValue(value: String): String? {
        val trimmed = value.trim()
        if (trimmed.length < 2 || !trimmed.startsWith('"') || !trimmed.endsWith('"')) {
            return null
        }
        return trimmed.substring(1, trimmed.length - 1)
            .trim()
            .lowercase()
            .takeIf(String::isNotEmpty)
    }

    private fun wildcardValue(value: String): String? {
        val normalized = value.lowercase()
            .replace("*", "")
            .replace("?", "")
            .trim()
        return normalized.takeIf(String::isNotEmpty)?.let { "*$it*" }
    }

    companion object {
        const val FIELD_SOURCE = "source"
        const val FIELD_SENDER = "sender"
        const val FIELD_GENRE = "genre"
        const val FIELD_THEME = "theme"
        const val FIELD_TITLE = "title"
        const val FIELD_DATE = "date"
        const val FIELD_TIME = "time"
        const val FIELD_DURATION = "duration"
        const val FIELD_SIZE = "size"

        private const val FIELD_ROW_INDEX = "rowIndex"
        private val SEARCH_FIELD_ALIASES = mapOf(
            "quelle" to FIELD_SOURCE,
            "sender" to FIELD_SENDER,
            "genre" to FIELD_GENRE,
            "thema" to FIELD_THEME,
            "theme" to FIELD_THEME,
            "titel" to FIELD_TITLE,
            "title" to FIELD_TITLE,
            "datum" to FIELD_DATE,
            "date" to FIELD_DATE,
            "zeit" to FIELD_TIME,
            "time" to FIELD_TIME,
            "dauer" to FIELD_DURATION,
            "duration" to FIELD_DURATION,
            "groesse" to FIELD_SIZE,
            "größe" to FIELD_SIZE,
            "size" to FIELD_SIZE
        )
        private val TOKEN_REGEX = """[^\s:]+:"[^"]*"|[^\s]+""".toRegex()
        private val DATE_FORMAT: DateTimeFormatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")
        private val TIME_FORMAT: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm")

        fun build(entries: List<AudioEntry>): AudiothekLuceneIndex =
            AudiothekLuceneIndex().apply { replaceEntries(entries) }
    }
}
