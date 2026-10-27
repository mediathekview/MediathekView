# Lucene search in the Films tab

This guide explains how MediathekView's Lucene search works and how to use it.

## Overview

Lucene mode searches a locally generated index. The default field is `titel`.

Queries support fields, phrases, wildcards, Boolean operators and ranges. Leading wildcards such as `*krimi` and `?atort` are allowed.

The application parses the query against the index, then also applies the interface filters for channels, dates, film duration, unwatched films, geoblocking, subscriptions and bookmarks. A film must satisfy both the query and those filters.

Some fields contain text; others contain Boolean values, dates or integers. Ordinary text searches are generally case-insensitive. Write `sender` values in lowercase, for example `sender:ard`.

Search field names and programme metadata remain unchanged when you select English.

## Query syntax

### Default field

Free text searches `titel`:

```text
tatort
"heute journal"
*krimi
```

These are equivalent to:

```text
titel:tatort
titel:"heute journal"
titel:*krimi
```

### Required, alternative and excluded terms

```text
+tatort +muenster
tatort OR polizeiruf
tatort -wien
```

### Phrases and grouping

```text
titel:"heute journal"
beschreibung:"wahre geschichten"
(titel:tatort OR titel:polizeiruf) AND sender:ard
```

### Wildcards

```text
titel:krimi*
titel:*bericht
titel:te?t
```

Leading wildcards are allowed. Many broad wildcard queries can noticeably slow searching.

### Ranges

Ranges are especially useful for dates and numeric fields:

```text
sendedatum:[20250101 TO 20251231]
länge:[1800 TO 5400]
größe:[500 TO 5000]
season:[1 TO 3]
episode:[10 TO 20]
startzeit:[18:00:00 TO 23:00:00]
```

### Boolean fields

Boolean fields store the string `true`. Use it to select matching films:

```text
livestream:true
highquality:true
untertitel:true
```

## Available search fields

| Field           | Type            | Meaning                           | Notes                               |
| --------------- | --------------- | --------------------------------- | ----------------------------------- |
| `sender`        | String          | Channel name                      | Query in lowercase                  |
| `titel`         | Text            | Film title                        | Default field                       |
| `thema`         | Text            | Topic                             | Full-text terms separated by spaces |
| `beschreibung`  | Text            | Description                       | Useful for content searches         |
| `livestream`    | Boolean         | Live stream                       | Use true                            |
| `highquality`   | Boolean         | HQ version available              | Use true                            |
| `untertitel`    | Boolean         | Subtitles available               | Use true                            |
| `trailerteaser` | Boolean         | Trailer, teaser or preview        | Use true                            |
| `audioversion`  | Boolean         | Audio description / audio version | Use true                            |
| `signlanguage`  | Boolean         | Sign-language version             | Use true                            |
| `sendedatum`    | YYYYMMDD date   | Broadcast date                    | Missing dates use 19000101          |
| `wochentag`     | Text            | Broadcast weekday                 | German names, e.g. Montag, Dienstag |
| `neu`           | Boolean         | New in the film list              | Use true                            |
| `länge`         | Integer         | Duration in seconds               | 0 if unknown                        |
| `größe`         | Integer         | File size in megabytes            | 0 if unknown                        |
| `duplicate`     | Boolean         | Classified as a duplicate         | Use true                            |
| `startzeit`     | HH:mm:ss string | Broadcast start time              | Sortable text; ranges supported     |
| `season`        | Integer         | Season number                     | 0 if unknown                        |
| `episode`       | Integer         | Episode number                    | 0 if unknown                        |

## Examples by field

### `sender`

```text
sender:ard
sender:zdf
(sender:ard OR sender:zdf) +titel:tagesschau
sender:arte AND beschreibung:dokumentation
```

Use lowercase channel names.

### `titel`

```text
titel:"heute journal"
titel:tatort*
titel:(krimi OR thriller)
+titel:reportage -titel:sport
```

### `thema`

```text
thema:nachrichten
thema:"kultur"
thema:(politik OR wirtschaft)
thema:dokumentation AND sender:3sat
```

### `beschreibung`

```text
beschreibung:"wahre begebenheit"
beschreibung:(klimawandel OR energiewende)
beschreibung:*migration*
beschreibung:geschichte AND sender:arte
```

This is often the best field for free-text content searches.

### `livestream`

```text
livestream:true
livestream:true AND sender:ard
livestream:true AND (sender:ard OR sender:zdf)
livestream:true AND sender:phoenix
```

### `highquality`

```text
highquality:true
highquality:true AND titel:tatort
highquality:true AND größe:[1000 TO 8000]
highquality:true AND sender:zdf
```

### `untertitel`

```text
untertitel:true
untertitel:true AND sender:arte
untertitel:true AND beschreibung:dokumentation
untertitel:true AND -audioversion:true
```

### `trailerteaser`

```text
trailerteaser:true
trailerteaser:true AND titel:film
trailerteaser:true AND sender:zdf
-trailerteaser:true AND titel:krimi
```

Excluding trailers with `-trailerteaser:true` is often more useful than selecting them.

### `audioversion`

```text
audioversion:true
audioversion:true AND sender:ard
audioversion:true AND untertitel:true
-audioversion:true AND titel:"heute show"
```

### `signlanguage`

```text
signlanguage:true
signlanguage:true AND sender:zdf
signlanguage:true AND beschreibung:nachrichten
-signlanguage:true AND titel:reportage
```

### `sendedatum`

```text
sendedatum:20250309
sendedatum:[20250101 TO 20250331]
sendedatum:[20240101 TO 20241231] AND sender:arte
sendedatum:[20250301 TO 20250309] AND titel:tatort
```

Always use YYYYMMDD. Missing dates are stored as 19000101.

### `wochentag`

```text
wochentag:montag
wochentag:freitag AND sender:3sat
wochentag:(samstag OR sonntag) AND thema:kinder
wochentag:dienstag AND startzeit:[20:00:00 TO 23:00:00]
```

The analyzer accepts lowercase weekday names. This field is generally absent for live streams, so it is not useful for them.

### `neu`

```text
neu:true
neu:true AND sender:ard
neu:true AND sendedatum:[20250301 TO 20250309]
neu:true AND -duplicate:true
```

### `länge`

```text
länge:[0 TO 900]
länge:[1800 TO 5400]
länge:[5400 TO 20000] AND thema:dokumentation
länge:[1200 TO 2400] AND sender:zdf
```

Use seconds: 1800 is 30 minutes; 5400 is 90 minutes.

### `größe`

```text
größe:[0 TO 500]
größe:[700 TO 3000]
größe:[3000 TO 10000] AND highquality:true
größe:[1000 TO 5000] AND titel:tatort
```

Values are megabytes.

### `duplicate`

```text
duplicate:true
duplicate:true AND sender:ard
duplicate:true AND titel:tatort
-duplicate:true AND neu:true
```

### `startzeit`

```text
startzeit:"20:15:00"
startzeit:[18:00:00 TO 22:00:00]
startzeit:[05:00:00 TO 09:00:00] AND sender:phoenix
startzeit:[20:00:00 TO 23:59:59] AND sender:ard
```

Use HH:mm:ss.

### `season`

```text
season:1
season:[1 TO 3]
season:10 AND titel:folge
season:[5 TO 8] AND episode:[1 TO 4]
```

### `episode`

```text
episode:1
episode:[1 TO 10]
episode:12 AND season:3
episode:[100 TO 200] AND sender:zdf
```

## Combined examples

### Evening crime programmes in high quality

```text
(titel:tatort OR titel:polizeiruf OR beschreibung:krimi) AND highquality:true AND startzeit:[20:00:00 TO 23:30:00]
```

### Arte documentaries with subtitles within a date range

```text
sender:arte AND (thema:dokumentation OR beschreibung:dokumentation) AND untertitel:true AND sendedatum:[20240101 TO 20251231]
```

### New episodes of a series

```text
neu:true AND season:[1 TO 20] AND episode:[1 TO 999] AND titel:"die sendung"
```

### Long reports without trailers

```text
thema:reportage AND länge:[2700 TO 20000] AND -trailerteaser:true
```

## Interaction with interface filters

You do not need to express everything in the Lucene query. The Films tab additionally applies:

- Channels selected in the filter dialog
- Date range
- Minimum and maximum duration
- Unwatched films only
- Bookmarks only
- Hide geoblocked films
- Hide subscription matches

A valid query can therefore return fewer results when interface filters are active. The interface date range is applied separately as a Lucene date filter; it is not read from the search text.

## Common pitfalls

### Channel names

Prefer `sender:ard` to `sender:ARD`.

### Numeric duration and size

Use `länge:[5400 TO 7200]`, not `länge:90min`. Both `länge` and `größe` require numbers.

### Date format

Use `sendedatum:20260309`, not `sendedatum:09.03.2026`.

### Boolean fields

Only `true` is useful in practice. To exclude live streams, use `-livestream:true`, not `livestream:false`.

## Quick reference

```text
free text                -> searches titel
field:value              -> searches a specific field
"exact phrase"           -> phrase search
* and ?                  -> wildcards
[from TO to]             -> range search
+term                    -> required term
-term                    -> excluded term
(a OR b) AND c           -> grouped logic
```

## Further reading

[Official Lucene query syntax documentation](https://lucene.apache.org/core/10_4_0/queryparser/org/apache/lucene/queryparser/classic/package-summary.html#package.description)

Lucene offers more precise filtering than classic search: combine free text in `titel` with structured fields such as `sender`, `sendedatum`, `länge`, `untertitel` and `highquality`. Numeric, date and time ranges, phrases, wildcards and Boolean operators can all be combined.
