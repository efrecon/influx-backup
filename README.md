# InfluxDB Backup

## Introduction

This tool (and Docker container) will perform backup of (parts of) Influx
databases at regular interval and arrange to only keep the latest backups.  It
is capable of two completely different types of backups, based on the value of
the command-line option `-mode`.

* When the value of this option is `backup`, which is the default, a full raw
  [backup][backups] of (some of) the databases will be performed. This backup
  will also encompass the metastore.
* When the value of this option is `csv`, CSV [backups][csvexport] of dataseries
  of the databases will be performed. There are ways to perform incremental
  backups, if necessary, or to select which dataseries and/or databases are
  targeted. In order to cover a wider number of use cases and to keep storage
  under control, a number of the command-line options accepted only influence
  the behaviour of the CSV backing up operations.

  [backups]: https://docs.influxdata.com/influxdb/v1.4/administration/backup_and_restore/#backups
  [csvexport]: https://docs.influxdata.com/influxdb/v1.4/tools/shell/#specify-the-format-of-the-server-responses-with-format

Note that internal workings of this project are still under development, it has
however run in production environments. The project has only been tested
against the [OSS][influxdb] version of influx. The script has grown out of
local needs and would need structuring and improvement love for a cleaner
design. Any pull request appreciated! A [Docker] container for most versions of
[influx][tags] is also available. The implementation is forward compatible,
being able to detect past and future Alpine-based Influx images, as explained
[here](hooks/README.md).

  [influxdb]: https://www.influxdata.com/time-series-platform/influxdb/
  [Docker]: https://hub.docker.com/r/efrecon/influx-backup/
  [tags]: https://hub.docker.com/r/efrecon/influx-backup/tags/


## Example Use

This section provides quick insights into how to use this project to perform
regular backups of your Influx databases. Detailed description of the
command-line options can be found in the next section.

### Raw Backup at Regular Intervals

Provided that you are running a host at the name `influxdb` (Docker environments
come to mind here), the following command would perform backup snapshots of all
databases every half an hour, into sub-directories created under
`/mnt/external`, probably a remote file system.

    backup.tcl -host influxdb -period 1800 -root /mnt/external

### One-Shot CSV Dump

Provided the same host as above, the following command would dump only the
dataseries which contain `example` in their name as CSV.  This backup would be
created in a sub-directory of `/mnt/external`, where the first sub-directory is
the current date and the next directory the name of the database.

    backup.tcl -mode csv -host influxdb -period -1 -root /mnt/external \
               -dst "%date%/%db%" -accept "*example*" -latest ""

There will be as many CSV files as there are dataseries matching `*example*`,
and these will be roughly named as the series and compressed using `gzip`. The
final name of the files on disk is controlled by the default of an option called
`-basename` and uses a default character set coming from `-charset`).

### Accumulated CSV Conversion

Provided you have a dataserie called `example` in a database called `db` that is
updated every hour, the following command would generate an uncompressed CSV
file with all history starting from when the command was started.  This serves
as an example for incremental backups, using a specific [InfluxQL] relative
query.

    backup.tcl -mode csv -host influxdb -period 3600 -root /mnt/external \
               -dst "" -databases "db" -accept "example" -latest "" \
               -compress "" -combine on \
               -query "SELECT * from \"%serie%\" WHERE time > now() and time <= now()"

## Command-Line Options

The tool only recognises single-dash "long" command-line options. All options
are expected to take arguments, and only the last option will be taken into
account.

### `-host`

This is the name of the host of IP running Influx, it defaults to `localhost`.

### `-port`

This is the port number at the host running the Influx daemon and defaults to
`8088`.  Network connections can only occur with proper settings for
[bind-address].

  [bind-address]: https://docs.influxdata.com/influxdb/v1.4/administration/config/#bind-address-127-0-0-1-8088

### `-cliport`

This is the port number at the host for regular CLI connections, it defaults to
`8086`.  The backup utility will use this port to list databases or dataseries.

### `-username`

This option specify the name of a user with administration rights within the
system.  It defaults to an empty string as the defaults for Influx are to start
restricting authentication as soon as users are declared.

### `-password`

This option specifies the password for the user with administration rights.
Most of the time, passing the password as an argument is not convenient from a
security standpoint: use `-password_file` instead.

### `-password_file`

This option specifies the path to a file containing the password for the user
with administration rights.  This can be used with Docker [secrets].  The
`-password` option always has precedence and the content of `-password_file`
will only be taken into consideration when the value of `-password` is empty.
Trailing whitespaces (including line breaks) will automatically be removed from
the password.

  [secrets]: https://docs.docker.com/engine/swarm/secrets/

### `-root`

Points to the directory location under which all created backups will be
created. Under regular circumstances, sub-directories under `-root` will be
created for each backup and these directories will contain a timestamp. This
behaviour can be controlled to some extent using the `-dst` command-line option.
All backups directories will be created if they do not exist.

The default is to store all backups under `/backup`, which is an exported volume
when running as a Docker container.

### `-format`

Specifies how to format date timestamps when creating sub-directories for
backups.  The default is `%Y%m%d-%H%M%S`, where `%`-led tokens are placeholders
for parts of the current date and time expressed in the local timezone. For
example, `Mon Jan 2 15:04:05 UTC 2006` would translate to `20060102-15:04:05`
when run on a machine localised at `UTC`, using the default template. The
complete list of templates can be found
[here](https://www.tcl.tk/man/tcl8.6/TclCmd/clock.htm#M26).

### `-dst`

Specifies the location of the sub-directory destination to be used for each
backup being created.  This should contains paths relative to the main `-root`
of the backups.  In these paths, and depending on the backup mode, a number of
`%`-enclosed tokens can be used.  These tokens will be dynamically resolved at
the time of the backup:

* `%date%` will be resolved to the current date, specified as of the `-format`
  option.  This is valid for all forms of backups.
* `%db` or `%database%` (they are equivalent) will be resolved to the name of
  the database being backed up. This will only be used whenever relevant.
* `%serie%` will be resolved to the name of the data serie being backed up, and
  this is only relevant for `csv` backups.

### `-databases`

The value of this option should contain a space sperated list of database names,
databases that should exist at the Influx host.  When empty, the default, all
existing databases will be considered for backup and this list will be populated
each time a backup is being run in order to account for new databases being
added at the host, or databases being removed.

### `-period`

Specifies the period for backups, in seconds. The implementation targets this
period as the starting time for the backups, taking into account that backing up
operations might take some time to perform. The default is `900` seconds, i.e.
every quarter of an hour. When the period is an empty string or less than zero,
a single backup will be generated, thus allowing this script to be placed under
the control of a scheduling daemon such as `crond`.

### `-keep`

Specifies the number of backups to keep on disk. Older backups will
automatically be removed in order to keep disk space under control. Setting the
value of this option to `0` (or a negative number) will keep all backups on
disk.  The default is to keep the `3` latest backups. When removing old backups,
dates are extracted from the name of directories, through trying to scan using
the date format specified at `-format`.

### `-compress`

When set to, the default `gzip`, this will arrange for `csv` backups to be
passed further to the `gzip` executable for compression. The value of this
option should take the location of the binary, and another compression tool.
Note that when using the `-combine` option, decompression will also happen, by
the way of the `-d` command-line option to `gzip`, meaning that if another
compression binary is used, it should also support for `-d` option to decompress
and should behaves similarily to `gzip`.

Compression can be entirely turned off by setting this option to an empty string.

### `-accept` and `-reject`

The value of these two options can be used to restrict the set of dataseries
that will be backed up to `csv` files.  Each can contain a space separated list
of glob-style filters that will be used to first select across all existing
dataseries using `-accept`, then reject away some series using `-reject`.  The
defaults for these two options are `*` and the empty string, meaning that all
series will be accepted (as the `*` filter matches any string) and none will be
rejected.

### `-basename`

This option can be used to specify the basename for the CSV files that will be
created. It can contain the same `%`-surrounded tokens as for the `-dst` option.
Further sub-string substitution can occur on the resulting basename, as
described in `-map`. Character cleanup on the final resulting name will always
be performed, using the character set specified by `-charset`.

The default for `basename` is `%serie%`, which will create a csv file for each
dataserie in a database.

### `-map`

Before generating the final name of the CSV file, sub-strings can be sustituted
to others using the value of the `-map` option.  This option should take a
space-separated list of sub-strings and their replacements.  This can be used to
convert between internal representations to naming conventions that might be
more meaningfull to external clients or partners.  The default is an empty map.

### `-charset`

This contains the set of characters that are allowed in the CSV names for the
files created on disk. This set of characters is applied after all
transformations described through `-basename` and `-map`. The default is to
accept A-Z, a-z, 0-9 and a few punctuations. All characters not belonging to the
set will be replaced by an underscore.  Note that directory separators should
explicitely (and always) be out of this set.

### `-link`

When none-empty, this option will specify a path location to entertain as the
"latest" backup.  This can contain the same set of `%`-surrounded tokens as the
`-dst` option (except `%serie`) and will arrange for this location to be a
symbolic link to the latest backup directory. The implementation favours (and
computes) relative directories starting from the `-root` directory.  The default
is `latest`, which will create (and update) a directory called `latest` directly
under the `-root` directory and pointing at the latest backups.

### `-query`

The value of this option is the [InfluxQL] query that will be issued for each
data extraction when doing a CSV backup. This query will be issued for each
database and each data series as specified by the `-database`, `-accept` and
`-reject` options.

Before being issued, the query is transformed twice: All occurences of the
`%`-surrounded tokens as specified for the `-dst` will be performed. Second, all
occurences of the data-based `%`-led placeholder tokens will be replaced by
their value for at the time of the backup (see `-format` for a reference to this
tokens).

The default for this option is `select * from "%serie%"` which effectively
selects all values for a given time-serie.  Note that `%serie` will be
automatically replaced by the name of selected serie, as explained above.
Sepcifying different queries can be used to perform incremental CSV backups of
the database, by selecting data relative to the current date and time, and not
removing older backups, e.g. `-keep -1` and considering the use of `-combine`.

  [InfluxQL]: https://docs.influxdata.com/influxdb/v1.4/query_language/spec/

### `-combine`

When its value is set to `on`, this option will arrange to append the result of
the query at the end of the previous query when dumping to CSV. Using this with
`-keep` set to `-1` and using relative InfluxQL queries will result in ever
growing CSV files containing all relevant data.

Values of this option are any representation of a boolean, i.e. `on`, `off`,
`true`, `false`, `0`, `1` are all recognised values.

### `-influx`

Full path to the influx [shell], relative paths will be resolved through the
`PATH` variable. It defaults to `influx`.

  [shell]: https://docs.influxdata.com/influxdb/v1.4/tools/shell/

### `-influxd`

Full path to the `influxd` database process , relative paths will be resolved
through the `PATH` variable. It defaults to `influxd`.

## Acknowledgements

The development of this script has been sponsored by
[Lindborg Systems](http://lsys.se/) and the 
[SCOTT](https://www.scottproject.eu/) project.