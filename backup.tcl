#!/usr/bin/env tclsh

package require Tcl 8.6

set options {
    -host      localhost
    -port      8088
    -cliport   8086
    -username  ""
    -password  ""
    -password_file ""
    -root      /backup
    -dst       "%date%"
    -format    "%Y%m%d-%H%M%S"
    -databases ""
    -influx    influx
    -influxd   influxd
    -period    900
    -keep      3
    -mode      "backup"
    -charset   "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_,;"
    -compress  "gzip"
    -accept    {*}
    -reject    {}
    -map       {}
    -link      latest
    -query     "select * from \"%serie%\""
    -basename  "%serie%"
    -combine   off
}

# Quick options parser
foreach {opt val} $argv {
    if { [dict exists $options $opt] } {
        dict set options $opt $val
    } else {
        puts stderr "$opt unknown option, should be [join [dict keys $options] ,\ ]"
        exit
    }
}

# CleanName -- Restrict to set of characters
#
#      Arrange to only keep the set of characters specified as -charset in the
#      global options to the program and replace characters that are not allowed
#      by a special characters.  This procedure will not modify the incoming
#      string whenever the set of characters is empty.
#
# Arguments:
#      name     Incoming string to clean away from unallowed characters
#      replace  Replacement character for all rejected chars (can be empty)
#
# Results:
#      Cleaned up string (or same as input when the charset is empty)
#
# Side Effects:
#      None.
proc CleanName { name { replace "_" } } {
    global options

    # Nothing to do whenever we empty the set of characters to keep
    if { [dict get $options -charset] eq "" } {
        return $name
    }

    set cname ""
    foreach c [split $name ""] {
        if { [string first $c [dict get $options -charset]] >= 0 } {
            append cname $c
        } else {
            append cname $replace
        }
    }
    return $cname
}


# TempName -- Generate a temporary file name
#
#      Generate a name that can be used for the creation of temporary
#      files, this name will be generated out of a (possibly empty)
#      prefix, random characters and an extension.
#
# Arguments:
#      ext      Extension for the file, with/out leading dot.
#      size     Size of the random characters
#      pfx      Prefix to lead the name
#      allowed  Set of characters to use in the name
#
# Results:
#      A good-to-use file name
#
# Side Effects:
#      None.
proc TempName { { ext "" } { size 10 } { pfx "" } { allowed "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"}} {
    set name $pfx
    for {set i 0} {$i < $size} {incr i} {
        append name [string index $allowed [expr {int(rand()*[string length $allowed])}]]
    }
    append name .[string trimleft $ext "."]
    return $name
}


# CallInflux -- Call influx CLI
#
#      Call the influx CLI, arrange to pass further relevant options
#      from the command-line.  This will arrange to add information
#      around host, port and authentication details, including reading
#      the password from the file if relevant.
#
# Arguments:
#      args     List of options and values to pass further to influx
#
# Results:
#      Call Influx
#
# Side Effects:
#      Will call influx and wait for its end before returning.
proc CallInflux { args } {
    global options

    set call {*}[auto_execok [dict get $options -influx]]
    lappend call \
        -host [dict get $options -host] \
        -port [dict get $options -cliport]
    if { [dict get $options -username] ne "" } {
        lappend call -username [dict get $options -username]
        if { [dict get $options -password] ne "" } {
            lappend call -password [dict get $options -password]
        } elseif { [dict get $options -password_file] ne "" } {
            set fd [open [dict get $options -password_file]]
            set pswd [string trim [read $fd]]
            close $fd
            lappend call -password $pswd
        }
    }

    exec {*}[concat $call $args]
} 


# CallInfluxD -- Call influxD
#
#      Call the influx daemon, arrange to pass further relevant options
#      from the command-line.  This will arrange to add information
#      around host and port.
#
# Arguments:
#      cmd      influxd subcommand to call
#      args     List of options and values to the command
#
# Results:
#      Call influxd
#
# Side Effects:
#      This will call influxd and wait for its end before returning.
proc CallInfluxD { cmd args } {
    global options

    set call {*}[auto_execok [dict get $options -influxd]]
    lappend call $cmd -host [dict get $options -host]:[dict get $options -port]

    exec {*}[concat $call $args]
} 


# Databases -- List of Influx databases
#
#      Return the list of influx databases at the host.  This is essentially a
#      wrapper around SHOW DATABASES and will properly discard system databases
#      from the returned result.
#
# Arguments:
#      None.
#
# Results:
#      List of known databases at the host
#
# Side Effects:
#      None.
proc Databases {} {
    global options

    set dbs [list]    
    puts stdout "Getting list of databases"
    if { [catch {CallInflux -execute "SHOW DATABASES"} output] == 0 } {
        # Wait until the ---- marker and then consider all databases that do not
        # start with a _ as the ones we want to backup (there is one called
        # _internal)
        set begin 0
        foreach line [split $output \n] {
            set line [string trim $line]
            if { [string match --* $line] } {
                set begin 1
            } elseif { $begin } {
                if { $line ne "" && ![string match _* $line] } {
                    lappend dbs $line
                }
            }
        }
    } else {
        puts stderr "!! Cannot list databases: $output"
    }
    
    return $dbs
}


# Series -- List of data series
#
#      Return the list of data series in a given database at the host.  This
#      procedure will only consider dataseries which name matches one of the
#      glob-style patterns in the list at the global options -accept and will
#      discard any dataserie that matches one of the patters in -reject.
#
# Arguments:
#      db       Name of database
#
# Results:
#      Return the list of dataseries which names matches the list of patterns in
#      the global options -accept and -reject.
#
# Side Effects:
#      None.
proc Series { db } {
    global options

    set series [list]    
    puts stdout "Getting data series in database '$db'"
    if { [catch {CallInflux -execute "SHOW SERIES" -database "$db" } output] == 0 } {
        # Wait until the ---- marker and then consider all databases that do not
        # start with a _ as the ones we want to backup (there is one called
        # _internal)
        set begin 0
        foreach line [split $output \n] {
            set line [string trim $line]
            if { [string match --* $line] } {
                set begin 1
            } elseif { $begin } {
                if { $line ne "" } {
                    # accept and reject based on the global options
                    set keep 0
                    foreach ptn [dict get $options -accept] {
                        if { [string match $ptn $line] } {
                            set keep 1
                            break
                        }
                    }
                    if { $keep } {
                        foreach ptn [dict get $options -reject] {
                            if { [string match $ptn $line] } {
                                set keep 0
                                break
                            }
                        }
                    }

                    # Only return name of series that we should consider
                    if { $keep } {
                        lappend series $line
                    }
                }
            }
        }
    } else {
        puts stderr "!! Cannot list data series: $output"
    }
    
    return $series
}


# AppendFileContent -- Append file content to source file
#
#      Append the content of a file to the end of a destination file,
#      possibly skipping a number of (leading) lines.
#
# Arguments:
#      dstfile  Path to destination file to append to
#      srcfile  Path to file to append from
#      skip     Number of lines to skip from srcfile
#
# Results:
#      None.
#
# Side Effects:
#      Change content of destination file
proc AppendFileContent { dstfile srcfile { skip 0 } } {
    puts stdout "Appending content of $srcfile to $dstfile (skipping $skip first line(s))"
    set d_fd [open $dstfile "a"]
    set t_fd [open $srcfile "r"]
    set l_counter 0
    while { ![eof $t_fd] } {
        set line [string trim [gets $t_fd]]
        incr l_counter
        if { $l_counter > $skip && $line ne "" } {
            puts $d_fd $line
        }
    }
    close $t_fd
    close $d_fd    
}


# CommonRoot -- Return common leading string
#
#      Compute and return the common leading string from a set
#      of strings.
#
# Arguments:
#      strings  List of strings
#
# Results:
#      The common leading string of all strings, might be empty.
#
# Side Effects:
#      None.
proc CommonRoot { strings } {
    if { [llength $strings] <= 1 } {
        return [lindex $strings 0]
    }
    
    foreach s $strings {
        lappend lengths [list $s [string length $s]]
    }
    
    set strings [list]
    foreach pair [lsort -integer -index 1 $lengths] {
        lappend strings [lindex $pair 0]
    }
    
    set common ""
    set i 0
    foreach c [split [lindex $strings 0] ""] {
        set equal 0
        foreach other [lrange $strings 1 end] {
            if { $c ne [string index $other $i] } {
                return $common
            }
        }
        append common $c
        incr i
    }
    
    return $common
}


# RemoveEmptyDirs -- Remove empty directories upwards
#
#      Remove a directory if it is empty and continue UPWARDS the
#      hierarchy until no empty dir is present.  This is usefull
#      when cleaning up (empty!) file hierarchies
#
# Arguments:
#      dir      Dir at which to start cleaning
#
# Results:
#      None.
#
# Side Effects:
#      Remove directory and parents if empty
proc RemoveEmptyDirs { dir } {
    if { [file isdirectory $dir] } {
        set content [glob -nocomplain -directory $dir -- *]
        if { [llength $content] == 0 } {
            puts stderr "Removing $dir"
            file delete -force -- $dir
            RemoveEmptyDirs [file dirname $dir]
        }
    }
}


# ::CleanUp -- Get rid of old backups
#
#      Get rid of all old backups directories to only keep the -keep latest
#      directories (where -keep is a global option).
#
# Arguments:
#      None.
#
# Results:
#      Return the list of directories that were removed.
#
# Side Effects:
#      None.
proc ::CleanUp { dbs { unlikely "!"} } {
    global options
    
    # Will contain the list of directories that were removed
    set removed [list]
    
    # Generate a token that is as long as the date and is unlikely to be found
    # in a real file path...
    set now [clock seconds]
    set date [clock format $now -format [dict get $options -format]]
    set token [string repeat $unlikely [string length $date]]
    
    foreach db $dbs {
        # Generate a glob filter which will match all possible dates
        set map [list   "%db%" $db \
                        "%database%" $db \
                        "%date%" *]
        set g_filter [file join [dict get $options -root] [string map $map [dict get $options -dst]]]
        # Collect all directories that might hold a backup using the filter above
        set all [glob -nocomplain -type d -- $g_filter]
        
        # Now use the "impossible" token to extract out the date string from the
        # path and parse it back to a timestamp, and this for all directories
        # that we could find.
        set map [list   "%db%" $db \
                        "%database%" $db \
                        "%date%" $token]
        set template [file join [dict get $options -root] [string map $map [dict get $options -dst]]]
        set timestamps [list]
        foreach backup $all {
            set idx [string first $token $template]
            set dt [string range $backup $idx [expr {$idx + [string length $date] - 1}]]
            if { [catch {clock scan $dt -format [dict get $options -format]} when] == 0 } {
                lappend timestamps $when
            }
        }
        
        # Sort in decreasing order, i.e. latest first.
        set timestamps [lsort -integer -decreasing $timestamps]
        
        # Remove old ones, keeping only -keep
        if { [llength $timestamps] > [dict get $options -keep] } {
            foreach then [lrange $timestamps [dict get $options -keep] end] {
                # Convert back the timestamp to a date and remove the target.
                set date [clock format $then -format [dict get $options -format]]
                set map [list   "%db%" $db \
                                "%database%" $db \
                                "%date%" $date]
                set dir [file join [dict get $options -root] [string map $map [dict get $options -dst]]]
                puts stdout "Removing old backup from $dir"
                file delete -force -- $dir
                lappend removed $dir
            }
        }
    }

    # Automatically remove empty directories
    if { [llength $removed] > 1 } {
        RemoveEmptyDirs [CommonRoot $removed]
    } else {
        RemoveEmptyDirs [file dirname [lindex $removed 0]]
    }
    
    return $removed
}


# ::Backup -- Perform DB backup(s)
#
#      This will backup the databases which names are passed as parameters at
#      the host.  There are two supported types of backups, which is controlled
#      by the global option -mode.  When -mode is 'backup', this will be a low-
#      level Influx backup performed using the services exposed by the influxd
#      binary.  When -mode is 'csv', this will be a set of CSV files, one for
#      each accepted dataseries in each database.  In this case, a directory
#      will be created for each database, the name of the resulting file will be
#      cleaned up so all characters are kept within the specified -charset and
#      sets of characters can be remapped using the list specified at -map.  The
#      CSV backup is able to handle / and proper directory sructures will be
#      created if necessary (out of the (transformed) data series names).  Whenever
#      -compress is not empty, the resulting file will be compressed using this
#      command-line tool (typically gzip).
#
# Arguments:
#      dstdir   Root directory where to put current backup
#      dbs      List of databases to create backups for.
#
# Results:
#      None.
#
# Side Effects:
#      Creates backups and necessary directories under the destination directory
proc ::Backup { date { dbs {}} } {
    global options
    
    switch -- [string tolower [dict get $options -mode]] {
        "backup" {
            set map [list   "%date%" $date]
            set dstdir [file join [dict get $options -root] [string map $map [dict get $options -dst]]]
            puts stdout "Backup metadata into $dstdir"
            file mkdir [file dirname $dstdir]
            catch {CallInfluxD backup $dstdir} out
            puts stderr "!! $out"
            
            foreach db $dbs {
                set map [list   "%db%" $db \
                                "%database%" $db \
                                "%date%" $date]
                set dstdir [file join [dict get $options -root] [string map $map [dict get $options -dst]]]
                puts stdout "Backup database $db into $dstdir"
                file mkdir [file dirname $dstdir]
                catch {CallInfluxD backup -database $db $dstdir} out
                puts stderr "!! $out"
            }
        }
        "csv" {
            set now [clock seconds]
            foreach db $dbs {
                foreach serie [Series $db] {
                    set map [list   "%db%" $db \
                                    "%database%" $db \
                                    "%serie%" $serie \
                                    "%date%" $date]
                    set dstdir [file join [dict get $options -root] [string map $map [dict get $options -dst]]]

                    # Select file for backup destination
                    puts stdout "Backup serie $serie from database $db into $dstdir"
                    set basename [string map $map [dict get $options -basename]]
                    set fname [CleanName [string map [dict get $options -map] $basename]].csv
                    set dstfile [file join $dstdir [string trimleft $fname /]]
                    file mkdir [file dirname $dstfile]
                    set tmpfile [file join $dstdir [TempName ".csv"]]
                    
                    # Convert special keywords in incoming query.
                    set qry [string map $map [dict get $options -query]]
                    if { [catch {clock format $now -format $qry} tqry] } {
                        puts stderr "!! Cannot convert time-data in query: $tqry"
                    } else {
                        set qry $tqry
                    }
                    
                    # Execute query and possibly compress destination.
                    if { [dict get $options -combine] } {
                        if { [dict get $options -compress] ne "" } {
                            set compressed [glob -nocomplain -- ${dstfile}?*]
                            if { [llength $compressed] } {
                                if { [catch {exec {*}[auto_execok [dict get $options -compress]] -d [lindex $compressed 0]} output] } {
                                    puts stderr "!! Could not decompress [lindex $compressed 0]: $output"
                                }
                            }
                        }
                        # Dump to CSV into a temporary file and either make this
                        # the main destination file (the first time, when it
                        # does not exist), or append the content of the
                        # temporary file to the end of the file, except the
                        # first line which is the CSV header.
                        catch {exec {*}[auto_execok [dict get $options -influx]] \
                                    -host [dict get $options -host] \
                                    -execute $qry \
                                    -database "$db" \
                                    -precision=rfc3339 \
                                    -format csv \
                                    > $tmpfile}
                        if { [file exists $dstfile] && [file size $dstfile] > 0 } {
                            AppendFileContent $dstfile $tmpfile 1
                            file delete -force -- $tmpfile
                        } else {
                            puts stderr "Moving $tmpfile to $dstfile"
                            file rename -force -- $tmpfile $dstfile
                        }
                    } else {
                        catch {exec {*}[auto_execok [dict get $options -influx]] \
                                    -host [dict get $options -host] \
                                    -execute $qry \
                                    -database "$db" \
                                    -precision=rfc3339 \
                                    -format csv \
                                    > $dstfile}
                    }
                    if { [file exists $dstfile] } {
                        if { [dict get $options -compress] ne "" } {
                            if { [catch {exec {*}[auto_execok [dict get $options -compress]] $dstfile} output] == 0 } {
                                file delete -- $dstfile
                            } else {
                                puts stderr "!! Could not compress $dstfile: $output"
                            }
                        }
                    } else {
                        puts stderr "!! Could not backup to $dstfile"
                    }
                }
            }
        }
    }
}

# ::backup -- Periodically perform backups
#
#      This will regularily create a directory for backup using the current time
#      and perform the type of backup specified by the global option at -mode.  More
#      details about the backup types can be found in the comment for the procedure
#      performing the backup itself.  Once a backup has been performed, this will arrange
#      to only keep the latest -keep backups (as long as -keep is strictly positive).
#      Finally, a symbolic link, as specified at -link is kept updated to point at the
#      latest backup.
#
# Arguments:
#      None.
#
# Results:
#      None.
#
# Side Effects:
#      Creates backups and necessary directories under the -root directory
proc ::backup {} {
    global options
    
    set t_start [clock milliseconds]
    
    # Generate name for directory to hold backup
    set now [clock seconds]
    set date [clock format $now -format [dict get $options -format]]
    #set dstdir [file join [dict get $options -root] $date]

    # Get list of databases to backup
    set dbs [dict get $options -databases]
    if { [llength $dbs] == 0 } {
        set dbs [Databases]
    }
    
    # Perform backup and clean away old backups
    Backup $date $dbs
    if { [dict get $options -keep] > 0 } {
        CleanUp $dbs
    }
    
    # Keep symbolic link if requested
    if { [dict get $options -link] ne "" } {
        foreach db $dbs {
            set map [list   "%db%" $db \
                            "%database%" $db \
                            "%date%" $date]
            set r_dstdir [string map $map [dict get $options -dst]]
            set dstdir [file join [dict get $options -root] $r_dstdir]
            set r_lnk [string map $map [dict get $options -link]]
            set lnk [file join [dict get $options -root] $r_lnk]
            set levels [expr {[regexp -all / $r_lnk]+0}]
            puts stdout "Linking $lnk to $dstdir for $db"
            file mkdir [file dirname $lnk]
            # Remove existing link, if any (it is ok to fail on type since the
            # file might not exist yet.)
            if { [catch {file type $lnk} ftype] == 0 && $ftype eq "link" } {
                file delete -force -- $lnk
            }
            set relative [string repeat ../ $levels][string trimleft $r_dstdir /]
            if { [catch {file link -symbolic $lnk $relative} res] } {
                puts stderr "!! Cannot link: $res"
            }
        }
    }

    # Estimate elapsed milliseconds for the backup operations and take this into
    # account when scheduling next backup.  This is because backups can take a long time.
    if { [dict get $options -period] eq "" || [dict get $options -period] <= 0 } {
        puts stdout "No more backups, period was '[dict get $options -period]'"        
    } else {
        set elapsed [expr {[clock milliseconds]-$t_start}]
        set next [expr {[dict get $options -period]*1000-$elapsed}]
        if { $next < 0 } {
            set next 0
        }
        puts stdout "Next backup in [expr {int($next/1000)}] seconds"    
        after $next ::backup
    }
}

# Resolve -password_file into -password so callers can use temporary files.
if { [dict get $::options -password_file] ne "" } {
    puts stdout "Reading influx password from [dict get $::options -password_file]" 
    set fd [open [dict get $::options -password_file]]
    dict set ::options -password [string trim [read $fd]]
    close $fd
}
after idle ::backup

vwait forever
