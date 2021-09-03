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
    -pending   ".pending"
    -cleanup   off
    -format    "%Y%m%d-%H%M%S"
    -databases ""
    -influx    influx
    -influxd   influxd
    -period    900
    -wait      0
    -cron      {}
    -keep      3
    -mode      "backup"
    -charset   "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_,;"
    -compress  "gzip"
    -accept    {*}
    -reject    {}
    -map       {}
    -link      latest
    -query     "select * from \"%measurement%\""
    -basename  "%measurement%"
    -combine   off
    -portable  ""
    -separator ","
    -quote     "\""
    -log       stderr
    -verbose   INFO
    -logtstamp "%Y%m%d-%H%M%S"
}

# This is as per https://golang.org/pkg/encoding/csv/ and RFC4180, as this is
# what the InfluxDB implementation uses.
set builtins {
    separator   ","
    quote       "\""
}

##### Following code from https://wiki.tcl-lang.org/page/Converting+human+time+durations
proc HowLong {len unit} {
    if { [string is integer -strict $len] } {
        switch -glob -- $unit {
            "\[Yy\]*" {
                return [expr {$len*31536000}];   # Leap years?
            }
            "\[Mm\]\[Oo\]*" -
            "m*" {
                return [expr {$len*2592000}]
            }
            "\[Ww\]*" {
                return [expr {$len*604800}]
            }
            "\[Dd\]*" {
                return [expr {$len*86400}]
            }
            "\[Hh\]*" {
                return [expr {$len*3600}]
            }
            "\[Mm\]\[Ii\]*" -
            "M" {
                return [expr {$len*60}]
            }
            "\[Ss\]*" {
                return $len
            }
        }
    }
    return 0
}


proc Duration { str } {
    set words {}
    while {[scan $str %s%n word length] == 2} {
        lappend words $word
        set str [string range $str $length end]
    }

    set seconds 0
    for {set i 0} {$i<[llength $words]} {incr i} {
        set f [lindex $words $i]
        if { [scan $f %d%n n length] == 2 } {
            set unit [string range $f $length end]
            if { $unit eq "" } {
                incr seconds [HowLong $n [lindex $words [incr i]]]
            } else {
                incr seconds [HowLong $n $unit]
            }
        }
    }

    return $seconds
}
##### End of code from https://wiki.tcl-lang.org/page/Converting+human+time+durations


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

    Log DEBUG "Calling influx with args: $args"
    set call {*}[auto_execok [dict get $options -influx]]
    lappend call \
        -host [dict get $options -host] \
        -port [dict get $options -cliport]
    Log TRACE $call
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

    Log TRACE "[concat $call $args]"
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

    Log DEBUG "Calling influxd with args: $args"
    set call {*}[auto_execok [dict get $options -influxd]]
    lappend call $cmd -host [dict get $options -host]:[dict get $options -port]

    Log TRACE "[concat $call $args]"
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
    Log NOTICE "Getting list of databases"
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
        Log ERROR "Cannot list databases: $output"
    }

    return $dbs
}


# Measurements -- List of Measurements
#
#      Return the list of Measurements in a given database at the host.  This
#      procedure will only consider measurements which name matches one of the
#      glob-style patterns in the list at the global options -accept and will
#      discard any dataserie that matches one of the patters in -reject.
#
# Arguments:
#      db       Name of database
#
# Results:
#      Return the list of measurements which names matches the list of patterns in
#      the global options -accept and -reject.
#
# Side Effects:
#      None.
proc Measurements { db } {
    global options

    set measurements [list]
    Log INFO "Getting measurements in database '$db'"
    if { [catch {CallInflux -execute "SHOW MEASUREMENTS" -database "$db" } output] == 0 } {
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

                    # Only return name of measurements that we should consider
                    if { $keep } {
                        lappend measurements $line
                    }
                }
            }
        }
    } else {
        Log ERROR "Cannot list measurements: $output"
    }

    return $measurements
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
    Log DEBUG "Appending content of $srcfile to $dstfile (skipping $skip first line(s))"
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
            Log NOTICE "Removing $dir"
            file delete -force -- $dir
            RemoveEmptyDirs [file dirname $dir]
        }
    }
}


# ::CleanUpDB -- descr
#
#      descr
#
# Arguments:
#      db       descr
#      date     descr
#      token    descr
#
# Results:
#      None.
#
# Side Effects:
#      None.
proc ::CleanUpDB { db date token } {
    global options

    # Will contain the list of directories that were removed
    set removed [list]

    # Generate a glob filter which will match all possible dates
    set map [list "%date%" *]
    if { $db ne "" } {
        lappend map "%db%" $db \
                    "%database%" $db
    }
    set g_filter [file join [dict get $options -root] [string map $map [dict get $options -dst]]]
    # Collect all directories that might hold a backup using the filter above
    set all [glob -nocomplain -type d -- $g_filter]

    # Now use the "impossible" token to extract out the date string from the
    # path and parse it back to a timestamp, and this for all directories
    # that we could find.
    set map [list "%date%" $token]
    if { $db ne "" } {
        lappend map "%db%" $db \
                    "%database%" $db
    }
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
            set map [list "%date%" $date]
            if { $db ne "" } {
                lappend map "%db%" $db \
                            "%database%" $db
            }
            set dir [file join [dict get $options -root] [string map $map [dict get $options -dst]]]
            Log INFO "Removing old backup from $dir"
            file delete -force -- $dir
            lappend removed $dir
        }
    }

    return $removed
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

    if { [llength $dbs] } {
        foreach db $dbs {
            set removed [concat $removed [CleanUpDB $db $date $token]]
        }
    } else {
        set removed [CleanUpDB "" $date $token]
    }

    # Automatically remove empty directories
    if { [llength $removed] > 1 } {
        RemoveEmptyDirs [CommonRoot $removed]
    } else {
        RemoveEmptyDirs [file dirname [lindex $removed 0]]
    }

    return $removed
}


# ::CSVSplit -- Split CSV line into fields.
#
#      This is taken and adapted from http://wiki.tcl.tk/2215, refer to the wiki
#      for more information. The procedure below splits a line in CSV format
#      into the fields that it contains, provided the quoting and separation
#      characters passed as arguments.  Parsing occurs character by character,
#      which might be slow. The procedure has been adapted in order to handle
#      various quoting or separator characters.
#
# Arguments:
#      line      Line to split into its CSV fields
#      quote     Quoting character
#      separator Character used to separate fields
#
# Results:
#      A list with the fields contained in the line
#
# Side Effects:
#      Returns an error for fields that spans across lines.
proc ::CSVSplit {line {quote "\""} {separator ","}} {
    # Process each input character.
    set result [list]
    set beg 0
    set rx [string map [list @separator@ $separator] {.*?(?=@separator@|$)}]
    while {$beg < [string length $line]} {
        if {[string index $line $beg] eq $quote} {
            incr beg
            set quote false
            set word {}
            foreach char [concat [split [string range $line $beg end] {}] {{}}] {
                # Search forward for the closing quote, one character at a time.
                incr beg
                if {$quote} {
                    if {$char in [list $separator {}] } {
                        # Quote followed by comma or end-of-line indicates the end of
                        # the word.
                        break
                    } elseif {$char eq $quote} {
                        # Pair of quotes is valid.
                        append word $char
                    } else {
                        # No other characters can legally follow quote.  I think.
                        error "extra characters after close-quote"
                    }
                    set quote false
                } elseif {$char eq {}} {
                    # End-of-line inside quotes indicates embedded newline.
                    error "embedded newlines not supported"
                } elseif {$char eq $quote} {
                    # Treat the next character specially.
                    set quote true
                } else {
                    # All other characters pass through directly.
                    append word $char
                }
            }
            lappend result $word
        } else {
            # Use all characters up to the comma or line ending.
            regexp -start $beg $rx $line word
            lappend result $word
            set beg [expr {$beg + [string length $word] + 1}]
        }
    }

    # If the line ends in a comma, add a blank word to the result.
    if {[string index $line end] eq $separator} {
       lappend result {}
    }

    # Done.  Return the result list.
    return $result
}

# ::CSVQuote -- CSV quote a field
#
#      Arrange for minimial quoting of a field so that it can be appended to a
#      CSV file.
#
# Arguments:
#      field     Field to be quoted
#      quote     Quoting character
#      separator Character used to separate fields in the CSV.
#
# Results:
#      Quoted field
#
# Side Effects:
#      None.
proc ::CSVQuote { field {quote "\""} {separator ","} } {
    set field [string map [list $quote $quote$quote] $field]
    if { [string first $separator $field] >= 0 || [string first $quote $field] >= 0 } {
        return $quote$field$quote
    }

    return $field
}


# ::CSVConvert -- Convert CSV file if necessary
#
#      Arranges to convert the CSV file passed as a parameter so that it
#      contains the quoting and separator characters that are requested at the
#      command line. Conversion will only be performed if one of the options
#      differs from the characters used in the original file.
#
# Arguments:
#      infile   Path to incoming CSV file (generated by Influx)
#      tmpdir   Temporary directory where to place a temp file during conversion
#
# Results:
#      1 the file was converted, 0 otherwise
#
# Side Effects:
#      Uses a temporary file for conversion and replaces the original file
proc ::CSVConvert { infile tmpdir } {
    global options builtins

    # We convert only if the separator or quoting characters requested at the
    # command-line are different from the ones that are set in stone by Influx
    if { ( [dict get $options -separator] eq "" || [dict get $options -separator] eq [dict get $builtins separator] ) \
            && ( [dict get $options -quote] eq "" || [dict get $options -quote] eq [dict get $builtins quote] ) } {
        return 0
    } else {
        set tmpfile [file join $tmpdir [TempName ".csv"]]
        Log DEBUG "CSV Converting $infile via $tmpfile"
        set in [open $infile]
        set out [open $tmpfile "w"]
        while {![eof $in]} {
            set line [gets $in]
            set converted ""
            foreach field [CSVSplit $line [dict get $builtins quote] [dict get $builtins separator]] {
                append converted [CSVQuote $field [dict get $options -quote] [dict get $options -separator]] [dict get $options -separator]
            }
            set converted [string trimright $converted [dict get $options -separator]]
            puts $out $converted
        }
        close $in
        close $out
        if { [catch {file rename -force -- $tmpfile $infile} err] } {
            Log ERROR "$err"
        }
        return 1
    }
}

proc ::AutoClean {} {
    global options

    if { [dict get $options -pending] ne "" } {
        set map [list "%date%" "*"]
        set dirptn [file join [dict get $options -root] [string map $map [dict get $options -dst]][dict get $options -pending]]
        foreach dir [glob -nocomplain -types d -- $dirptn] {
            if { [dict get $options -cleanup] } {
                Log NOTICE "Removing old temporary directory at $dir"
                file delete -force -- $dir
            } else {
                Log WARN "Consider manually removing temporary directory at $dir"
            }
        }
    }
}

# ::Backup -- Perform DB backup(s)
#
#      This will backup the databases which names are passed as parameters at
#      the host.  There are two supported types of backups, which is controlled
#      by the global option -mode.  When -mode is 'backup', this will be a low-
#      level Influx backup performed using the services exposed by the influxd
#      binary.  When -mode is 'csv', this will be a set of CSV files, one for
#      each accepted datameasurements in each database.  In this case, a directory
#      will be created for each database, the name of the resulting file will be
#      cleaned up so all characters are kept within the specified -charset and
#      sets of characters can be remapped using the list specified at -map.  The
#      CSV backup is able to handle / and proper directory sructures will be
#      created if necessary (out of the (transformed) data measurements names).  Whenever
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
            set map [list "%date%" $date]
            set dstdir [file join [dict get $options -root] [string map $map [dict get $options -dst]]]
            if { [dict get $options -pending] ne "" } {
                set tmpdir ${dstdir}[dict get $options -pending]
                set bkpdir $tmpdir
            } else {
                set bkpdir $dstdir
            }
            if { [dict get $::options -portable] && [llength $dbs] == 0 } {
                Log INFO "Backup up entire database into $dstdir"
                file mkdir [file dirname $bkpdir]
                catch {CallInfluxD backup -portable $bkpdir} out
                Log ERROR "$out"
                if { [dict get $options -pending] ne "" } {
                    if { [catch {file rename -force -- $bkpdir $dstdir} err] } {
                        Log ERROR "$err"
                        file delete -force -- $bkpdir
                    }
                }
            } else {
                if { ! [dict get $::options -portable] } {
                    Log INFO "Backup metadata into $dstdir"
                    file mkdir [file dirname $dstdir]
                    catch {CallInfluxD backup $dstdir} out
                    Log ERROR "$out"
                }

                foreach db $dbs {
                    set map [list   "%db%" $db \
                                    "%database%" $db \
                                    "%date%" $date]
                    set dstdir [file join [dict get $options -root] [string map $map [dict get $options -dst]]]
                    if { [dict get $options -pending] ne "" } {
                        set tmpdir ${dstdir}[dict get $options -pending]
                        set bkpdir $tmpdir
                    } else {
                        set bkpdir $dstdir
                    }
                    Log INFO "Backup database $db into $dstdir"
                    file mkdir [file dirname $bkpdir]
                    if { [dict get $::options -portable] } {
                        catch {CallInfluxD backup -database $db -portable $bkpdir} out
                    } else {
                        catch {CallInfluxD backup -database $db $bkpdir} out
                    }
                    Log ERROR "$out"
                    if { [dict get $options -pending] ne "" } {
                        if { [catch {file rename -force -- $bkpdir $dstdir} err] } {
                            Log ERROR "$err"
                            file delete -force -- $bkpdir
                        }
                    }
                }
            }
        }
        "csv" {
            set now [clock seconds]
            foreach db $dbs {
                foreach measurement [Measurements $db] {
                    set map [list   "%db%" $db \
                                    "%database%" $db \
                                    "%measurement%" $measurement \
                                    "%serie%" $measurement \
                                    "%date%" $date]
                    set dstdir [file join [dict get $options -root] [string map $map [dict get $options -dst]]]
                    if { [dict get $options -pending] ne "" } {
                        set tmpdir ${dstdir}[dict get $options -pending]
                        set bkpdir $tmpdir
                    } else {
                        set bkpdir $dstdir
                    }

                    # Select file for backup destination
                    Log INFO "Backup measurement $measurement from database $db into $dstdir"
                    set basename [string map $map [dict get $options -basename]]
                    set fname [CleanName [string map [dict get $options -map] $basename]].csv
                    set dstfile [file join $bkpdir [string trimleft $fname /]]
                    file mkdir [file dirname $dstfile]
                    set tmpfile [file join $bkpdir [TempName ".csv"]]

                    # Convert special keywords in incoming query.
                    set qry [string map $map [dict get $options -query]]
                    if { [catch {clock format $now -format $qry} tqry] } {
                        Log WARN "Cannot convert time-data in query: $tqry"
                    } else {
                        set qry $tqry
                    }

                    # Execute query and possibly compress destination.
                    if { [dict get $options -combine] } {
                        if { [dict get $options -compress] ne "" } {
                            set compressed [glob -nocomplain -- ${dstfile}?*]
                            if { [llength $compressed] } {
                                if { [catch {exec {*}[auto_execok [dict get $options -compress]] -d [lindex $compressed 0]} output] } {
                                    Log ERROR "Could not decompress [lindex $compressed 0]: $output"
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
                        CSVConvert $tmpfile $dstdir
                        if { [file exists $dstfile] && [file size $dstfile] > 0 } {
                            AppendFileContent $dstfile $tmpfile 1
                            file delete -force -- $tmpfile
                        } else {
                            Log DEBUG "Moving $tmpfile to $dstfile"
                            if { [catch {file rename -force -- $tmpfile $dstfile} err] } {
                                Log ERROR "$err"
                                file delete -force -- $tmpfile
                            }
                        }
                    } else {
                        catch {exec {*}[auto_execok [dict get $options -influx]] \
                                    -host [dict get $options -host] \
                                    -execute $qry \
                                    -database "$db" \
                                    -precision=rfc3339 \
                                    -format csv \
                                    > $dstfile}
                        CSVConvert $dstfile $dstdir
                    }
                    if { [file exists $dstfile] } {
                        if { [dict get $options -compress] ne "" } {
                            if { [catch {exec {*}[auto_execok [dict get $options -compress]] $dstfile} output] == 0 } {
                                file delete -- $dstfile
                            } else {
                                Log ERROR "Could not compress $dstfile: $output"
                            }
                        }
                    } else {
                        Log ERROR "Could not backup to $dstfile"
                    }

                    if { [dict get $options -pending] ne "" } {
                        if { [catch {file rename -force -- $bkpdir $dstdir} err] } {
                            Log ERROR "$err"
                            file delete -force -- $bkpdir
                        }
                    }
                }
            }
        }
    }
}

# ::HasPortable -- descr
#
#      descr
#
# Arguments:
#      None.
#
# Results:
#      None.
#
# Side Effects:
#      None.
proc ::HasPortable {} {
    # Call influxd with help a try looking for a -portable flag description
    catch {CallInfluxD backup -help} res
    foreach line [split $res \n] {
        set line [string trim $line]
        if { [string index $line 0] eq "-" } {
            if { [lindex $line 0] eq "-portable" } {
                return 1
            }
        }
    }
    return 0
}


# ::backup -- Peform one backup
#
#      This will create a directory for backup using the current time and
#      perform the type of backup specified by the global option at -mode.  More
#      details about the backup types can be found in the comment for the
#      procedure performing the backup itself.  Once a backup has been
#      performed, this will arrange to only keep the latest -keep backups (as
#      long as -keep is strictly positive). Finally, a symbolic link, as
#      specified at -link is kept updated to point at the latest backup.
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

    # Generate name for directory to hold backup
    set now [clock seconds]
    set date [clock format $now -format [dict get $options -format]]
    #set dstdir [file join [dict get $options -root] $date]

    # Get list of databases to backup. When using new -portable backups, we
    # keep the list empty to trigger backups in one sweep.
    set dbs [dict get $options -databases]
    if { [llength $dbs] == 0 \
            && (![dict get $::options -portable] \
                || [string tolower [dict get $options -mode]] ne "backup") } {
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
            Log INFO "Linking $lnk to $dstdir for $db"
            file mkdir [file dirname $lnk]
            # Remove existing link, if any (it is ok to fail on type since the
            # file might not exist yet.)
            if { [catch {file type $lnk} ftype] == 0 && $ftype eq "link" } {
                file delete -force -- $lnk
            }
            set relative [string repeat ../ $levels][string trimleft $r_dstdir /]
            if { [catch {file link -symbolic $lnk $relative} res] } {
                Log ERROR "Cannot link: $res"
            }
        }
    }
}


# ::fieldMatch --
#
#	This command matches a crontab-like specification for a field
#	to a current value.
#
#	A field may be an asterisk (*), which always stands for
#	''first-last''.
#
#	Ranges of numbers are allowed.  Ranges are two numbers
#	separated with a hyphen.  The specified range is inclusive.
#	For example, 8-11 for an ''hours'' entry specifies execution
#	at hours 8, 9, 10 and 11.
#
#	Lists are allowed.  A list is a set of numbers (or ranges)
#	separated by commas.  Examples: ''1,2,5,9'', ''0-4,8-12''.
#
#	Step values can be used in conjunction with ranges.  Following
#	a range with ''/<number>'' specifies skips of the number's
#	value through the range.  For example, ''0-23/2'' can be used
#	in the hours field to specify command execution every other
#	hour (the alternative in the V7 standard is
#	''0,2,4,6,8,10,12,14,16,18,20,22'').  Steps are also permitted
#	after an asterisk, so if you want to say ''every two hours'',
#	just use ''*/2''.
#
# Arguments:
#	value	Current value of the field
#	spec	Matching specification
#
# Results:
#	returns 1 if the current value matches the specification, 0
#	otherwise
#
# Side Effects:
#	None.
proc ::fieldMatch { value spec } {
    if { $value != "0" } {
        regsub "^0" $value "" value
    }

    foreach rangeorval [split $spec ","] {

        # Analyse step specification
        set idx [string first "/" $rangeorval]
        if { $idx >= 0 } {
            set step [string trim \
                    [string range $rangeorval [expr $idx + 1] end]]
            set rangeorval [string trim \
                    [string range $rangeorval 0 [expr $idx - 1]]]
        } else {
            set step 1
            set rangeorval [string trim $rangeorval]
        }

        # Analyse range specification.
        set values ""
        set idx [string first "-" $rangeorval]
        if { $idx >= 0 } {
            set minval [string trim \
                    [string range $rangeorval 0 [expr $idx - 1]]]
            if { $minval != "0" } {
                regsub "^0" $minval "" minval
            }
            set maxval [string trim \
                    [string range $rangeorval [expr $idx + 1] end]]
            if { $maxval != "0" } {
                regsub "^0" $maxval "" maxval
            }
            for { set i $minval } { $i <= $maxval } { incr i $step } {
                if { $value == $i } {
                    return 1
                }
            }
        } else {
            if { $rangeorval == "*" } {
                if { ! [expr int(fmod($value, $step))] } {
                    return 1
                }
            } else {
                if { $rangeorval == $value } {
                    return 1
                }
            }
        }
    }

    return 0
}


# ::PeriodicPulse -- Periodically perform backups
#
#      This will regularily perform backups according to the -period option.
#      This procedure takes care of timing operations, while backup operations
#      themselves are handled by ::backup
#
# Arguments:
#      None.
#
# Results:
#      None.
#
# Side Effects:
#      Creates backups and necessary directories under the -root directory
proc ::PeriodicPulse {} {
    global options

    set t_start [clock milliseconds]

    ::backup

    # Estimate elapsed milliseconds for the backup operations and take this into
    # account when scheduling next backup.  This is because backups can take a long time.
    if { [dict get $options -period] eq "" || [dict get $options -period] <= 0 } {
        Log NOTICE "No more backups, period was '[dict get $options -period]'"
    } else {
        set elapsed [expr {[clock milliseconds]-$t_start}]
        set next [expr {[dict get $options -period]*1000-$elapsed}]
        if { $next < 0 } {
            set next 0
        }
        Log NOTICE "Next backup in [expr {int($next/1000)}] seconds"
        after $next ::PeriodicPulse
    }
}

proc ::CronPulse {} {
    global options

    set start_ms [clock milliseconds]

    # Transform current date/time into the various fields that are relevant for
    # the cron-like date and time specification.
    set now [expr {$start_ms / 1000}]
    set sec [clock format $now -format "%S"];   # Really superfluous?
    set min [clock format $now -format "%M"]
    set hour [clock format $now -format "%H"]
    set daymonth [clock format $now -format "%e"]
    set month [clock format $now -format "%m"]
    set dayweek [clock format $now -format "%w"]

    lassign [dict get $::options -cron] e_min e_hour e_daymonth e_month e_dayweek
    if { [fieldMatch $min $e_min] \
            && [fieldMatch $hour $e_hour] \
            && [fieldMatch $daymonth $e_daymonth] \
            && [fieldMatch $month $e_month] \
            && [fieldMatch $dayweek $e_dayweek] } {
        ::backup
    }

    set elapsed [expr {[clock milliseconds]-$start_ms}]
    set next [expr {(1000*60)-$elapsed}]
    if { $next < 0 } {
        set next 0
    }
    after $next ::CronPulse
}

proc ::Log { lvl msg } {
    if { [dict get $::options -log] ne "" } {
        set levels {ERROR WARN NOTICE INFO DEBUG TRACE}
        if { [lsearch -nocase $levels $lvl] <= [lsearch -nocase $levels [dict get $::options -verbose]] } {
            set lvl [string tolower $lvl]
            if { [dict get $::options -logtstamp] ne "" } {
                set now [clock seconds]
                set dt [clock format $now -format [dict get $::options -logtstamp]]
                set line "\[$dt\] "
            } else {
                set line ""
            }
            append line "\[$lvl\] $msg"
            puts [dict get $::options -log] $line
        }
    }
}

# INIT #1: Set options from environment variables, if any.


dict for { opt dft } [dict filter $options key -*] {
    set envvar INFLUX_BACKUP_[string toupper [string trimleft $opt -]]
    if { [info exists ::env($envvar)] } {
        Log NOTICE "Setting option $opt to [set ::env($envvar)] (via environment)"
        dict set options $opt [set ::env($envvar)]
    }
}

# INIT #2: Quick options parser
foreach {opt val} $argv {
    if { [dict exists $options $opt] } {
        Log NOTICE "Setting option $opt to $val (via command-line)"
        dict set options $opt $val
    } else {
        Log ERROR "$opt unknown option, should be [join [dict keys [dict filter $options key -*]] ,\ ]"
        exit 1
    }
}

# INIT #3: Verify options
if { ! [llength [auto_execok [dict get $options -influx]]] } {
    Log ERROR "Cannot find influx command: [dict get $options -influx]"
    exit 1
}
if { ! [llength [auto_execok [dict get $options -influxd]]] } {
    Log ERROR "Cannot find influxd command: [dict get $options -influxd]"
    exit 1
}

# Resolve -password_file into -password so callers can use temporary files.
if { [dict get $::options -password_file] ne "" } {
    Log INFO "Reading influx password from [dict get $::options -password_file]"
    set fd [open [dict get $::options -password_file]]
    dict set ::options -password [string trim [read $fd]]
    close $fd
}

# Resolve backup period to milliseconds, we allow human-readable durations such
# as 1w, 2 months 4d or 2y -3m. When an integer, this is in milliseconds.
set period [dict get $::options -period]
if {  $period ne "" && ! [string is integer -strict $period] } {
    dict set ::options -period [Duration $period]
    Log INFO "Converted human-readable period: '$period' to [dict get $::options -period] s."
}

# Portable backups or not?
if { [dict get $::options -portable] eq "" } {
    dict set ::options -portable [HasPortable]
}
if { [dict get $::options -portable] } {
    Log INFO "Enabling portable backing up"
} else {
    Log NOTICE "Using legacy backing up techniques"
}

# Cleanup old pending backups or suggest to
AutoClean

# Wait can introduce a first-time delaying period, either random between two
# values separated by a colon sign, or fixed. All these can also be expressed in
# human-redable form, or as an integer (milliseconds)
set wait [dict get $::options -wait]
if { [string first ":" $wait] >= 0 } {
    # Extract min and max and make sure to have good defaults for them
    lassign [split $wait :] min max
    if { $min eq "" } { set min 0 }
    if { $max eq "" } { set max [dict get $::options -period] }
    # Accept human-readable durations
    if { ! [string is integer -strict $min] } {
        set min [Duration $min]
    }
    if { ! [string is integer -strict $max] } {
        set max [Duration $max]
    }
    set wait [expr {$min + int(rand()*($max - $min))}]
} elseif { ![string is integer -strict $wait] } {
    set wait [Duration $wait]
}

# Possibly wait, then start either a periodic backup (every -period) or cron
# backup, i.e. as specified through -cron.
if { $wait > 0 } {
    if { [llength [dict get $::options -cron]] == 5 } {
        Log NOTICE "Waiting $wait s. before taking first cron backup..."
        after [expr {1000*$wait}] ::CronPulse
    } else {
        Log NOTICE "Waiting $wait s. before taking first periodic backup..."
        after [expr {1000*$wait}] ::PeriodicPulse
    }
} else {
    if { [llength [dict get $::options -cron]] == 5 } {
        Log NOTICE "Starting cron backup..."
        after idle ::CronPulse
    } else {
        Log NOTICE "Starting periodic backup..."
        after idle ::PeriodicPulse
    }
}

vwait forever
