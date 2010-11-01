#!/bin/awk -f
# Program:	mprintfmt
# Author:	Karsten M. Self
# Date:	10/18/1997
# Purpose:	Add intenting and vertical whitespace to SAS Macro output 
#		produced by RESERVEDB1 option output.
#
# This program is hereby placed on the public domain.
# Karsten M. Self 10/18/1997

BEGIN {
   IndentIncr = 4   # Amount to incr/decr indent by
   Indent     = 0   # Current indent level
   # Indent string pattern -- substrings are taken.  Note: this sets max indent
   IndentStrPt= "                                "
   IndentStr  = ""
   Incr       = 0   # Increment value
   Decr       = 0   # Decriment value
   }


# Initialize increment/decriment for new step
{
    Incr = 0
    Decr = 0
    }

# Delete null statements;
/^[     ]*;[    ]*$/ { break }

# Add linespace before program step;
/(^PROC |^DATA[     ;] )/ {
    print ""
    }


# Indent program step blocks;
#   Increment takes place on following step
#   Decriment takes place on following step
#   Non-initial 'DO' should move to next line
# ...incrementers -- increase indent by 4
/^SELECT[ (;]/ {
    Incr = Incr += IndentIncr
    }

/^DO  *.*/ {
    Incr = Incr += IndentIncr
    print ""
    }

/^ *[^*].**  *DO*[; ]/ {
    Incr = Incr += IndentIncr
    }


# ...decrimenters -- decrease indent by 4
/^ *END *;/ {
    Decr = Decr += IndentIncr
    }

# ...initializers -- set indent to 4
/^(PROC[    ]+|DATA[;   ]+)/ {
    Indent = IndentIncr
    Incr = 0
    Decr = 0
    }

# ...zeroers -- set indent to 0
/^(RUN *;|QUIT *;)/ {
    Indent = 0
    Incr = 0
    Decr = 0
    }


# Write current line
{ print IndentStr $0 }


# Compute current indent
{
    Indent    = Indent + Incr - Decr
    IndentStr = substr( IndentStrPt, 1, Indent )
    }

# Add spacing after ...
/(^ *[^*].*CALL SYMPUT\(.*\) *;|^ *END *;|^OPTIONS |^SET |^MERGE |^MODIFY |^INFILE )/ {
    print ""
    }

/(^QUIT *;|^RUN *;)/ {
    print ""
    print ""
    }