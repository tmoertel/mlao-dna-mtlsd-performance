#!/usr/bin/perl -nl
print $1 if /^ggsave\((?:file=)?"([^"]+)"/;
