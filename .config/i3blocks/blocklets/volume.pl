#!/usr/bin/env perl
use strict;
# use warnings;
# use diagnostics;

$ENV{LANG} = 'C';

sub get_volume_from_index {
    # return -1 if mute
    my $index = $_[0];
    my $sinks_out = `pacmd list-sinks`;

    # clear output
    $sinks_out =~ s/^.+?index: $index//s;

    # check for mute
    $sinks_out =~ /muted: (\w+)/;
    if ($1 eq "yes"){
        return -1
    }

    # get volume
    $sinks_out =~ /volume:.+?(\d+)%/;
    return $1;
    
}

sub wrap_str {
    return "<span size='x-small'>" . $_[0] . "</span>\n";
}

my $sources_out = `pacmd list-sources`;
$sources_out =~ /\* index: (\d+)/;
my $index = $1;

if ($ENV{BLOCK_BUTTON}){
    if ($ENV{BLOCK_BUTTON} == 1) {
        system("pactl", "set-sink-mute", $index, "toggle");
    } elsif ($ENV{BLOCK_BUTTON} == 4) {
        system("pactl", "set-sink-volume", $index, "+5%");
    } elsif ($ENV{BLOCK_BUTTON} == 5) {
        system("pactl", "set-sink-volume", $index, "-5%");
    } elsif ($ENV{BLOCK_BUTTON} == 6) {
        system("pactl", "set-sink-volume", $index, "+1%");
    } elsif ($ENV{BLOCK_BUTTON} == 7) {
        system("pactl", "set-sink-volume", $index, "-1%");
    }
}

my $volume = get_volume_from_index($index);

if ($volume < 0) {
    print(wrap_str(""));
} elsif ($volume < 100) {
    print(wrap_str($volume));
} elsif ($volume = 100) {
    print("\n");
} else {
    print("<span foreground=\"#" . $ENV{__BASE08} . "\">" . wrap_str($volume));
}

if ($ARGV[0]) {
    system("pkill", "-RTMIN+" . $ARGV[0], "i3blocks")
}
