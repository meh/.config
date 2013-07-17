# ttc - Twitter Tab Completion
use Irssi;
use strict;

use Data::Dumper;

use vars qw($VERSION %IRSSI);

$VERSION="0.1.1";
%IRSSI = (
        authors=> 'PtPazuzu',
        contact=> 'peter@pandorasdoosje.nl',
        name=> 'ttc - Twitter Tab Completion',
        description=> '@<nick> Tab-completion for Bitlbee-twitter integration',
        license=> 'Public Domain',
);

sub wildcard {
    my $value = shift;

    $value =~ s/\*/.*?/;
    $value =~ s/\?/./;

    return $value;
}

sub twitter_complete {
  my ($complist, $window, $word, $linestart, $wantspace) = @_;

  my $channel = $window->{active};
  
  # the completion is ok if 
  #  this is a channel, 
  #  and the searched word starts with '@'
  if (($channel->{type} ne "CHANNEL") || ($word !~ /^@/)) {
    return;
  }

  my $return   = 1;
  my @channels = split /\s*[:,]\s*/, Irssi::settings_get_str('twitter_complete_in');

  for my $expression (@channels) {
    $expression = wildcard($expression);

    if (eval { $channel->{name} =~ /$expression/; }) {
      $return = 0;
      last;
    }
  }

  if ($return) {
    return;
  }

  $word =~ s/^@//;

  for my $user ($channel->nicks()) {
    if ($user->{nick} =~ /^$word/i) {
      (my $name = $user->{host}) =~ s/@.*$//;

      push (@{$complist}, '@' . $name);
    }
  }
}

Irssi::signal_add_first('complete word', 'twitter_complete');

Irssi::settings_add_str('twitter', 'twitter_complete_in', '#twitter_*');
