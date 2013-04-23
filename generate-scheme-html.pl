#!/usr/bin/perl

=head1 NAME

generate-scheme-html - Write a SKOS concept scheme in RDF to XHTML/RDFa

=head1 SYNOPSIS

    generate-scheme-html -D conceptscheme.rdf -t target/

=OPTIONS

=head2 -D --data

An RDF graph in a file such as RDF/XML or Turtle.

=head2 -t --target

A directory where the resulting files should be put.

=head1 AUTHOR

Dorian Taylor, C<< <dorian at cpan.org> >>

=head1 LICENSE AND COPYRIGHT

Copyright 2012 Dorian Taylor.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this file except in compliance with the License.  You may
obtain a copy of the License at
L<http://www.apache.org/licenses/LICENSE-2.0>

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied.  See the License for the specific language governing
permissions and limitations under the License.

=cut

use strict;
use warnings FATAL => 'all';

# guts
use URI;
use RDF::Trine;
use XML::LibXML::LazyBuilder qw(DOM E);

# interface
use Getopt::Long ();
use Pod::Usage   ();
use Path::Class  ();

my $NS = RDF::Trine::NamespaceMap->new({
    rdf  => 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    skos => 'http://www.w3.org/2004/02/skos/core#',
});

Getopt::Long::Configure qw(no_ignore_case);

my %opt = (
    data   => [],
    target => undef,
    base   => undef,
);

Getopt::Long::GetOptions(
    # -D like roqet
    'D|data=s'   => sub { push @{$opt{data}}, Path::Class::File->new($_[1]) },
    't|target=s' => sub { $opt{target} = Path::Class::Dir->new($_[1]) },
    'b|base=s'   => sub { $opt{base} = URI->new($_[1])->canonical },
);

Pod::Usage::pod2usage('I need an RDF file of some kind (-D option).')
    unless @{$opt{data}};
Pod::Usage::pod2usage('I need to know where to write the files (-t option).')
    unless $opt{target};

my $model = RDF::Trine::Model->temporary_model;

eval { $opt{target}->mkpath };

# XXX be smarter about guessing type
for my $file (@{$opt{data}}) {
    my $parser = RDF::Trine::Parser->guess_parser_by_filename($file);
    $parser->parse_file_into_model(undef, $file->open, $model);
}

my $iter = $model->get_statements
    (undef, $NS->rdf('type'), $NS->skos('Concept'));

while (my $statement = $iter->next) {
    my $subject = $statement->subject;
    my $doc = doc($model, $subject, \&concept_body);

    my $uuid = URI->new($subject->value);

    my $fn = $opt{target}->file($uuid->uuid . '.xml');
    my $fh = $fn->openw;

    $fh->syswrite($doc->toString(1));
}

### SUBS

sub uuid_uri {
    my $uuid = URI->new(shift);
    URI->new_abs($uuid->uuid, $opt{base});
}

sub related_terms {
    my ($model, $subject) = @_;
    my @list = E li => {},
        (E a => { href => '#oops' },
         (E span => { property => 'skos:prefLabel' },
          'This concept has no related terms because ' .
              'we need you to connect them!'));

    return (E div => {},
        (E h2 => {}, 'Related Concepts'),
        (E ul => { rel => 'skos:related' }, @list));
}

sub resource_list {
    my ($model, $subject, $heading, $predicate) = @_;

    my @list;
    for my $o ($model->objects($subject, $NS->uri($predicate))) {
        my ($label) = $model->objects($o, $NS->skos('prefLabel'));
        push @list, E li => {}, (E a => { href => uuid_uri($o->value) },
                                 (E span => { property => 'skos:prefLabel' },
                                  $label->value));
    }

    my $out = (@list ? (E ul => { rel => $predicate }, @list) :
        (E p => {}, 'Nothing to show.'));

    E div => {}, (E h2 => {}, $heading), $out;
}

sub connected_resources {
    my ($model, $subject) = @_;
    # buh this doesn't need to be done every run
    my $skos = $NS->skos->uri->value;
    my $re = qr/^$skos/o;
    # warn $re;

    # collect up the predicates
    my %map;
    my $iter = $model->get_statements(undef, undef, $subject);
    while (my $statement = $iter->next) {
        my $p = $statement->predicate->value;
        # throw out skos predicates because we're handling them elsewhere
        next if $p =~ $re;

        my $s = $statement->subject->value;
        $map{$s} ||= [];
        push @{$map{$s}}, $p;
    }

    my $out;
    if (scalar keys %map) {
        my @list;
        for my $uri (sort keys %map) {
            my $rev = join ' ', sort map { $_ } @{$map{$uri}};
            push @list, E a => { rev => $rev, href => uuid_uri($uri) }, $uri;
        }
        $out = E ul => {}, @list;
    }
    else {
        $out = E p => {},
            "There are no connected resources because we haven't done that yet.";
    }

    E div => {}, (E h2 => {}, 'Connected Resources'), $out;
}

# definition
# examples
# notes
# related
# broader
# narrower

sub concept_body {
    my ($model, $subject, $tnode) = @_;

    return E body => { typeof => 'skos:Concept' },
        ((E h1 => { property => 'skos:prefLabel' }, $tnode->value),
         (E p => { property => 'skos:definition' },
          'This is where the definition would go.'),
         (E div => {},
          (E h2 => {}, 'Examples'),
          (E p => { property => 'skos:example' },
           'This is where an example would go.')),
         (E div => {},
          (E h2 => {}, 'Notes'),
          (E p => { property => 'skos:note' },
           'This is where a note would go.')),
         (E div => { class => 'semantic-relations' },
          resource_list($model, $subject, 'Related Concepts', 'skos:related'),
          resource_list($model, $subject, 'Broader concepts', 'skos:broader'),
          resource_list
              ($model, $subject, 'More Specific Concepts', 'skos:narrower'),
      ),
          connected_resources($model, $subject),
     );
}

sub doc {
    my ($model, $subject, $body_cb) = @_;

    my ($tnode) = $model->objects($subject, $NS->skos('prefLabel'));

    # i have no idea why but map was Just Not Working
    my %xmlns;
    for my $k (keys %$NS) {
        $xmlns{"xmlns:$k"} = $NS->{$k};
    }

    DOM E html => {
        xmlns   => 'http://www.w3.org/1999/xhtml',
        version => 'XHTML+RDFa 1.0', %xmlns },
    (E head => {},
     (E title => { property => 'skos:prefLabel' }, $tnode->value),
     (E base => { href => uuid_uri($subject->value) })),
         $body_cb->($model, $subject, $tnode);
}
