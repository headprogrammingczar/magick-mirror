# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

package Magick::Mirror;

use IPC::Run;
use File::Spec;
use File::Basename;

use strict;
use warnings;

# Effects

# Prefix the output file with "grayscale-", and reduce the colorspace of the image with convert
sub grayscale {
  my ($self) = @_;

  $self->simple_transform('grayscale', '-colorspace', 'Gray');
}

sub flip {
  my ($self) = @_;

  $self->simple_transform('flip', '-flip');
}

sub flop {
  my ($self) = @_;

  $self->simple_transform('flop', '-flop');
}

# Negates an image; only really makes sense on masks
sub negate {
  my ($self) = @_;

  $self->simple_transform('negate', '-negate');
}

# Add drop shadow to an image
# x, y should be one of "-num" or "+num"
sub shadow {
  my ($self, $color, $opacity, $blur, $x, $y) = @_;
  my ($shadow, $geometry);

  $geometry = $opacity .'x'. $blur . $x . $y;

  # Make shadow
  $shadow = $self->clone();
  $shadow->simple_transform("shadow_". $color ."_". $geometry, '-background', $color, '-shadow', $geometry);

  print "shadow debug: ". $shadow->identify();

  # -layers merge
  $self->merge_below($shadow);
}

# Composite two images with a mask such that one blends into the other
sub blend {
  my ($self, $other, $mask) = @_;
  my ($inverted_mask);

  $inverted_mask = $mask->clone();
  $inverted_mask->negate();

  $other->mask($inverted_mask);
  $self->composite($other);
}

sub merge_below {
  my ($self, $bottom) = @_;
  my ($out, $path, $cont) = $self->match();
  my ($new_cont, $filename);

  $filename = $bottom->run();

  $new_cont = sub {
    my $contents = $cont->($_);
    return &raw_convert($contents, $filename, '-', '-background', 'none', '-layers', 'merge', '-');
  };

  $self->set("merge_below-$out", $path, $new_cont);
}

# The omnicient all-powerful -layers merge
sub merge {
  my ($self, $top) = @_;
  my ($out, $path, $cont) = $self->match();
  my ($new_cont, $filename);

  $filename = $top->run();

  $new_cont = sub {
    my $contents = $cont->($_);
    return &raw_convert($contents, '-', $filename, '-background', 'none', '-layers', 'merge', '-');
  };

  $self->set("merge-$out", $path, $new_cont);
}

# Composite another image beneath this one
# We can't directly do this with a simple transformation because the $self and $bottom inputs are swapped in the call to `convert`
sub composite_under {
  my ($self, $bottom) = @_;
  my ($out, $path, $cont) = $self->match();
  my ($new_cont, $filename);

  $filename = $bottom->run();

  $new_cont = sub {
    my $contents = $cont->($_);
    return &raw_convert($contents, $filename, '-', '-composite', '-');
  };

  $self->set("composite_under-$out", $path, $new_cont);
}

# Composite another image on top of this one
sub composite {
  my ($self, $top) = @_;
  my ($filename);

  # It is important that we do not defer generating the top image, as it could be different later, and we want what it looks like right now
  $filename = $top->run();

  $self->simple_transform('composite', $filename, '-composite');
}

# Take a mask and apply it to the current image
# http://www.imagemagick.org/Usage/compose/#copyopacity
# convert top mask -compose Copy_Opacity -composite out
sub mask {
  my ($self, $mask) = @_;
  my ($mask_path);

  $mask_path = $mask->run();

  $self->simple_transform("mask", $mask_path, '-compose', 'Copy_Opacity', '-composite');
}

sub make_uv {
  my ($r, $g) = @_;
  my ($uv, $g_file);

  $uv = $r->clone();

  $g_file = $g->run();
  $uv->simple_transform('uv', $g_file, '-background', 'black', '-channel', 'RG', '-combine');

  return $uv;
}

sub focus {
  my ($self, $mask, $geometry) = @_;

  $mask->negate();

  $self->blur($mask, $geometry);
}

sub blur {
  my ($self, $mask, $geometry) = @_;
  my ($mask_path);

  $mask_path = $mask->run();

  $self->simple_transform("blur", $mask_path, '-compose', 'Blur', '-set', "option:compose:args", $geometry, "-composite");
}

sub level {
  my ($self, $low, $high, $gamma) = @_;

  $self->simple_transform('level', "-level", $low . "%," . $high . "%," . $gamma);
}

sub gamma {
  my ($self, $gamma) = @_;

  $self->level(0, 100, $gamma);
}

sub sigmoid {
  my ($self, $factor, $threshold) = @_;

  $self->simple_transform("sigmoid", "-sigmoidal-contrast", $factor . ',' . $threshold . '%');
}

sub auto_orient {
  my ($self) = @_;

  $self->simple_transform('autoorient', '-auto-orient');
}

sub get_exif {
}

# Basic image resize and reposition
sub resize {
  my ($self, $geometry) = @_;
  my ($out, $path, $cont) = $self->match();
  my ($new_cont);

  $new_cont = sub {
    my $contents = $cont->($_);
    return &raw_convert($contents, '-', "-resize", $geometry, '-');
  };

  $self->set("resize-$geometry-$out", $path, $new_cont);
}

# Basic canvas resize and reposition (crops images)
sub extent {
  my ($self, $geometry) = @_;
  my ($out, $path, $cont) = $self->match();
  my ($new_cont);

  $new_cont = sub {
    my $contents = $cont->($_);
    return &raw_convert($contents, '-', "-extent", $geometry, '-');
  };

  $self->set("extent-$geometry-$out", $path, $new_cont);
}

# Low-level stuff

# Simple transformation generator - for example use, see sub grayscale
sub simple_transform {
  my ($self, $prefix, @options) = @_;
  my ($out, $path, $cont) = $self->match();
  my ($new_cont);

  $new_cont = sub {
    my $contents = $cont->($_);
    return &transform($contents, @options);
  };

  $self->set("$prefix-$out", $path, $new_cont);
}

sub format {
  my ($self, $format) = @_;
  my ($out, $path, $cont) = $self->match();
  my ($new_cont, $new_out, $name, $file);

  $file = File::Spec->catpath(undef, $path, $out);

  ($name, undef, undef) = File::Basename::fileparse($file, qr/\..*/);

  $new_out = "$name.$format";

  $new_cont = sub {
    my $contents = $cont->($_);
    return &raw_convert($contents, '-', $contents, '-format', $format, "$format:-");
  };

  $self->set($new_out, $path, $new_cont);
}

sub transform {
  my ($in, @opts) = @_;

  return &raw_convert($in, '-', @opts, '-');
}

# Extremely basic wrapper for a call to `convert`
sub raw_convert {
  my ($in, @opts) = @_;
  my (@cmd, $out, $err, $status);

  # plug options and parameterized stdin to convert
  @cmd = ('convert', @opts);
  $status = &IPC::Run::run(\@cmd, \$in, \$out, \$err);

  # return stdout
  return $out;
}

# Return some metrics about the image object
# Warning: performs repeat computation, so you probably want to checkpoint prior to calling this
sub identify {
  my ($self) = @_;
  my ($out, $path, $cont) = $self->match();
  my ($contents);

  $contents = $cont->(undef);

  return &raw_identify($contents);
}

sub raw_identify {
  my ($in) = @_;
  my (@cmd, $out, $err, $status);

  # plug options and parameterized stdin to convert
  @cmd = ('identify', '-');
  $status = &IPC::Run::run(\@cmd, \$in, \$out, \$err);

  # return stdout
  return $out;
}

# Set a checkpoint for a specific file. This will test for the existence of the checkpoint image, and if it exists, short-circuit the generation function.
sub named_checkpoint {
  my ($self, $filename) = @_;
  my ($out, $path, $cont) = $self->match();
  my ($new_cont, $file);

  $file = File::Spec->catpath(undef, $path, $filename);

  $new_cont = sub {
    if (-e $file) {
      return &read_file($file);
    } else {
      my $contents = $cont->($_);
      &write_file($file, $contents);
      return $contents;
    }
  };

  $self->set($filename, $path, $new_cont);
}

# Generate a checkpoint image using the current automatically generated name
sub checkpoint {
  my ($self) = @_;
  my ($out, $path, $cont) = $self->match();

  $self->named_checkpoint($out);
}

sub write_file {
  my ($file, $contents) = @_;
  my ($fh);

  open($fh, '>', $file) or die "Couldn't open checkpoint $file for writing";
  print $fh $contents;
  close $fh;
}

sub read_file {
  my ($file) = @_;
  my ($fh, $contents);

  open($fh, '<', $file) or die "Couldn't open checkpoint $file for reading";
  { local $/; $contents = <$fh>; }
  close $fh;

  return $contents;
}

# Set a checkpoint - the last operation in the pipeline is now a write to file
# Then execute the current generation function
# Returns the name of the generated file
sub run {
  my ($self) = @_;

  $self->checkpoint();

  my ($out, $path, $cont) = $self->match();

  $cont->(undef);

  return File::Spec->catpath(undef, $path, $out);
}

sub out_path {
  my ($self, $out_path) = @_;

  my ($out, $path, $cont) = $self->match();

  $self->set($out, $out_path, $cont);
}

# A Magick Mirror object has 2 fields - the output file, and a generating function
# The generating function takes no parameters, and outputs raw image data
# Operations on this object are "buffered" in the generating function, but not computed
# A typical image conversion operation will change the output file for automatic checkpoint handling,
# and update the generating function to be composed with an IPC call to convert. There's likely faster ways to accomplish this goal, but PerlMagick has a propensity for core dumps and missing features.
sub new {
  my ($infile, $path) = @_;
  my ($self, $in);

  $self = {};
  bless $self;

  (undef, undef, $in) = File::Spec->splitpath($infile);

  $self->set($in, $path, sub{return &read_file($infile);});

  return $self;
}

# Copies an image object
# Will not result in recomputation if a checkpoint is set just prior to cloning
sub clone {
  my ($self) = @_;
  my ($out, $path, $cont) = $self->match();
  my ($new);

  $new = {};
  bless $new;
  $new->set($out, $path, $cont);

  return $new;
}

sub match {
  my ($self) = @_;

  return ($self->{'out'}, $self->{'path'}, $self->{'cont'});
}

sub set {
  my ($self, $out, $path, $cont) = @_;

  $self->{'out'} = $out;
  $self->{'path'} = $path;
  $self->{'cont'} = $cont;
}

1;

__END__

