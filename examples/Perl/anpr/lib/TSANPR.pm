# The MIT License (MIT)
# Copyright © 2022-2025 TS-Solution Corp.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to all conditions.
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

package TSANPR;
use strict;
use warnings;
use FFI::Platypus;
use Exporter 'import';
use Encode qw(decode);

our @EXPORT_OK = qw(load_tsanpr unload_tsanpr anpr_initialize anpr_read_file anpr_read_pixels);

my $ffi;
my $lib_loaded = 0;

sub load_tsanpr {
    my ($libpath) = @_;
    die "Library path is required" unless $libpath;

    $ffi = FFI::Platypus->new( api => 1 );
    $ffi->lib($libpath);

    # Attach C functions with _raw names
    $ffi->attach( [ anpr_initialize   => 'anpr_initialize_raw'   ] => ['string'] => 'string' );
    $ffi->attach( [ anpr_read_file    => 'anpr_read_file_raw'    ] => ['string', 'string', 'string'] => 'string' );
    $ffi->attach( [ anpr_read_pixels  => 'anpr_read_pixels_raw'  ] => ['opaque', 'ulong', 'ulong', 'long', 'string', 'string', 'string'] => 'string' );

    $lib_loaded = 1;
    return 1;
}

sub unload_tsanpr {
    undef $ffi;
    $lib_loaded = 0;
}

sub anpr_initialize {
    die "Library not loaded" unless $lib_loaded;
    my $ret = anpr_initialize_raw(@_);
    return defined $ret ? decode('UTF-8', $ret) : undef;
}
sub anpr_read_file {
    die "Library not loaded" unless $lib_loaded;
    my $ret = anpr_read_file_raw(@_);
    return defined $ret ? decode('UTF-8', $ret) : undef;
}
sub anpr_read_pixels {
    die "Library not loaded" unless $lib_loaded;
    my $ret = anpr_read_pixels_raw(@_);
    return defined $ret ? decode('UTF-8', $ret) : undef;
}

1;
