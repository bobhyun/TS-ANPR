#!/usr/bin/env perl
#
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

use strict;
use warnings;
use Config;
use Imager;
use lib './lib';
use TSANPR qw(load_tsanpr unload_tsanpr anpr_initialize anpr_read_file anpr_read_pixels);
use FFI::Platypus::Buffer qw(scalar_to_pointer);

my $examplesBaseDir = "../..";

sub getEngineFileName {
    my $base = $examplesBaseDir;
    if ($^O eq 'MSWin32') {
        # 64-bit or 32-bit Windows
        if ($Config::Config{archname} =~ /x64/) {
            return $base . "\\bin\\windows-x86_64\\tsanpr.dll";
        } else {
            return $base . "\\bin\\windows-x86\\tsanpr.dll";
        }
    } elsif ($^O eq 'linux') {
        my $arch = `uname -m`;
        chomp $arch;
        return $base . "/bin/linux-x86_64/libtsanpr.so" if $arch eq 'x86_64';
        return $base . "/bin/linux-aarch64/libtsanpr.so" if $arch eq 'aarch64';
    }
    die "Unsupported OS or architecture";
}

sub readImageFile {
    my ($imgfile, $outputFormat, $options) = @_;
    print "$imgfile (outputFormat=\"$outputFormat\", options=\"$options\") => ";
    my $result = anpr_read_file($imgfile, $outputFormat, $options);
    print "$result\n";
}

sub readEncodedImage {
    my ($imgfile, $outputFormat, $options) = @_;
    print "$imgfile (outputFormat=\"$outputFormat\", options=\"$options\") => ";
    open my $fh, '<:raw', $imgfile or do {
        print "File open failed\n";
        return;
    };
    my $data;
    read $fh, $data, -s $fh;
    close $fh;

    # Get pointer to the data
    my $ptr = scalar_to_pointer($data);

    my $result = anpr_read_pixels($ptr, length($data), 0, 0, "encoded", $outputFormat, $options);
    print "$result\n";
}

sub readPixelBuffer {
    my ($imgfile, $outputFormat, $options) = @_;
    print "$imgfile (outputFormat=\"$outputFormat\", options=\"$options\") => ";

    # Read the image file
    my $img = Imager->new;
    $img->read(file => $imgfile)
        or die "Cannot read $imgfile: ", $img->errstr;

    # Extract image information
    my $width  = $img->getwidth;
    my $height = $img->getheight;
    my $channels = $img->getchannels;

    # Determine pixel format
    my $pixelFormat;
    if ($channels == 1) {
        $pixelFormat = "GRAY";
    } elsif ($channels == 3) {
        $pixelFormat = "BGR"; # Imager uses RGB by default; convert if necessary
    } elsif ($channels == 4) {
        $pixelFormat = "BGRA";
    } else {
        die "Unsupported channel count: $channels";
    }

    # Extract raw pixel data
    my $raw;
    $img->write(data => \$raw, type => 'raw')
        or die "Cannot extract raw pixel data: ", $img->errstr;

    # Calculate stride (bytes per row)
    my $stride = $width * $channels;

    # Convert raw data to pointer
    my $ptr = scalar_to_pointer($raw);

    # Call anpr_read_pixels with pointer
    my $result = anpr_read_pixels($ptr, $width, $height, $stride, $pixelFormat, $outputFormat, $options);
    print "$result\n";
}

sub readLicensePlates {
    # NOTICE:
	# anpr_initialize should be called only once after library load.
	# Therefore, it is not possible to change the country code after anpr_initialize has been called.
	# While using the free trial license, you can try all languages.
	# When you purchase a commercial license, you can only use the selected language.
    my ($countryCode) = @_;
    my $initParams = "text;country=$countryCode";
    my $error = anpr_initialize($initParams);
    if ($error && length($error)) {
        print "anpr_initialize() failed (error=$error)\n";
        return -1;
    }
    my $imageDir = "$examplesBaseDir/img/$countryCode/";

    # TODO: Try each function as needed
    my $anprFunc = \&readImageFile;
    # my $anprFunc = \&readEncodedImage;
    # my $anprFunc = \&readPixelBuffer;

    # TODO: Try each output format as needed
    my $outputFormat = "text";
    # my $outputFormat = "json";
    # my $outputFormat = "yaml";
    # my $outputFormat = "xml";
    # my $outputFormat = "csv";

    $anprFunc->("${imageDir}licensePlate.jpg", $outputFormat, "");  # Single license plate recognition (default)
    $anprFunc->("${imageDir}multiple.jpg", $outputFormat, "vm");    # Recognize multiple license plates attached to vehicles
    $anprFunc->("${imageDir}multiple.jpg", $outputFormat, "vmb");   # Recognize multiple license plates attached to vehicles (including motorcycles)
    $anprFunc->("${imageDir}surround.jpg", $outputFormat, "vms");   # Recognize multiple license plates attached to vehicles with surround detection
    $anprFunc->("${imageDir}surround.jpg", $outputFormat, "dms");   # Recognize multiple surrounding objects (vehicles)
    $anprFunc->("${imageDir}surround.jpg", $outputFormat, "dmsr");  # Recognize multiple surrounding objects (vehicles) and license plates

    # Recognize multiple surrounding objects and license plates within RoI
    $anprFunc->("${imageDir}surround.jpg", $outputFormat, "dmsri549,700,549,2427,1289,2427,1289,700")
    
    return 0;
}

# Main
my $engineFileName = getEngineFileName();
load_tsanpr($engineFileName);

# TODO: Try each country code as needed
readLicensePlates("KR");
# readLicensePlates("JP");
# readLicensePlates("VN");

unload_tsanpr();
