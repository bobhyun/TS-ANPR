#!/usr/bin/perl -w

use strict;
use warnings;
use utf8;
use Win32::API;
use Imager;
use Data::Buffer;

my $IMG_PATH = "..\\img\\";
my $DLL_NAME = "..\\bin\\x64\\tsanpr.dll";

my $anpr_initialize = Win32::API->Import($DLL_NAME, "char *anpr_initialize(char *outputFormat)");
die unless defined $anpr_initialize;

my $anpr_read_file = Win32::API->Import($DLL_NAME, 
  "char *anpr_read_file(char* imgFileName, char* outputFormat, char* options)");
die unless defined $anpr_read_file;

my $anpr_read_pixels = Win32::API->Import($DLL_NAME, 
  "char *anpr_read_pixels(unsigned char* pixels, unsigned long width, unsigned long height, \
    unsigned long stride, char* pixelFormat, char* outputFormat, char* options)");
die unless defined $anpr_read_pixels;


sub initialize {
  my $error = anpr_initialize("text");
  return $error
}

sub readFile {
  my $imgFile = $_[0];
  my $outputFormat = $_[1];
  my $options = $_[2];

  print("${imgFile} outputFormat=\"${outputFormat}\", options=\"${options}\") => ");
  my $result = anpr_read_file($imgFile, $outputFormat, $options);
  print("${result}\n");
  return;
}

sub readPixels {
  my $imgFile = $_[0];
  my $outputFormat = $_[1];
  my $options = $_[2];

  print("${imgFile} outputFormat=\"${outputFormat}\", options=\"${options}\") => ");

  my $img = Imager->new();
  $img->read(file => $imgFile) or die img->errstr();
  my $width = $img->getwidth();
  my $height = $img->getheight();

  my $data = Data::Buffer->new;
  for my $y ( 0 .. $height-1 ) {
    my $sample = $img->getsamples(y => $y);
    $data->put_bytes($sample);
  }

  my $result = anpr_read_pixels($data->bytes(), $width, $height, 0, "BGR", $outputFormat, $options);
  print("${result}\n");
  return;
}

sub anprDemo1 {
  my $outputFormat = $_[0];
  readFile("${IMG_PATH}licensePlate.jpg", $outputFormat, "v");
  readFile("${IMG_PATH}licensePlate.jpg", $outputFormat, "");
  readFile("${IMG_PATH}multiple.jpg", $outputFormat, "vm");
  readFile("${IMG_PATH}multiple.jpg", $outputFormat, "");
  readFile("${IMG_PATH}surround.jpg", $outputFormat, "vms");
  readFile("${IMG_PATH}surround.jpg", $outputFormat, "");
  return;
}

sub anprDemo2 {  
  my $outputFormat = $_[0];
  readPixels("${IMG_PATH}licensePlate.jpg", $outputFormat, "v");
  readPixels("${IMG_PATH}licensePlate.jpg", $outputFormat, "");
  readPixels("${IMG_PATH}multiple.jpg", $outputFormat, "vm");
  readPixels("${IMG_PATH}multiple.jpg", $outputFormat, "");
  readPixels("${IMG_PATH}surround.jpg", $outputFormat, "vms");
  readPixels("${IMG_PATH}surround.jpg", $outputFormat, "");
  return;
}

sub main {
  
  binmode(STDOUT, ":encoding(utf8)");

  my $error = initialize();
  if ($error ne "") {
    print($error);
    exit(1);
  }
 
  anprDemo1("text");
  anprDemo1("json");
  anprDemo1("yaml");
  anprDemo1("xml");
  anprDemo1("csv");
 
  anprDemo2("text");
  anprDemo2("json");
  anprDemo2("yaml");
  anprDemo2("xml");
  anprDemo2("csv"); 
}


main();