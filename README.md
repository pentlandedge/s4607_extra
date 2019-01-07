# s4607_extra
Extra utilities for working with Stanag 4607 data that don't really belong in the core library (s4607). As an example, a useful query function to extract a particular subset of fields from the hierarchical packet structure would belong here not in the core. 

NOTE: the library has recently moved from using rebar to the newer rebar3 build tool. The version 1.0.0 release is the last version using rebar.

## Prerequisites
It is necessary to have Erlang installed, and the compiler erlc available on the path. The rebar3 tool is used to control the build process, so it is necessary to have this installed and on the path too. 

If using the (optional) Makefile, then the make utility must be available.

## Building

The simplest way to build the software, run the unit tests, perform static analysis and generate the module documentation in one step is to use make:
```
# make
```
The makefile has rules for each of these steps which can be run separately if preferred. It uses rebar3 to do the real work.

Alternatively, the software can be compiled (on a Linux platform) directly using rebar3:
```
# rebar3 compile
```
This library has dependencies on the core s4607 library and the jsx library for JSON handling. Both 'make' and 'rebar3 compile' will attempt to fetch these dependencies.
## Running interactively
The library functions can be called from the Erlang shell. When starting the shell, it is necessary to supply the path to the ebin directories for both this library and the s4607 library on which it depends. From the root directory, the most convenient way to start the Erlang shell and pass it the paths to the compiled files is using rebar3:
```
# rebar3 shell 
```
## Coordinate conversion
There is a function for converting Latitude, Longitude, Altitude triples (referenced to the WGS ellipsoid) into ECEF (Earth Centred Earth Fixed) format. For example, from the Erlang prompt:
```
1> {X,Y,Z} = coord:lla_to_ecef({45,350,1000}).
```
where {X,Y,Z} are bound to the ECEF coordinates following the conversion.

## Extracting target statistics
For a number of applications, the most useful statistics are contained within the dwell segments (which incorporate target reports). However, the mission segment supplies the reference date which is used as the base on which to apply the time offset in the dwell. This library provides a utility which extracts all of the dwell segments in a packet list, but notes the reference date from mission segments as they are encountered. All of the data is returned as a list of dictionaries where each item in the list corresponds to a dwell segment. To call it (on a PacketList extracted using the s4607 library):
```
1> DwellStats = tgt_stats:extract(PacketList).
```

## Converting target information to GeoJSON
This is currently a work in progress. The aim is to convert the most useful fields from the target statistics structure into GeoJSON. GeoJSON is a standard format for exchanging geographical data, and can be consumed by various javascript mapping libraries. This should allow the presentation of Stanag 4607 target information in the browser. 

