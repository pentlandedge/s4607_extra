# s4607_extra
Extra utilities for working with Stanag 4607 data that don't really belong in the core library (s4607). As an example, a useful query function to extract a particular subset of fields from the hierarchical packet structure would belong here not in the core. The project uses the rebar utility to manage builds and dependencies, so rebar must be available on the path.

## Dependencies
This library is designed to work with s4607, and a compiled copy of this code is required. To fetch the dependency using rebar:
```
# rebar get-deps
```
## Building the library
It is necessary to have Erlang installed, and the compiler erlc available on the path. The software can be built (on a Linux platform) using rebar 
```
# rebar compile 
```
## Running interactively
The library functions can be called from the Erlang shell. When starting the shell, it is necessary to supply the path to the ebin directories for both this library and the s4607 library on which it depends. From the root directory, the most convenient way to start the Erlang shell and pass it the paths to the compiled files is using rebar:
```
# rebar shell 
```
which is the equivalent of running
```
# erl -pa ebin -pa deps/s4607/ebin
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
