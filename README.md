# s4607_extra
Extra utilities for working with Stanag 4607 data that don't really belong in the core library (s4607). As an example, a useful query function to extract a particular subset of fields from the hierarchical packet structure would belong here not in the core.

## Dependencies
This library is designed to work with s4607, and a compiled copy of this code is required. This dependency is not currently handled automatically, but will be in the future.

## Coordinate conversion
There is a function for converting Latitude, Longitude, Altitude triples (referenced to the WGS ellipsoid) into ECEF (Earth Centred Earth Fixed) format. For example, from the Erlang prompt:
```
1> {X,Y,Z} = coord:lla_to_ecef({45,350,1000}).
```
where {X,Y,Z} are bound to the ECEF coordinated following the conversion.

