# SuuntoToStrava

Automatic merge and upload of Suunto Quest and Suunto GPS Tracking Pod data.

This utility automates steps needed when using [Suunto GPS Tracking Pod][SuuntoGPS]
in combination [Suunto Quest sporttester watches][SuuntoQuest] with [Strava][Strava].

# How to use

## Installation
- download jar file from releases and place it anywhere on your drive.
- execute the jar
- browser should open, offering you to connect to Strava
- change settings as needed
- press `Save settings` when done

## Regular use
- connect GPS Track Pod
- connect Suunto Quest
- wait until all data are sent to Movescount
- execute the `SuuntoToStrava` jar
- browser should open, showing progress of reading and uploading activities
- you can review all uploaded activities by following links to Strava. All activities are upload as private, so that
you can review and edit them before making visibile to others.

# Notes
As far as I know, the utility works fine, but there are no guarantees. If you experience some issues or if you want to
provide some feedback, use [the issue tracker][issues].

It may work with other combinations of GPS Track Pod and other Suunto Devices, but this is not tested.
Adding support for other Suunto watches should be easy if needed, only moveslink.XMLParser class would need to be adjusted.

The utility uses local copies of data uploaded to Movescount by Moveslink software, merges GPS and Heart Rate data
and sends them to Strava. When using this utility you should disable Movescount sending data to Strava on its own, 
or remove Strava integration from Movescount completely.

## Laps and activity boundaries detection

The utility uses laps from both GPS Pod and watches. It detects activity boundaries smartly - the primary source of
activity boundaries and types are the watches and if there are substantial segments of GPS activity with no corresponding
watch record, that part of GPS data is uploaded as a separate activity.

The utility includes a safety heuristics preventing uploading rides as runs, as this often results in Strava invalidating
running Estimated Best efforts. The heuristics is based on maximum speed achieved. If you are very fast runner, you might
occasionally have to change the activity type to running after uploading.

Example of multi-sport activities workflow, like triathlon:

- turn on GPS track pod
- swim (no HR data, you do not even take watches)
- take watches and start activity Bike with them
- bike
- stop the activity using the watches, start Run on watches
- run
- stop the activity using the watches
- stop the GPS pod

This will result in three activities created on Strava, one of Unknown type (GPS data only), and two of types Ride and Run
respectively.

----------------------

Some code (Suunto Move file parsers) adapted from https://github.com/oldhu/suunto2nike

[SuuntoGPS]: http://www.suunto.com/Products/PODs/Suunto-GPS-Track-POD/
[SuuntoQuest]: http://www.suunto.com/sports-watch-collections/Suunto-Quest-Collection/
[Strava]: https://www.strava.com
[issues]: ../../issues
