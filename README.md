# Sun Rise-Transit-Set

A Clojure learning exercise printing Sun events for an observer location
- month and day
- rise, transit, set times
- rise azimuth (West of South)
- transit elevation, transit declination
- length of day.


## Credits

The code in  {efx,mthu,ssetc,ut2tdb,vsp}.clj  was translated or freely paraphrased from Java code in SkyviewCafe which carry the following notice:

    ```
    Copyright (C) 2000-2007 by Kerry Shetline, kerry@shetline.com.

    This code is free for public use in any non-commercial application. All
    other uses are restricted without prior consent of the author, Kerry
    Shetline. The author assumes no liability for the suitability of this
    code in any application.

    2007 MAR 31   Initial release as Sky View Cafe 4.0.36.
    ```

Also based indirectly on:
- Truncated version of  VSOP87D series.
  Jean Meeus  _Astromical Algorithms, 2nd Ed._

- Bretagnon, P.; Francou, G. (1988).
 "Planetary Theories in rectangular and spherical variables: VSOP87 solution".
 Astronomy and Astrophysics. 202: 309.


Hat-tip to  Michiel Borkent  for the excellent babashka (aka bb)
 https://github.com/babashka/babashka


## Usage

- no args         ==>  rise,set ... for two-weeks around today's date
- 2 args: year 0  ==>  ditto for 20th of each month;  equinoxes, solstices.
- 3 args: year start-day num-of-weeks  ==> ditto for each week
- 4 args: year month day num-of-days   ==> ditto for days requested

A  'config.edn' file (in current execution dir) can be used to override the default observer location.


## Examples
    
    $ clojure -M -m find-events.core  # 2 weeks around today
    3-24  7:08   13:19  19:31    -92.98676   46.65793 (  1.62280)  12h 23m
    3-25  7:07   13:19  19:32    -93.54262   47.05097 (  2.01588)  12h 25m
    3-26  7:05   13:19  19:33    -94.09774   47.44332 (  2.40827)  12h 28m
    3-27  7:03   13:18  19:35    -94.65201   47.83488 (  2.79986)  12h 32m
    3-28  7:01   13:18  19:36    -95.20532   48.22555 (  3.19056)  12h 35m
    3-29  6:59   13:18  19:37    -95.75754   48.61521 (  3.58025)  12h 38m
    3-30  6:57   13:17  19:38    -96.30854   49.00378 (  3.96883)  12h 41m
    3-31  6:55   13:17  19:40    -96.85821   49.39114 (  4.35621)  12h 45m
    4-01  6:54   13:17  19:41    -97.40701   49.77719 (  4.74228)  12h 47m
    4-02  6:52   13:17  19:42    -97.95305   50.16183 (  5.12693)  12h 50m
    4-03  6:50   13:16  19:43    -98.49797   50.54494 (  5.51006)  12h 53m
    4-04  6:48   13:16  19:45    -99.04104   50.92643 (  5.89158)  12h 57m
    4-05  6:46   13:16  19:46    -99.58216   51.30620 (  6.27138)  13h  0m
    4-06  6:44   13:15  19:47   -100.12120   51.68414 (  6.64935)  13h  3m
    4-07  6:42   13:15  19:48   -100.65804   52.06016 (  7.02541)  13h  6m
    :loc MSP.
    
    $ bb -cp src:resources -m find-events.core 2022 0  # 20th of each month
     1-20  7:44   12:24  17:04    -61.96443   25.02906 (-20.00632)   9h 20m
     2-20  7:06   12:27  17:48    -75.48952   34.30480 (-10.73084)  10h 42m
     3-20  7:16   13:20  19:26    -90.75819   45.08078 (  0.04553)  12h 10m
     4-20  6:20   13:12  20:05   -107.38235   56.73051 ( 11.69618)  13h 45m
     5-20  5:39   13:10  20:41   -119.90157   65.12042 ( 20.08732)  15h  2m
     6-20  5:26   13:15  21:03   -125.21690   68.46823 ( 23.43642)  15h 37m
     7-20  5:46   13:19  20:53   -120.81076   65.58791 ( 20.55685)  15h  7m
     8-20  6:21   13:16  20:11   -108.48871   57.29853 ( 12.26755)  13h 50m
     9-20  6:58   13:06  19:14    -92.23053   45.92263 (  0.89111)  12h 16m  
    10-20  7:35   12:58  18:19    -76.01872   34.51407 (-10.51842)  10h 44m 
    11-20  7:17   11:59  16:40    -62.40308   25.23458 (-19.79923)   9h 23m
    12-20  7:48   12:11  16:34    -56.80228   21.60209 (-23.43294)   8h 46m

    2022    3-20 10:33    6-21  4:14    9-22 20:03   12-21 15:48 
    :loc MSP.

    $ clojure -M -m find-events.core 2022 3 13 15  # 15 days from date
    3-13  7:29   13:22  19:17    -86.85447   42.31590 ( -2.71944)  11h 48m
    3-14  7:27   13:22  19:18    -87.41137   42.71032 ( -2.32500)  11h 51m
    3-15  7:25   13:22  19:19    -87.96871   43.10509 ( -1.93021)  11h 54m
    3-16  7:23   13:22  19:20    -88.52638   43.50011 ( -1.53517)  11h 57m
    3-17  7:22   13:21  19:22    -89.08427   43.89529 ( -1.13999)  12h  0m
    3-18  7:20   13:21  19:23    -89.64227   44.29052 ( -0.74475)  12h  3m
    3-19  7:18   13:21  19:24    -90.20028   44.68571 ( -0.34955)  12h  6m
    3-20  7:16   13:20  19:26    -90.75819   45.08078 (  0.04553)  12h 10m
    3-21  7:14   13:20  19:27    -91.31589   45.47563 (  0.44040)  12h 13m
    3-22  7:12   13:20  19:28    -91.87329   45.87016 (  0.83496)  12h 16m
    3-23  7:10   13:20  19:29    -92.43028   46.26430 (  1.22913)  12h 19m
    3-24  7:08   13:19  19:31    -92.98676   46.65793 (  1.62280)  12h 23m
    3-25  7:07   13:19  19:32    -93.54262   47.05097 (  2.01588)  12h 25m
    3-26  7:05   13:19  19:33    -94.09774   47.44332 (  2.40827)  12h 28m
    3-27  7:03   13:18  19:35    -94.65201   47.83488 (  2.79986)  12h 32m
    :loc MSP.

### Limitations

Observer data and logic are simplified -- just enough to adjust times to zone and for DST (as of present US rules).


### Bugs

None known.
You should assume any discrepancies compared to  SkyviewCafe  are my responsibility.

## License

Other than credited above:
Copyright © 2018-2022   L. E. Vandergriff

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.

   ```
"Freely you have received, freely give."  Mt. 10:8
   ```
