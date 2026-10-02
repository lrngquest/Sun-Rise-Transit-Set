# Sun Rise-Transit-Set

A Clojure learning exercise printing Sun events for an observer location
- month and day
- rise, transit, set times
- rise azimuth (clockwise West of South)
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

May be run from dir containing  /src  with Clojure, or using released bb jar.

- no args         ==>  rise,set ... for two-weeks around today's date
- 2 args: year 0  ==>  ditto for 20th of each month; equinox/solstice date,time
  2 args: year 1  ==>  equinox & solstice details
- 3 args: year start-day num-of-weeks  ==> rise,set,transit... for each week
- 4 args: year month day num-of-days   ==> ditto for days requested

A  'config.edn' file (in current execution dir) can be used to override the default observer location of MSP airport.


## Examples
    
     $ clojure -M -m find-events.core 2026 0    # 20th of each month (2026)
     1-20  7:44 12:24 17:04  -61.97561   25.03657 (-19.99846)   9h 20m
     2-20  7:06 12:27 17:48  -75.50587   34.31628 (-10.71771)  10h 43m
     3-20  7:16 13:20 19:26  -90.77341   45.09163 (  0.05870)  12h 10m
     4-20  6:20 13:12 20:05 -107.39501   56.73927 ( 11.70738)  13h 45m
     5-20  5:39 13:10 20:41 -119.90935   65.12548 ( 20.09425)  15h  2m
     6-20  5:26 13:15 21:03 -125.21654   68.46792 ( 23.43677)  15h 37m
     7-20  5:46 13:19 20:53 -120.80093   65.58145 ( 20.54968)  15h  7m
     8-20  6:21 13:16 20:11 -108.47154   57.28671 ( 12.25390)  13h 50m
     9-20  6:58 13:06 19:14  -92.21060   45.90858 (  0.87469)  12h 16m
    10-20  7:35 12:58 18:19  -76.00056   34.50147 (-10.53332)  10h 44m
    11-20  7:17 11:59 16:40  -62.39212   25.22736 (-19.80797)   9h 22m
    12-20  7:48 12:11 16:34  -56.80296   21.60258 (-23.43262)   8h 46m

    2026  3-20  9:46    6-21  3:24    9-22 19:05   12-21 14:50   leap:0  Dday: Sat
    :loc MSP


    $ bb rts.jar 2026 9 23 5   # 5 days forward from date
    9-23  7:01 13:05 19:08  -90.56026   44.74245 ( -0.29155)  12h  7m
    9-24  7:03 13:05 19:06  -90.00971   44.35336 ( -0.68066)  12h  4m
    9-25  7:04 13:05 19:05  -89.45912   43.96424 ( -1.06981)  12h  1m
    9-26  7:05 13:04 19:03  -88.90858   43.57516 ( -1.45890)  11h 58m
    9-27  7:06 13:04 19:01  -88.35818   43.18622 ( -1.84786)  11h 54m
    
 
    $ bb rts.jar 2026 1
    year 2026   Equinox,solstice details
    3-20  9:46   297.75209   24.99409 (  0.00045)  12h 10m
    6-21  3:24   210.94664  -15.37445 ( 23.43795)  15h 37m
    9-22 19:05    90.14744   -0.14667 ( -0.00032)  12h 10m
   12-21 14:50    37.19447   12.46937 (-23.43744)   8h 46m


### Limitations

Observer data and logic are simplified -- just enough to adjust times to zone and for DST (US rules at time of writing) .


### Bugs

None known.
You should assume any discrepancies compared to  SkyviewCafe  are my responsibility.

## License

Other than credited above:
Copyright © 2018-2026   L. E. Vandergriff

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.

   ```
"Freely you have received, freely give."  Mt. 10:8
   ```
