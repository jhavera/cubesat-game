IDENTIFICATION DIVISION.
PROGRAM-ID. CUBESAT-DEBRIS-TRACKER.

*> ============================================================
*> CUBESAT SPACE DEBRIS DETECTION ADVENTURE GAME
*> You are the operator of SENTINEL-1, a CubeSat in LEO
*> equipped with an optical debris sensor. Three satellites
*> subscribe to your collision warning service.
*>
*> COMPILE: cobc -x -free -o cubesat-game cubesat-game.cob
*> RUN:     ./cubesat-game
*> ============================================================

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
REPOSITORY.
    FUNCTION ALL INTRINSIC.

DATA DIVISION.
WORKING-STORAGE SECTION.

*> CLEAR SCREEN COMMAND (macOS/Linux)
01 WS-CLEAR-CMD       PIC X(6) VALUE "clear".

*> GAME CONFIGURATION
01 WS-MAX-TURNS        PIC 9(2) VALUE 5.
01 WS-TOTAL-DEBRIS     PIC 9(5) VALUE 30000.
01 WS-MAX-DETECTED     PIC 9(2) VALUE 10.

*> SUBSCRIBER SATELLITE TABLE (3 SATELLITES)
01 WS-SATELLITE-TABLE.
   05 WS-SAT OCCURS 3 TIMES.
      10 WS-SAT-NAME        PIC X(16).
      10 WS-SAT-AGENCY      PIC X(12).
      10 WS-SAT-ALTITUDE    PIC 9(4).
      10 WS-SAT-INCLINATION PIC 9(3).
      10 WS-SAT-LONGITUDE   PIC S9(3).
      10 WS-SAT-LATITUDE    PIC S9(3).
      10 WS-SAT-VELOCITY    PIC 9(5)V99.
      10 WS-SAT-RISK        PIC 9V9(4).
      10 WS-SAT-STATUS      PIC X(10).
         88 SAT-NOMINAL     VALUE "NOMINAL".
         88 SAT-WARNING     VALUE "WARNING".
         88 SAT-CRITICAL    VALUE "CRITICAL".
      10 WS-SAT-WARNED      PIC 9 VALUE 0.
         88 SAT-WAS-WARNED  VALUE 1.

*> CUBESAT (PLAYER) STATUS
01 WS-CUBESAT-STATUS.
   05 WS-CUBE-NAME    PIC X(12) VALUE "SENTINEL-1".
   05 WS-CUBE-ALT     PIC 9(4)  VALUE 0550.
   05 WS-CUBE-POWER   PIC 9(3)  VALUE 100.
   05 WS-CUBE-FUEL    PIC 9(3)  VALUE 100.
   05 WS-CUBE-SENSOR  PIC X(10) VALUE "READY".
      88 SENSOR-READY    VALUE "READY".
      88 SENSOR-BUSY     VALUE "BUSY".
      88 SENSOR-COOLDOWN VALUE "COOLDOWN".

*> SCAN PARAMETERS (PLAYER INPUT)
01 WS-SCAN-PARAMS.
   05 WS-SCAN-AZ-CHOICE  PIC 9.
   05 WS-SCAN-EL-CHOICE  PIC 9.
   05 WS-SCAN-RNG-CHOICE PIC 9.
   05 WS-SCAN-AZ-MIN     PIC 9(3).
   05 WS-SCAN-AZ-MAX     PIC 9(3).
   05 WS-SCAN-EL-MIN     PIC S9(3).
   05 WS-SCAN-EL-MAX     PIC S9(3).
   05 WS-SCAN-RNG-MIN    PIC 9(4).
   05 WS-SCAN-RNG-MAX    PIC 9(4).
   05 WS-SCAN-FOV        PIC 9(5).

*> DETECTED DEBRIS TABLE (UP TO 10 PER SCAN)
01 WS-DEBRIS-TABLE.
   05 WS-DEBRIS-COUNT PIC 9(2) VALUE 0.
   05 WS-DEBRIS OCCURS 10 TIMES.
      10 WS-DEB-ID       PIC X(8).
      10 WS-DEB-AZ       PIC 9(3).
      10 WS-DEB-EL       PIC S9(3).
      10 WS-DEB-RANGE    PIC 9(4).
      10 WS-DEB-VELOCITY PIC 9(5)V99.
      10 WS-DEB-SIZE     PIC X(8).
         88 DEB-SMALL    VALUE "SMALL".
         88 DEB-MEDIUM   VALUE "MEDIUM".
         88 DEB-LARGE    VALUE "LARGE".
      10 WS-DEB-THREAT   PIC X(8).
         88 THREAT-LOW      VALUE "LOW".
         88 THREAT-MODERATE VALUE "MODERATE".
         88 THREAT-HIGH     VALUE "HIGH".
         88 THREAT-CRITICAL VALUE "CRITICAL".

*> MANEUVER OPTIONS
01 WS-MANEUVER-TABLE.
   05 WS-MANEUVER-CHOICE PIC 9.
   05 WS-TARGET-SAT      PIC 9.
   05 WS-MAN-DESC        PIC X(30).

*> GAME STATE
01 WS-GAME-STATE.
   05 WS-CURRENT-TURN  PIC 9(2) VALUE 0.
   05 WS-SCORE         PIC S9(4) VALUE 0.
   05 WS-COLLISIONS    PIC 9(2) VALUE 0.
   05 WS-WARNINGS-SENT PIC 9(2) VALUE 0.
   05 WS-SCANS-DONE    PIC 9(2) VALUE 0.
   05 WS-GAME-OVER     PIC 9 VALUE 0.
      88 GAME-ACTIVE   VALUE 0.
      88 GAME-ENDED    VALUE 1.

*> WORKING VARIABLES
01 WS-WORK-FIELDS.
   05 WS-IDX           PIC 9(2).
   05 WS-IDX2          PIC 9(2).
   05 WS-RAND-NUM      PIC 9V9(6).
   05 WS-RAND-INT      PIC 9(5).
   05 WS-TEMP-RISK     PIC 9V9(4).
   05 WS-HIGHEST-RISK  PIC 9V9(4).
   05 WS-HIGHEST-SAT   PIC 9.
   05 WS-DEBRIS-IN-SCAN PIC 9(5).
   05 WS-INPUT-BUFFER  PIC X(10).
   05 WS-CONFIRM       PIC X.
   05 WS-RISK-DISPLAY  PIC Z9.9999.
   05 WS-POWER-COST    PIC 9(2).
   05 WS-EVENT-ROLL    PIC 9V9(4).
   05 WS-SEED-INIT     PIC 9 VALUE 0.
   05 WS-SCORE-DISP    PIC S9(4) SIGN LEADING SEPARATE.

*> DISPLAY FORMATTING
01 WS-SEPARATOR PIC X(56) VALUE
   "========================================================".
01 WS-THIN-SEP PIC X(56) VALUE
   "--------------------------------------------------------".

PROCEDURE DIVISION.

0000-MAIN-CONTROL.
    PERFORM 1000-INITIALIZE-GAME
    PERFORM 2000-SHOW-INTRO
    PERFORM 3000-GAME-LOOP
       UNTIL GAME-ENDED
    PERFORM 9000-GAME-OVER
    STOP RUN
    .

*> UTILITY: CLEAR THE TERMINAL SCREEN
0100-CLEAR-SCREEN.
    CALL "SYSTEM" USING WS-CLEAR-CMD
    .

1000-INITIALIZE-GAME.
    IF WS-SEED-INIT = 0
        MOVE FUNCTION RANDOM(17943) TO WS-RAND-NUM
        MOVE 1 TO WS-SEED-INIT
    END-IF

    *> SATELLITE 1 - WEATHER SAT (NOAA)
    MOVE "WEATHERBIRD-7   " TO WS-SAT-NAME(1)
    MOVE "NOAA        "     TO WS-SAT-AGENCY(1)
    MOVE 0520                TO WS-SAT-ALTITUDE(1)
    MOVE 098                 TO WS-SAT-INCLINATION(1)
    MOVE -045                TO WS-SAT-LONGITUDE(1)
    MOVE 035                 TO WS-SAT-LATITUDE(1)
    MOVE 07612.50            TO WS-SAT-VELOCITY(1)
    MOVE 0.0000              TO WS-SAT-RISK(1)
    SET SAT-NOMINAL(1)       TO TRUE
    MOVE 0                   TO WS-SAT-WARNED(1)

    *> SATELLITE 2 - COMMS SAT (ESA)
    MOVE "GLOBALLINK-12   " TO WS-SAT-NAME(2)
    MOVE "ESA         "     TO WS-SAT-AGENCY(2)
    MOVE 0580                TO WS-SAT-ALTITUDE(2)
    MOVE 053                 TO WS-SAT-INCLINATION(2)
    MOVE 012                 TO WS-SAT-LONGITUDE(2)
    MOVE -020                TO WS-SAT-LATITUDE(2)
    MOVE 07580.30            TO WS-SAT-VELOCITY(2)
    MOVE 0.0000              TO WS-SAT-RISK(2)
    SET SAT-NOMINAL(2)       TO TRUE
    MOVE 0                   TO WS-SAT-WARNED(2)

    *> SATELLITE 3 - EARTH OBSERVATION (JAXA)
    MOVE "TERRAWATCH-3    " TO WS-SAT-NAME(3)
    MOVE "JAXA        "     TO WS-SAT-AGENCY(3)
    MOVE 0510                TO WS-SAT-ALTITUDE(3)
    MOVE 097                 TO WS-SAT-INCLINATION(3)
    MOVE 140                 TO WS-SAT-LONGITUDE(3)
    MOVE 045                 TO WS-SAT-LATITUDE(3)
    MOVE 07625.80            TO WS-SAT-VELOCITY(3)
    MOVE 0.0000              TO WS-SAT-RISK(3)
    SET SAT-NOMINAL(3)       TO TRUE
    MOVE 0                   TO WS-SAT-WARNED(3)
    .

2000-SHOW-INTRO.
    PERFORM 0100-CLEAR-SCREEN

    DISPLAY WS-SEPARATOR
    DISPLAY "  CUBESAT DEBRIS TRACKING COMMAND CENTER"
    DISPLAY "  ======================================"
    DISPLAY WS-SEPARATOR
    DISPLAY SPACES
    DISPLAY "  Welcome, Operator."
    DISPLAY SPACES
    DISPLAY "  You command " WS-CUBE-NAME ", a 3U CubeSat"
    DISPLAY "  in Low Earth Orbit at " WS-CUBE-ALT " km."
    DISPLAY "  Your mission: detect space debris and issue"
    DISPLAY "  collision warnings to subscriber satellites."
    DISPLAY SPACES
    DISPLAY "  ACTIVE DEBRIS OBJECTS: " WS-TOTAL-DEBRIS
    DISPLAY "  SUBSCRIBER SATELLITES: 3"
    DISPLAY "  MISSION DURATION:      "
       WS-MAX-TURNS " ORBITAL PASSES"
    DISPLAY SPACES
    DISPLAY WS-THIN-SEP
    DISPLAY "  SUBSCRIBER MANIFEST:"
    DISPLAY WS-THIN-SEP

    PERFORM VARYING WS-IDX FROM 1 BY 1
       UNTIL WS-IDX > 3
        DISPLAY "  [" WS-IDX "] "
           WS-SAT-NAME(WS-IDX)
           " | " WS-SAT-AGENCY(WS-IDX)
        DISPLAY "      Alt: " WS-SAT-ALTITUDE(WS-IDX)
           " km  Inc: " WS-SAT-INCLINATION(WS-IDX)
           " deg  Status: " WS-SAT-STATUS(WS-IDX)
    END-PERFORM

    DISPLAY WS-THIN-SEP
    DISPLAY SPACES
    DISPLAY "  Press ENTER to begin mission..."
    ACCEPT WS-INPUT-BUFFER
    .

3000-GAME-LOOP.
    ADD 1 TO WS-CURRENT-TURN

    IF WS-CURRENT-TURN > WS-MAX-TURNS
        SET GAME-ENDED TO TRUE
    ELSE
        PERFORM 3100-SHOW-STATUS
        PERFORM 3200-RANDOM-EVENT
        PERFORM 3300-CHOOSE-SCAN
        PERFORM 3400-EXECUTE-SCAN
        PERFORM 3500-ASSESS-THREATS
        PERFORM 3600-CHOOSE-ACTION
        PERFORM 3700-RESOLVE-TURN
    END-IF
    .

3100-SHOW-STATUS.
    PERFORM 0100-CLEAR-SCREEN
    MOVE WS-SCORE TO WS-SCORE-DISP

    DISPLAY WS-SEPARATOR
    DISPLAY "  ORBITAL PASS " WS-CURRENT-TURN
       " OF " WS-MAX-TURNS
    DISPLAY WS-SEPARATOR
    DISPLAY SPACES
    DISPLAY "  SENTINEL-1 SYSTEMS:"
    DISPLAY "    Power: " WS-CUBE-POWER
       "%  |  Fuel: " WS-CUBE-FUEL
       "%  |  Sensor: " WS-CUBE-SENSOR
    DISPLAY "    Score: " WS-SCORE-DISP
       "  |  Warnings Issued: " WS-WARNINGS-SENT
    DISPLAY SPACES
    DISPLAY "  SUBSCRIBER STATUS:"
    DISPLAY WS-THIN-SEP

    PERFORM VARYING WS-IDX FROM 1 BY 1
       UNTIL WS-IDX > 3
        MOVE WS-SAT-RISK(WS-IDX) TO WS-RISK-DISPLAY
        DISPLAY "  " WS-SAT-NAME(WS-IDX)
           " | Status: " WS-SAT-STATUS(WS-IDX)
           " | Risk: " WS-RISK-DISPLAY
    END-PERFORM

    DISPLAY WS-THIN-SEP
    .

3200-RANDOM-EVENT.
    MOVE FUNCTION RANDOM TO WS-EVENT-ROLL

    EVALUATE TRUE
        WHEN WS-EVENT-ROLL < 0.15
            DISPLAY SPACES
            DISPLAY "  ** SOLAR FLARE DETECTED **"
            DISPLAY "  Sensor interference! Power draw +5%"
            SUBTRACT 5 FROM WS-CUBE-POWER
            IF WS-CUBE-POWER < 10
                MOVE 10 TO WS-CUBE-POWER
            END-IF

        WHEN WS-EVENT-ROLL < 0.25
            DISPLAY SPACES
            DISPLAY "  ** GROUND STATION UPDATE **"
            DISPLAY "  New TLE data received. Debris catalog"
            DISPLAY "  updated with 47 new objects."
            ADD 47 TO WS-TOTAL-DEBRIS

        WHEN WS-EVENT-ROLL < 0.35
            DISPLAY SPACES
            DISPLAY "  ** BATTERY RECHARGE **"
            DISPLAY "  Passing through sunlight. Power +10%"
            ADD 10 TO WS-CUBE-POWER
            IF WS-CUBE-POWER > 100
                MOVE 100 TO WS-CUBE-POWER
            END-IF

        WHEN OTHER
            DISPLAY SPACES
            DISPLAY "  Systems nominal. No events detected."
    END-EVALUATE
    .

3300-CHOOSE-SCAN.
    DISPLAY SPACES
    DISPLAY "  CONFIGURE SENSOR SCAN"
    DISPLAY WS-THIN-SEP
    DISPLAY SPACES

    DISPLAY "  SELECT AZIMUTH SECTOR:"
    DISPLAY "    1) PROGRADE    (  0-120 deg) - Forward"
    DISPLAY "    2) LATERAL     (120-240 deg) - Sideways"
    DISPLAY "    3) RETROGRADE  (240-360 deg) - Behind"
    DISPLAY "  Choice (1-3): "
    ACCEPT WS-SCAN-AZ-CHOICE

    EVALUATE WS-SCAN-AZ-CHOICE
        WHEN 1
            MOVE 000 TO WS-SCAN-AZ-MIN
            MOVE 120 TO WS-SCAN-AZ-MAX
        WHEN 2
            MOVE 120 TO WS-SCAN-AZ-MIN
            MOVE 240 TO WS-SCAN-AZ-MAX
        WHEN 3
            MOVE 240 TO WS-SCAN-AZ-MIN
            MOVE 360 TO WS-SCAN-AZ-MAX
        WHEN OTHER
            DISPLAY "  Invalid. Defaulting to PROGRADE."
            MOVE 000 TO WS-SCAN-AZ-MIN
            MOVE 120 TO WS-SCAN-AZ-MAX
    END-EVALUATE

    DISPLAY SPACES
    DISPLAY "  SELECT ELEVATION BAND:"
    DISPLAY "    1) ABOVE    (+30 to +90 deg) - Higher orbit"
    DISPLAY "    2) HORIZON  (-30 to +30 deg) - Same plane"
    DISPLAY "    3) BELOW    (-90 to -30 deg) - Lower orbit"
    DISPLAY "  Choice (1-3): "
    ACCEPT WS-SCAN-EL-CHOICE

    EVALUATE WS-SCAN-EL-CHOICE
        WHEN 1
            MOVE +30 TO WS-SCAN-EL-MIN
            MOVE +90 TO WS-SCAN-EL-MAX
        WHEN 2
            MOVE -30 TO WS-SCAN-EL-MIN
            MOVE +30 TO WS-SCAN-EL-MAX
        WHEN 3
            MOVE -90 TO WS-SCAN-EL-MIN
            MOVE -30 TO WS-SCAN-EL-MAX
        WHEN OTHER
            DISPLAY "  Invalid. Defaulting to HORIZON."
            MOVE -30 TO WS-SCAN-EL-MIN
            MOVE +30 TO WS-SCAN-EL-MAX
    END-EVALUATE

    DISPLAY SPACES
    DISPLAY "  SELECT RANGE BAND:"
    DISPLAY "    1) CLOSE    (100-500 km)  - High detail"
    DISPLAY "    2) MEDIUM   (500-1000 km) - Balanced"
    DISPLAY "    3) FAR      (1000-2000 km)- Wide coverage"
    DISPLAY "  Choice (1-3): "
    ACCEPT WS-SCAN-RNG-CHOICE

    EVALUATE WS-SCAN-RNG-CHOICE
        WHEN 1
            MOVE 0100 TO WS-SCAN-RNG-MIN
            MOVE 0500 TO WS-SCAN-RNG-MAX
            MOVE 8 TO WS-POWER-COST
        WHEN 2
            MOVE 0500 TO WS-SCAN-RNG-MIN
            MOVE 1000 TO WS-SCAN-RNG-MAX
            MOVE 5 TO WS-POWER-COST
        WHEN 3
            MOVE 1000 TO WS-SCAN-RNG-MIN
            MOVE 2000 TO WS-SCAN-RNG-MAX
            MOVE 3 TO WS-POWER-COST
        WHEN OTHER
            DISPLAY "  Invalid. Defaulting to MEDIUM."
            MOVE 0500 TO WS-SCAN-RNG-MIN
            MOVE 1000 TO WS-SCAN-RNG-MAX
            MOVE 5 TO WS-POWER-COST
    END-EVALUATE

    DISPLAY SPACES
    DISPLAY "  Scan configured. Power cost: "
       WS-POWER-COST "%"
    DISPLAY "  TAKING PICTURE... Stand by."
    SUBTRACT WS-POWER-COST FROM WS-CUBE-POWER
    .

3400-EXECUTE-SCAN.
    MOVE 0 TO WS-DEBRIS-COUNT
    ADD 1 TO WS-SCANS-DONE

    MOVE FUNCTION RANDOM TO WS-RAND-NUM

    EVALUATE WS-SCAN-RNG-CHOICE
        WHEN 1
            COMPUTE WS-DEBRIS-COUNT =
               FUNCTION INTEGER(WS-RAND-NUM * 4) + 1
        WHEN 2
            COMPUTE WS-DEBRIS-COUNT =
               FUNCTION INTEGER(WS-RAND-NUM * 6) + 2
        WHEN 3
            COMPUTE WS-DEBRIS-COUNT =
               FUNCTION INTEGER(WS-RAND-NUM * 8) + 3
        WHEN OTHER
            MOVE 3 TO WS-DEBRIS-COUNT
    END-EVALUATE

    IF WS-DEBRIS-COUNT > WS-MAX-DETECTED
        MOVE WS-MAX-DETECTED TO WS-DEBRIS-COUNT
    END-IF

    PERFORM VARYING WS-IDX FROM 1 BY 1
       UNTIL WS-IDX > WS-DEBRIS-COUNT

        MOVE FUNCTION RANDOM TO WS-RAND-NUM
        STRING "DBR-" WS-CURRENT-TURN WS-IDX
           DELIMITED BY SIZE INTO WS-DEB-ID(WS-IDX)

        COMPUTE WS-DEB-AZ(WS-IDX) =
           WS-SCAN-AZ-MIN +
           FUNCTION INTEGER(WS-RAND-NUM *
           (WS-SCAN-AZ-MAX - WS-SCAN-AZ-MIN))

        MOVE FUNCTION RANDOM TO WS-RAND-NUM
        COMPUTE WS-DEB-EL(WS-IDX) =
           WS-SCAN-EL-MIN +
           FUNCTION INTEGER(WS-RAND-NUM *
           (WS-SCAN-EL-MAX - WS-SCAN-EL-MIN))

        MOVE FUNCTION RANDOM TO WS-RAND-NUM
        COMPUTE WS-DEB-RANGE(WS-IDX) =
           WS-SCAN-RNG-MIN +
           FUNCTION INTEGER(WS-RAND-NUM *
           (WS-SCAN-RNG-MAX - WS-SCAN-RNG-MIN))

        MOVE FUNCTION RANDOM TO WS-RAND-NUM
        COMPUTE WS-DEB-VELOCITY(WS-IDX) =
           7000 + FUNCTION INTEGER(WS-RAND-NUM * 1500)

        MOVE FUNCTION RANDOM TO WS-RAND-NUM
        EVALUATE TRUE
            WHEN WS-RAND-NUM < 0.50
                SET DEB-SMALL(WS-IDX) TO TRUE
            WHEN WS-RAND-NUM < 0.82
                SET DEB-MEDIUM(WS-IDX) TO TRUE
            WHEN OTHER
                SET DEB-LARGE(WS-IDX) TO TRUE
        END-EVALUATE
    END-PERFORM

    *> FRESH SCREEN FOR SCAN RESULTS
    PERFORM 0100-CLEAR-SCREEN

    DISPLAY WS-SEPARATOR
    DISPLAY "  SENSOR IMAGE ANALYSIS COMPLETE"
    DISPLAY WS-SEPARATOR
    DISPLAY "  Objects detected: " WS-DEBRIS-COUNT
    DISPLAY SPACES
    DISPLAY "  ID       AZ   EL    RANGE  VEL      SIZE"
    DISPLAY WS-THIN-SEP

    PERFORM VARYING WS-IDX FROM 1 BY 1
       UNTIL WS-IDX > WS-DEBRIS-COUNT
        DISPLAY "  " WS-DEB-ID(WS-IDX)
           "  " WS-DEB-AZ(WS-IDX)
           "  " WS-DEB-EL(WS-IDX)
           "   " WS-DEB-RANGE(WS-IDX)
           "  " WS-DEB-VELOCITY(WS-IDX)
           "  " WS-DEB-SIZE(WS-IDX)
    END-PERFORM

    DISPLAY WS-THIN-SEP
    .

3500-ASSESS-THREATS.
    DISPLAY SPACES
    DISPLAY "  COMPUTING COLLISION PROBABILITIES..."
    DISPLAY SPACES

    MOVE 0.0000 TO WS-HIGHEST-RISK
    MOVE 1 TO WS-HIGHEST-SAT

    PERFORM VARYING WS-IDX FROM 1 BY 1
       UNTIL WS-IDX > 3

        MOVE 0.0000 TO WS-SAT-RISK(WS-IDX)

        PERFORM VARYING WS-IDX2 FROM 1 BY 1
           UNTIL WS-IDX2 > WS-DEBRIS-COUNT

            COMPUTE WS-TEMP-RISK =
               1.0 / (WS-DEB-RANGE(WS-IDX2) + 1)

            EVALUATE TRUE
                WHEN DEB-LARGE(WS-IDX2)
                    COMPUTE WS-TEMP-RISK =
                       WS-TEMP-RISK * 3.0
                WHEN DEB-MEDIUM(WS-IDX2)
                    COMPUTE WS-TEMP-RISK =
                       WS-TEMP-RISK * 1.5
                WHEN OTHER
                    CONTINUE
            END-EVALUATE

            MOVE FUNCTION RANDOM TO WS-RAND-NUM
            IF WS-SCAN-EL-CHOICE = 2
                COMPUTE WS-TEMP-RISK =
                   WS-TEMP-RISK * (1.5 + WS-RAND-NUM)
            END-IF

            ADD WS-TEMP-RISK TO WS-SAT-RISK(WS-IDX)
        END-PERFORM

        IF WS-SAT-RISK(WS-IDX) > 0.9999
            MOVE 0.9999 TO WS-SAT-RISK(WS-IDX)
        END-IF

        EVALUATE TRUE
            WHEN WS-SAT-RISK(WS-IDX) >= 0.5000
                SET SAT-CRITICAL(WS-IDX) TO TRUE
            WHEN WS-SAT-RISK(WS-IDX) >= 0.2000
                SET SAT-WARNING(WS-IDX) TO TRUE
            WHEN OTHER
                SET SAT-NOMINAL(WS-IDX) TO TRUE
        END-EVALUATE

        IF WS-SAT-RISK(WS-IDX) > WS-HIGHEST-RISK
            MOVE WS-SAT-RISK(WS-IDX) TO WS-HIGHEST-RISK
            MOVE WS-IDX TO WS-HIGHEST-SAT
        END-IF

        MOVE WS-SAT-RISK(WS-IDX) TO WS-RISK-DISPLAY
        DISPLAY "  " WS-SAT-NAME(WS-IDX)
           " -> Risk: " WS-RISK-DISPLAY
           " [" WS-SAT-STATUS(WS-IDX) "]"
    END-PERFORM

    DISPLAY SPACES
    MOVE WS-HIGHEST-RISK TO WS-RISK-DISPLAY
    DISPLAY "  >> HIGHEST THREAT: "
       WS-SAT-NAME(WS-HIGHEST-SAT)
       " (Risk: " WS-RISK-DISPLAY ")"
    DISPLAY SPACES

    PERFORM VARYING WS-IDX FROM 1 BY 1
       UNTIL WS-IDX > WS-DEBRIS-COUNT
        MOVE FUNCTION RANDOM TO WS-RAND-NUM
        EVALUATE TRUE
            WHEN WS-HIGHEST-RISK >= 0.5
               AND WS-RAND-NUM > 0.5
                SET THREAT-CRITICAL(WS-IDX) TO TRUE
            WHEN WS-HIGHEST-RISK >= 0.2
                SET THREAT-MODERATE(WS-IDX) TO TRUE
            WHEN OTHER
                SET THREAT-LOW(WS-IDX) TO TRUE
        END-EVALUATE
    END-PERFORM
    .

3600-CHOOSE-ACTION.
    DISPLAY WS-THIN-SEP
    DISPLAY "  OPERATOR DECISION REQUIRED"
    DISPLAY WS-THIN-SEP
    DISPLAY SPACES
    DISPLAY "  Which satellite should receive a warning?"
    DISPLAY "    1) " WS-SAT-NAME(1)
    DISPLAY "    2) " WS-SAT-NAME(2)
    DISPLAY "    3) " WS-SAT-NAME(3)
    DISPLAY "    4) No warning (conserve fuel/power)"
    DISPLAY "  Choice (1-4): "
    ACCEPT WS-TARGET-SAT

    IF WS-TARGET-SAT >= 1 AND WS-TARGET-SAT <= 3
        DISPLAY SPACES
        DISPLAY "  RECOMMEND MANEUVER FOR "
           WS-SAT-NAME(WS-TARGET-SAT) ":"
        DISPLAY "    1) RAISE ORBIT    (+2 km altitude)"
        DISPLAY "    2) LOWER ORBIT    (-2 km altitude)"
        DISPLAY "    3) LATERAL THRUST (shift ground track)"
        DISPLAY "    4) RETROGRADE BURN (slow down)"
        DISPLAY "  Choice (1-4): "
        ACCEPT WS-MANEUVER-CHOICE

        EVALUATE WS-MANEUVER-CHOICE
            WHEN 1
                MOVE "RAISE ORBIT (+2 KM)" TO WS-MAN-DESC
                ADD 2 TO WS-SAT-ALTITUDE(WS-TARGET-SAT)
            WHEN 2
                MOVE "LOWER ORBIT (-2 KM)" TO WS-MAN-DESC
                SUBTRACT 2
                   FROM WS-SAT-ALTITUDE(WS-TARGET-SAT)
            WHEN 3
                MOVE "LATERAL THRUST" TO WS-MAN-DESC
            WHEN 4
                MOVE "RETROGRADE BURN" TO WS-MAN-DESC
            WHEN OTHER
                MOVE "NO MANEUVER" TO WS-MAN-DESC
        END-EVALUATE

        MOVE 1 TO WS-SAT-WARNED(WS-TARGET-SAT)

        DISPLAY SPACES
        DISPLAY "  >> WARNING TRANSMITTED TO "
           WS-SAT-NAME(WS-TARGET-SAT)
        DISPLAY "  >> RECOMMENDED: " WS-MAN-DESC
        SUBTRACT 2 FROM WS-CUBE-FUEL
    ELSE
        DISPLAY SPACES
        DISPLAY "  No warning issued. Conserving resources."
        MOVE 0 TO WS-TARGET-SAT
    END-IF
    .

3700-RESOLVE-TURN.
    DISPLAY SPACES
    DISPLAY WS-THIN-SEP
    DISPLAY "  TURN RESOLUTION - PASS " WS-CURRENT-TURN
    DISPLAY WS-THIN-SEP

    IF WS-TARGET-SAT = WS-HIGHEST-SAT
       AND WS-TARGET-SAT > 0
        DISPLAY SPACES
        DISPLAY "  EXCELLENT! You warned the satellite with"
        DISPLAY "  the highest collision probability!"
        DISPLAY "  " WS-SAT-NAME(WS-TARGET-SAT)
           " executes " WS-MAN-DESC
        DISPLAY "  Collision AVOIDED. +200 points!"
        ADD 200 TO WS-SCORE
        ADD 1 TO WS-WARNINGS-SENT
    ELSE IF WS-TARGET-SAT > 0
            AND WS-TARGET-SAT NOT = WS-HIGHEST-SAT
        DISPLAY SPACES
        DISPLAY "  You warned "
           WS-SAT-NAME(WS-TARGET-SAT) ","
        DISPLAY "  but "
           WS-SAT-NAME(WS-HIGHEST-SAT)
           " had the highest risk!"

        MOVE FUNCTION RANDOM TO WS-RAND-NUM
        IF WS-RAND-NUM < WS-HIGHEST-RISK
            DISPLAY "  ** NEAR MISS for "
               WS-SAT-NAME(WS-HIGHEST-SAT) "! **"
            DISPLAY "  +50 points (partial credit)"
            ADD 50 TO WS-SCORE
        ELSE
            DISPLAY "  Fortunately, no collision occurred."
            DISPLAY "  +75 points."
            ADD 75 TO WS-SCORE
        END-IF
        ADD 1 TO WS-WARNINGS-SENT
    ELSE
        DISPLAY SPACES
        DISPLAY "  No warning was issued."
        MOVE FUNCTION RANDOM TO WS-RAND-NUM
        IF WS-RAND-NUM < WS-HIGHEST-RISK
            DISPLAY "  ** DEBRIS IMPACT on "
               WS-SAT-NAME(WS-HIGHEST-SAT) "! **"
            DISPLAY "  Satellite damaged! -100 points!"
            SUBTRACT 100 FROM WS-SCORE
            ADD 1 TO WS-COLLISIONS
        ELSE
            DISPLAY "  Lucky! No collisions this pass."
            DISPLAY "  +25 points for resource conservation."
            ADD 25 TO WS-SCORE
        END-IF
    END-IF

    PERFORM VARYING WS-IDX FROM 1 BY 1
       UNTIL WS-IDX > 3
        MOVE 0 TO WS-SAT-WARNED(WS-IDX)
    END-PERFORM

    PERFORM VARYING WS-IDX FROM 1 BY 1
       UNTIL WS-IDX > 3
        MOVE FUNCTION RANDOM TO WS-RAND-NUM
        COMPUTE WS-SAT-LONGITUDE(WS-IDX) =
           FUNCTION MOD(
              WS-SAT-LONGITUDE(WS-IDX) + 25, 360)
           - 180
        MOVE FUNCTION RANDOM TO WS-RAND-NUM
        COMPUTE WS-SAT-LATITUDE(WS-IDX) =
           FUNCTION MOD(
              WS-SAT-LATITUDE(WS-IDX) + 10, 180)
           - 90
    END-PERFORM

    IF WS-CUBE-POWER < 15
        DISPLAY SPACES
        DISPLAY "  *** LOW POWER WARNING ***"
        DISPLAY "  SENTINEL-1 power at " WS-CUBE-POWER "%"
        DISPLAY "  Entering power-save mode. +15% recovered."
        ADD 15 TO WS-CUBE-POWER
    END-IF

    IF WS-CUBE-FUEL < 10
        DISPLAY SPACES
        DISPLAY "  *** LOW FUEL WARNING ***"
        DISPLAY "  Communication thruster fuel critical!"
    END-IF

    DISPLAY SPACES
    DISPLAY "  Press ENTER for next orbital pass..."
    ACCEPT WS-INPUT-BUFFER
    .

9000-GAME-OVER.
    PERFORM 0100-CLEAR-SCREEN
    MOVE WS-SCORE TO WS-SCORE-DISP

    DISPLAY WS-SEPARATOR
    DISPLAY "  MISSION COMPLETE - FINAL REPORT"
    DISPLAY WS-SEPARATOR
    DISPLAY SPACES
    DISPLAY "  Operator:         YOU"
    DISPLAY "  CubeSat:          " WS-CUBE-NAME
    DISPLAY "  Orbital Passes:   " WS-MAX-TURNS
    DISPLAY "  Scans Performed:  " WS-SCANS-DONE
    DISPLAY "  Warnings Issued:  " WS-WARNINGS-SENT
    DISPLAY "  Collisions:       " WS-COLLISIONS
    DISPLAY "  Remaining Power:  " WS-CUBE-POWER "%"
    DISPLAY "  Remaining Fuel:   " WS-CUBE-FUEL "%"
    DISPLAY SPACES
    DISPLAY "  FINAL SCORE: " WS-SCORE-DISP " POINTS"
    DISPLAY SPACES

    EVALUATE TRUE
        WHEN WS-SCORE >= 800
            DISPLAY "  RATING: ***** ELITE OPERATOR *****"
            DISPLAY "  Outstanding! Space is safer because"
            DISPLAY "  of you. NASA wants your number."
        WHEN WS-SCORE >= 500
            DISPLAY "  RATING: ****  SKILLED OPERATOR  ****"
            DISPLAY "  Great work! Your subscribers trust"
            DISPLAY "  your judgment."
        WHEN WS-SCORE >= 200
            DISPLAY "  RATING: ***   CAPABLE OPERATOR   ***"
            DISPLAY "  Solid performance. Room for growth"
            DISPLAY "  but satellites survived."
        WHEN WS-SCORE >= 0
            DISPLAY "  RATING: **    NOVICE OPERATOR    **"
            DISPLAY "  Some close calls. Consider more"
            DISPLAY "  training before next mission."
        WHEN OTHER
            DISPLAY "  RATING: *     NEEDS IMPROVEMENT    *"
            DISPLAY "  Satellites were damaged on your watch."
            DISPLAY "  Review debris tracking procedures."
    END-EVALUATE

    DISPLAY SPACES
    DISPLAY WS-SEPARATOR
    DISPLAY "  Thank you for protecting Low Earth Orbit!"
    DISPLAY WS-SEPARATOR
    DISPLAY SPACES
    .
    