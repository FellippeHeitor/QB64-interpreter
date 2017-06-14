CLS
PRINT "Seconds to count:";
INPUT sec
IF sec = 0 THEN
    PRINT "Bummer..."
    END
END IF

IF sec > 10 THEN
    sec = 10
END IF

PRINT "Will wait for";
PRINT sec;
PRINT "seconds..."

s = TIMER
DO
    t = TIMER - s
    IF t > sec THEN
        EXIT DO
    END IF
LOOP

PRINT "done!"
