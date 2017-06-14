PRINT "Counting to 10:"
cls
a = 0
DO
    a = a + 1
    COLOR a
    PRINT a
    IF a = 10 THEN
        EXIT DO
    END IF
LOOP
COLOR 15
PRINT "Done."
COLOR 7
