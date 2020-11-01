i = 0
j = 0
DO
    i = i + 1
    COLOR i
    PRINT "I =" + STR$(i)
    DO
        j = j + 1
        COLOR j
        PRINT "J LOOP" + STR$(j)
    LOOP WHILE j < 3
    j = 0
LOOP UNTIL i > 4
