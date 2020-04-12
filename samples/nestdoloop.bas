i = 0
j = 0
DO
    i = i + 1
    PRINT "I =" + STR$(i)
    DO
        j = j + 1
        PRINT "J LOOP" + STR$(j)
    LOOP WHILE j < 3
    j = 0
LOOP UNTIL i > 4
