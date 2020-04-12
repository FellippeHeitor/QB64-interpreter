DO WHILE i < 4
    i = i + 1
    PRINT "I =" + STR$(i)
    DO UNTIL j > 3
        j = j + 1
        PRINT "J LOOP" + STR$(j)
    LOOP
    j = 0
LOOP
