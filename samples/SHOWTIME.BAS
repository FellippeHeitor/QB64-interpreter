KEY OFF
DO
    CLS
    a = RND * 14
    a = a + 1
    COLOR a
    LOCATE 1, 1
    PRINT TIME$
    _DISPLAY
    _LIMIT 30
    a$ = INKEY$
    b = LEN(a$)
    IF b > 0 THEN
        PRINT "A key was pressed."
        COLOR 7, 0
        KEY ON
        EXIT DO
    END IF
LOOP
