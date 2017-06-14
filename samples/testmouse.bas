SCREEN 13
KEY OFF
PRINT "Click around..."
DO
    IF _MOUSEBUTTON(1) = -1 THEN
        r = RND * 50
        c% = RND * 255
        CIRCLE (_MOUSEX, _MOUSEY), r, c%
    END IF

    _LIMIT 60
    _DISPLAY
LOOP
KEY ON
