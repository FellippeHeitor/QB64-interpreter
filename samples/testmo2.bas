KEY OFF
CLS
DO
    k$ = INKEY$
    LOCATE 1, 1
    i = _MOUSEINPUT
    PRINT "Use your mouse..."
    PRINT
    PRINT "Mouse X =";
    PRINT _MOUSEX
    PRINT "Mouse Y =";
    PRINT _MOUSEY
    PRINT "Mouse Button 1 =";
    PRINT _MOUSEBUTTON(1)
    PRINT "Mouse Button 2 =";
    PRINT _MOUSEBUTTON(2)

    LOCATE _MOUSEY, _MOUSEX
    IF _MOUSEBUTTON(1) THEN
        PRINT CHR$(219);
    END IF

    IF _MOUSEBUTTON(2) THEN
        PRINT CHR$(176);
    END IF

    _DISPLAY
LOOP UNTIL k$ = CHR$(27)
KEY ON
