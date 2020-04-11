KEY OFF
DO
    CLS
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

    _LIMIT 60
    _DISPLAY
LOOP
KEY ON
