SCREEN 13
DO
    x = RND * _WIDTH
    y = RND * _HEIGHT
    r = RND * 20
    c% = RND * 255

    CIRCLE (x, y), r, c%

    _DISPLAY
LOOP
