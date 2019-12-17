OPTION _EXPLICIT

$CONSOLE

CONST true = -1, false = NOT true

DIM SHARED debugging AS _BYTE
debugging = true
'ON ERROR GOTO oops

IF debugging = false THEN
    _CONSOLE OFF
ELSE
    _SCREENMOVE _SCREENX + 800, _SCREENY
END IF

KEY 2, "LIST" + CHR$(13)
KEY 3, "LOAD "
KEY 4, "SAVE "
KEY 5, "RUN" + CHR$(13)
KEY ON

TYPE vartype
    name AS STRING * 40
    type AS _BYTE
    scope AS STRING * 50
    protected AS _BYTE
END TYPE

CONST varType_FLOAT = 0
CONST varTypeSTRING = 1
CONST varTypeINTEGER = 2
CONST varTypeSINGLE = 3
CONST varTypeDOUBLE = 4
CONST varType_UBYTE = 5
CONST varType_BYTE = 6
CONST varType_UINTEGER = 7
CONST varType_UINTEGER64 = 8
CONST varType_INTEGER64 = 9
CONST varType_ULONG = 10
CONST varTypeLONG = 11


REDIM SHARED vars(0) AS vartype
REDIM SHARED strings(0) AS STRING
REDIM SHARED nums(0) AS _FLOAT
REDIM SHARED program(0) AS STRING
DIM SHARED totalVars AS _UNSIGNED LONG, varType_DEFAULT AS _BYTE
DIM SHARED thisScope$, currentLine AS _UNSIGNED LONG, lineThatErrored AS _UNSIGNED LONG
DIM varIndex AS _UNSIGNED LONG, i AS _UNSIGNED LONG
DIM doLine AS _UNSIGNED LONG, loopLine AS _UNSIGNED LONG
DIM ifLine AS _UNSIGNED LONG
DIM SHARED errorHappened AS _BYTE
DIM SHARED k AS LONG
DIM externalLimit AS INTEGER
DIM temp$, L$, L1$, saveFile$, q AS STRING * 1, k$
DIM MyBad AS _BYTE, Comma1 AS INTEGER, Comma2 AS INTEGER, Comma3 AS INTEGER
DIM SHARED running AS _BYTE, loaded AS _BYTE, loadedFile$
DIM SHARED CurrentSCREEN%

thisScope$ = "MAIN MODULE"
varType_DEFAULT = varTypeSINGLE

IF _FILEEXISTS(COMMAND$) THEN
    loaded = load(COMMAND$)
    IF loaded THEN PRINT "Loaded - "; COMMAND$
ELSEIF LEN(COMMAND$) > 0 THEN
    IF LCASE$(RIGHT$(COMMAND$, 4)) <> ".bas" THEN
        IF _FILEEXISTS(COMMAND$ + ".bas") THEN
            loaded = load(COMMAND$ + ".bas")
            IF loaded THEN PRINT "Loaded - "; COMMAND$ + ".bas"
        ELSE
            PRINT "File not found - "; COMMAND$ + ".bas"
        END IF
    ELSE
        PRINT "File not found - "; COMMAND$
    END IF
END IF

'internal variables (functions)
RESTORE QB64Functions
DIM funcName$
DO
    READ funcName$
    IF funcName$ = "*end*" THEN EXIT DO
    varIndex = addVar(funcName$)
    vars(varIndex).protected = true
LOOP

QB64Functions:
DATA val,int,asc,cos,sin,len,rnd,timer,time$,date$
DATA chr$,inkey$,_width,_height,_mousex,_mousey,_mousebutton
DATA str$,asc,_resize,_resizewidth,_resizeheight,_scaledwidth
DATA _scaledheight,_screenhide,_console,_blink,_fileexists
DATA _direxists,_devices,_device$,_deviceinput,_lastbutton
DATA _lastaxis,_lastwheel,_button,_buttonchange,_axis,_wheel
DATA _screenx,_screeny,_os$,_title$,_mapunicode,_keydown
DATA _keyhit,_windowhandle,_screenimage,_freetimer,_fullscreen
DATA _smooth,_windowhasfocus,_clipboard$,_clipboardimage
DATA _exit,_openhost,_connected,_connectionaddress,_connectionaddress$
DATA _openconnection,_openclient,environ$,_errorline,_inclerrorline
DATA _acceptfiledrop,_totaldroppedfiles,_droppedfile,_droppedfile$
DATA _newimage,_loadimage,_copyimage,_source,_dest,_display
DATA _pixelsize,_clearcolor,_blend,_defaultcolor,_backgroundcolor
DATA _palettecolor,_loadfont,_fontwidth,_fontheight,_font
DATA _printwidth,_printmode,_rgba,_rgba32,_rgb,_rgb32
DATA _red,_red32,_green,_green32,_blue,_blue32
DATA _alpha,_alpha32,_mouseinput,_mousewheel,freefile
DATA shell,_shellhide,command$,_commandcount,_sndrate
DATA _sndopenraw,_sndrawlen,_sndlen,_sndpaused,_sndopen
DATA _sndgetpos,_sndplaying,_sndcopy,seek,loc,eof
DATA lof,screen,point,tab,spc,inp,pos,sgn,lbound,ubound
DATA oct$,hex$,exp,fix,cdbl,csng,_round,cint,clng
DATA csrlin,mki$,mkl$,mks$,mkd$,mksmbf$,mkdmbf$
DATA _mk$,cvsmbf,cvdmbf,cvi,cvl,cvs,cvd,_cv
DATA string$,space$,instr,_instrrev,mid$,sqr
DATA tan,atn,log,abs,erl,err,ucase$,lcase$,left$
DATA right$,ltrim$,rtrim$,_trim$,_cwd$,_startdir$
DATA _dir$,_inclerrorfile$,_atan2,_hypot,_pi,_desktopheight
DATA _desktopwidth,_screenexists,_controlchr,_stricmp
DATA _strcmp,_autodisplay,_shr,_shl,_deflate$,_inflate$
DATA _readbit,_setbit,_resetbit,_togglebit
DATA *end*

SCREEN CurrentSCREEN%
DO
    k = _KEYHIT

    IF (k = ASC("C") OR k = ASC("c")) AND (_KEYDOWN(100305) OR _KEYDOWN(100306)) THEN
        PRINT "Break."
        IF running THEN running = false
        _KEYCLEAR
    END IF

    IF running THEN
        currentLine = currentLine + 1
        IF currentLine > UBOUND(program) THEN
            running = false
        ELSE
            L1$ = program(currentLine)
        END IF
    END IF

    IF NOT running THEN _AUTODISPLAY: LINE INPUT "."; L1$

    L1$ = LTRIM$(RTRIM$(L1$))
    L$ = UCASE$(L1$)
    IF isNumber(LEFT$(L$, INSTR(L$, CHR$(32)) - 1)) THEN
        IF NOT running THEN
            IF loaded THEN
                i = VAL(LEFT$(L1$, INSTR(L1$, CHR$(32)) - 1))
                L1$ = MID$(L1$, INSTR(L1$, CHR$(32)) + 1)
                IF i > UBOUND(program) THEN
                    REDIM _PRESERVE program(i) AS STRING
                END IF
                program(i) = L1$
            ELSE
                PRINT "No program loaded. Use NEW or LOAD <file>."
            END IF
        END IF
    ELSEIF LEFT$(L$, 5) = "LOAD " THEN
        IF NOT running THEN
            temp$ = Parse$(MID$(L1$, 6))
            tryWithExtension:
            IF _FILEEXISTS(temp$) THEN
                loaded = load(temp$)
                IF loaded THEN PRINT "Loaded."
            ELSE
                IF RIGHT$(temp$, 4) = ".bas" THEN
                    PRINT "File not found - "; temp$
                ELSE
                    temp$ = temp$ + ".bas"
                    GOTO tryWithExtension
                END IF
            END IF
        END IF
    ELSEIF L$ = "RELOAD" THEN
        IF NOT running THEN
            IF loaded THEN
                L1$ = "LOAD " + loadedFile$
                GOTO tryWithExtension
            ELSE
                PRINT "No program loaded."
            END IF
        END IF
    ELSEIF LEFT$(L$, 7) = "INSERT " AND VAL(MID$(L$, 8)) > 0 THEN
        IF NOT running THEN
            IF loaded THEN
                IF VAL(MID$(L$, 8)) <= UBOUND(program) THEN
                    REDIM _PRESERVE program(UBOUND(program) + 1) AS STRING
                    FOR i = UBOUND(program) - 1 TO VAL(MID$(L$, 8)) STEP -1
                        program(i + 1) = program(i)
                    NEXT
                    program(VAL(MID$(L$, 8))) = ""
                    L$ = "EDIT " + MID$(L$, 8)
                    GOTO Edit
                ELSE
                    PRINT "Invalid line number -"; VAL(MID$(L$, 8))
                END IF
            ELSE
                PRINT "No program loaded."
            END IF
        END IF
    ELSEIF LEFT$(L$, 6) = "CHDIR " THEN
        CHDIR Parse$(MID$(L1$, 7))
    ELSEIF LEFT$(L$, 6) = "MKDIR " THEN
        MKDIR Parse$(MID$(L1$, 7))
    ELSEIF LEFT$(L$, 5) = "KILL " THEN
        KILL Parse$(MID$(L1$, 6))
    ELSEIF L$ = "DEBUG ON" THEN
        _CONSOLE ON
        debugging = true
        PRINT "Debugging enabled."
    ELSEIF L$ = "DEBUG OFF" THEN
        _CONSOLE OFF
        debugging = false
        PRINT "Debugging disabled."
    ELSEIF LEFT$(L$, 7) = "DELETE " AND VAL(MID$(L$, 8)) > 0 THEN
        IF NOT running THEN
            IF loaded THEN
                IF INSTR(MID$(L$, 8), "-") = 0 THEN
                    IF VAL(MID$(L$, 8)) <= UBOUND(program) THEN
                        FOR i = VAL(MID$(L$, 8)) + 1 TO UBOUND(program)
                            program(i - 1) = program(i)
                        NEXT
                        REDIM _PRESERVE program(UBOUND(program) - 1) AS STRING
                    ELSE
                        PRINT "Invalid line number -"; VAL(MID$(L$, 8))
                    END IF
                ELSE
                    'interval
                    DIM lower AS _UNSIGNED LONG, upper AS _UNSIGNED LONG
                    DIM interval$
                    interval$ = MID$(L$, 8)
                    lower = VAL(LEFT$(interval$, INSTR(interval$, "-") - 1))
                    upper = VAL(MID$(interval$, INSTR(interval$, "-") + 1))
                    IF lower > upper THEN SWAP lower, upper

                    IF lower < 1 OR upper > UBOUND(program) THEN
                        PRINT "Invalid interval ("; LTRIM$(RTRIM$(interval$)); ")"
                    ELSE
                        FOR i = upper + 1 TO UBOUND(program)
                            program(lower + (i - (upper + 1))) = program(i)
                        NEXT
                        REDIM _PRESERVE program(UBOUND(program) - (upper - lower) - 1) AS STRING
                    END IF
                END IF
            ELSE
                PRINT "No program loaded."
            END IF
        END IF
    ELSEIF L$ = "SAVE" THEN
        IF NOT running THEN
            IF loaded THEN
                IF loadedFile$ = "" THEN
                    PRINT "Missing: file name."
                ELSE
                    DIM ff AS INTEGER
                    ff = FREEFILE
                    OPEN loadedFile$ FOR OUTPUT AS ff
                    FOR i = 1 TO UBOUND(program)
                        PRINT #ff, program(i)
                    NEXT
                    CLOSE ff
                    PRINT "Saved - "; loadedFile$
                END IF
            ELSE
                PRINT "No program loaded."
            END IF
        END IF
    ELSEIF LEFT$(L$, 5) = "SAVE " THEN
        IF NOT running THEN
            saveFile$ = UCASE$(LTRIM$(RTRIM$(MID$(L$, 6))))
            IF loaded THEN
                IF saveFile$ <> loadedFile$ THEN
                    IF _FILEEXISTS(saveFile$) THEN
                        INPUT "Overwrite (y/N)?", q$1
                        IF LCASE$(q$1) = "y" THEN
                            GOTO overWrite
                        END IF
                    ELSE
                        GOTO overWrite
                    END IF
                ELSE
                    overWrite:
                    ff = FREEFILE
                    OPEN saveFile$ FOR OUTPUT AS ff
                    FOR i = 1 TO UBOUND(program)
                        PRINT #ff, program(i)
                    NEXT
                    CLOSE ff
                    loadedFile$ = saveFile$
                    PRINT "Saved - "; loadedFile$
                END IF
            ELSE
                PRINT "No program loaded."
            END IF
        END IF
    ELSEIF L$ = "CLEAR" THEN
        DIM j AS _UNSIGNED LONG
        j = 0
        FOR i = totalVars TO 1 STEP -1
            IF NOT vars(i).protected THEN
                totalVars = totalVars - 1
                vars(i).name = ""
                vars(i).type = 0
                strings(i) = ""
                nums(i) = 0
            END IF
        NEXT
    ELSEIF L$ = "NEW" OR LEFT$(L$, 4) = "NEW " THEN
        IF NOT running THEN
            IF LEN(L$) > 3 THEN loadedFile$ = MID$(L$, 5) ELSE loadedFile$ = ""
            loaded = true
            REDIM SHARED program(1) AS STRING
        END IF
    ELSEIF L$ = "_DISPLAY" THEN
        _DISPLAY
    ELSEIF LEFT$(L$, 5) = "EDIT " AND VAL(MID$(L$, 6)) > 0 THEN
        IF NOT running THEN
            Edit:
            IF loaded THEN
                IF VAL(MID$(L$, 6)) = UBOUND(program) + 1 THEN
                    REDIM _PRESERVE program(UBOUND(program) + 1) AS STRING
                    'EDIT can be used to increase program size without the need for INSERT
                END IF

                IF VAL(MID$(L$, 6)) <= UBOUND(program) THEN
                    DIM row AS INTEGER, col AS INTEGER
                    row = CSRLIN: col = POS(1)
                    PRINT LEFT$(program(VAL(MID$(L$, 6))), 80);
                    DO
                        LOCATE row, col, 1
                        k$ = "": WHILE k$ = "": k$ = INKEY$: _LIMIT 30: WEND
                        SELECT CASE k$
                            CASE CHR$(13)
                                DIM p$
                                p$ = ""
                                FOR i = 1 TO 80
                                    p$ = p$ + CHR$(SCREEN(row, i))
                                NEXT
                                program(VAL(MID$(L$, 6))) = RTRIM$(p$)
                                EXIT DO
                            CASE CHR$(27)
                                EXIT DO
                            CASE CHR$(8)
                                IF col > 1 THEN
                                    FOR i = col TO 80
                                        LOCATE row, i - 1
                                        PRINT CHR$(SCREEN(row, i));
                                    NEXT
                                    col = col - 1
                                END IF
                            CASE CHR$(0) + CHR$(83)
                                FOR i = col TO 79
                                    LOCATE row, i
                                    PRINT CHR$(SCREEN(row, i + 1));
                                NEXT
                            CASE CHR$(0) + CHR$(75)
                                IF col > 1 THEN col = col - 1
                            CASE CHR$(0) + CHR$(77)
                                IF col < 80 THEN col = col + 1
                            CASE ELSE
                                PRINT k$;
                                IF col < 80 THEN col = col + 1
                        END SELECT
                    LOOP
                    PRINT
                    LOCATE , , 0
                ELSE
                    PRINT "Invalid line number -"; VAL(MID$(L$, 6))
                END IF
            ELSE
                PRINT "No program loaded."
            END IF
        END IF
    ELSEIF L$ = "LIST VARIABLES" THEN
        IF NOT running THEN
            j = 0
            db_echo "Listing variables"
            db_echo "Total vars:" + STR$(totalVars)
            FOR i = 1 TO totalVars
                IF NOT vars(i).protected THEN
                    j = j + 1
                    PRINT RTRIM$(vars(i).name); " = ";
                    IF vars(i).type = varTypeSTRING THEN
                        PRINT CHR$(34); strings(i); CHR$(34)
                    ELSE
                        PRINT nums(i)
                    END IF

                    IF j MOD 20 = 0 AND i < totalVars THEN
                        PRINT "-- hit a key --"
                        k$ = "": WHILE k$ = "": k$ = INKEY$: _LIMIT 30: WEND
                        IF k$ = CHR$(3) THEN
                            PRINT "Break."
                            GOTO ListEnd
                        END IF
                        _KEYCLEAR
                    END IF
                END IF
            NEXT
        END IF
    ELSEIF L$ = "LIST" OR L$ = "LIST PAUSE" THEN
        IF NOT running THEN
            IF loaded THEN
                DIM maxSpaceBefore AS INTEGER, prevFG AS _UNSIGNED LONG, prevBG AS _UNSIGNED LONG
                DIM thisLineNum$, screenMaxCols AS INTEGER

                prevFG = _DEFAULTCOLOR
                prevBG = _BACKGROUNDCOLOR

                IF _PIXELSIZE = 0 THEN
                    screenMaxCols = _WIDTH
                ELSE
                    screenMaxCols = _WIDTH / _PRINTWIDTH("W")
                END IF

                maxSpaceBefore = LEN(STR$(UBOUND(program))) + 1

                MyBad = true
                COLOR , 0
                MyBad = false

                FOR i = 1 TO UBOUND(program)
                    thisLineNum$ = STR$(i)
                    thisLineNum$ = SPACE$(maxSpaceBefore - LEN(thisLineNum$) - 1) + thisLineNum$ + " "
                    MyBad = true
                    COLOR 8
                    MyBad = false
                    PRINT thisLineNum$;

                    MyBad = true
                    COLOR 7
                    MyBad = false
                    PRINT program(i);

                    IF POS(1) < screenMaxCols THEN
                        PRINT SPACE$((screenMaxCols + 1) - POS(1));
                    END IF

                    IF i MOD 20 = 0 AND L$ = "LIST PAUSE" THEN
                        MyBad = true
                        COLOR 8
                        MyBad = false

                        PRINT "-- hit a key --"
                        k$ = "": WHILE k$ = "": k$ = INKEY$: _LIMIT 30: WEND
                        IF k$ = CHR$(3) THEN
                            COLOR prevFG, prevBG
                            PRINT "Break."
                            GOTO ListEnd
                        END IF
                        _KEYCLEAR
                    END IF
                NEXT
                COLOR prevFG, prevBG
                PRINT "End of file - "; loadedFile$
                ListEnd:
            ELSE
                PRINT "No program loaded."
            END IF
        END IF
    ELSEIF LEFT$(L$, 5) = "LIST " AND VAL(MID$(L$, 6)) > 0 THEN
        IF NOT running THEN
            IF loaded THEN
                IF VAL(MID$(L$, 6)) <= UBOUND(program) THEN
                    PRINT program(VAL(MID$(L$, 6)))
                ELSE
                    PRINT "Invalid line number -"; VAL(MID$(L$, 6))
                END IF
            ELSE
                PRINT "No program loaded."
            END IF
        END IF
    ELSEIF LEFT$(L$, 6) = "WIDTH " THEN
        DIM p1$, p2$
        p$ = MID$(L$, 7)
        IF INSTR(p$, ",") THEN
            p1$ = LEFT$(p$, INSTR(p$, ",") - 1)
            p2$ = MID$(p$, INSTR(p$, ",") + 1)
            IF LEN(p1$) = 0 THEN
                WIDTH , VAL(Parse$(p2$))
            ELSE
                WIDTH VAL(Parse$(p1$)), VAL(Parse$(p2$))
            END IF
        ELSE
            WIDTH VAL(Parse$(p$))
        END IF
    ELSEIF LEFT$(L$, 10) = "RANDOMIZE " THEN
        RANDOMIZE VAL(Parse$(MID$(L$, 11)))
    ELSEIF LEFT$(L$, 7) = "_LIMIT " THEN
        externalLimit = VAL(Parse$(MID$(L$, 8)))
    ELSEIF LEFT$(L$, 1) = "'" OR LEFT$(L$, 4) = "REM " OR L$ = "'" OR L$ = "REM" OR L$ = "" THEN
        'it's a comment.
    ELSEIF LEFT$(L$, 4) = "KEY " THEN
        IF _TRIM$(MID$(L$, 5)) = "ON" THEN
            KEY ON
        ELSEIF _TRIM$(MID$(L$, 5)) = "OFF" THEN
            KEY OFF
        ELSE
            GOTO syntaxerror
        END IF
    ELSEIF L$ = "RUN" THEN
        IF NOT running THEN
            IF loaded THEN
                currentLine = 0
                externalLimit = 0
                running = true
            ELSE
                PRINT "No program loaded."
            END IF
        END IF
    ELSEIF LEFT$(L$, 6) = "COLOR " THEN
        DIM c$, c1$, c2$
        c$ = MID$(L$, 7)
        IF INSTR(c$, ",") THEN
            c1$ = LEFT$(c$, INSTR(c$, ",") - 1)
            c2$ = MID$(c$, INSTR(c$, ",") + 1)
            IF LEN(c1$) > 0 AND LEN(c2$) > 0 THEN
                COLOR VAL(Parse(c1$)), VAL(Parse(c2$))
            ELSEIF LEN(c1$) > 0 AND LEN(c2$) = 0 THEN
                COLOR VAL(Parse(c1$))
            ELSEIF LEN(c1$) = 0 AND LEN(c2$) > 0 THEN
                COLOR , VAL(Parse(c2$))
            END IF
        ELSE
            COLOR VAL(Parse(c$))
        END IF
    ELSEIF LEFT$(L$, 7) = "_BLINK " THEN
        IF _TRIM$(MID$(L$, 8)) = "ON" THEN
            _BLINK ON
        ELSEIF _TRIM$(MID$(L$, 8)) = "OFF" THEN
            _BLINK OFF
        ELSE
            GOTO syntaxerror
        END IF
    ELSEIF LEFT$(L$, 7) = "LOCATE " THEN
        c$ = MID$(L$, 8)
        IF INSTR(c$, ",") THEN
            c1$ = LEFT$(c$, INSTR(c$, ",") - 1)
            c2$ = MID$(c$, INSTR(c$, ",") + 1)
            IF LEN(c1$) > 0 AND LEN(c2$) > 0 THEN
                LOCATE VAL(Parse$(c1$)), VAL(Parse$(c2$))
            ELSEIF LEN(c1$) > 0 AND LEN(c2$) = 0 THEN
                LOCATE VAL(Parse$(c1$))
            ELSEIF LEN(c1$) = 0 AND LEN(c2$) > 0 THEN
                LOCATE , VAL(Parse$(c2$))
            END IF
        ELSE
            LOCATE VAL(Parse$(c$))
        END IF
    ELSEIF LEFT$(L$, 6) = "FILES " THEN
        temp$ = Parse$(MID$(L1$, 7))
        FILES temp$
    ELSEIF L$ = "FILES" THEN
        FILES
    ELSEIF LEFT$(L$, 8) = "CIRCLE (" THEN
        IF CurrentSCREEN% = 0 THEN PRINT "Invalid mode.": running = false: GOTO Parse.Done
        Comma1% = INSTR(L$, ","): Comma2% = INSTR(Comma1% + 1, L$, ","): Comma3% = INSTR(Comma2% + 1, L$, ",")

        DIM XPos1%, YPos1%, X1%, Y1%, Rad%, DrawClr%, EPos%, Elipse!, Arc%
        DIM Comma4%, Comma5%, ArcBeg!, ArcEnd!, d##
        XPos1% = INSTR(L$, " (") + 2
        YPos1% = Comma1% + 1
        X1% = VAL(Parse(MID$(L$, XPos1%, Comma1% - XPos1%)))
        Y1% = VAL(Parse(MID$(L$, YPos1%, Comma2% - YPos1% - 1)))

        Rad% = VAL(Parse(MID$(L$, Comma2% + 1, Comma3% - Comma2% - 1)))

        c$ = LTRIM$(RTRIM$(LEFT$(MID$(L$, Comma3% + 1), 3))) 'Color attribute (variable or constant)
        IF RIGHT$(c$, 1) = "," THEN c$ = LEFT$(c$, LEN(c$) - 1) 'If single-digit attribute

        IF INSTR("0123456789", LEFT$(c$, 1)) > 0 THEN DrawClr% = VAL(c$) ELSE DrawClr% = VAL(Parse(c$))

        EPos% = INSTR(L$, ", , , ")

        IF EPos% > 0 THEN
            EPos% = EPos% + 6: Elipse = VAL(Parse$(MID$(L$, EPos%)))
        ELSE
            Arc% = INSTR(Comma3% + 1, L$, ",")

            IF Arc% > 0 THEN
                Comma4% = Arc%
                Comma5% = INSTR(Comma4% + 1, L$, ",")

                ArcBeg = VAL(Parse$(MID$(L$, Comma4% + 1, Comma5% - Comma4% - 1))) ': PRINT "ArcBeg:"; ArcBeg;   '* * * * Test PRINT
                ArcEnd = VAL(Parse$(MID$(L$, Comma5% + 1))) ': PRINT " ArcEnd:"; ArcEnd;

                IF INSTR(Comma5% + 1, L$, ",") > 0 THEN EPos% = INSTR(Comma5% + 1, L$, ",") + 1: Elipse = VAL(Parse$(MID$(L$, EPos%)))
            END IF
        END IF

        IF Arc% > 0 AND Elipse = 0 THEN CIRCLE (X1%, Y1%), Rad%, DrawClr%, ArcBeg, ArcEnd: GOTO Circ.Done
        IF Elipse > 0 AND Arc% = 0 THEN CIRCLE (X1%, Y1%), Rad%, DrawClr%, , , Elipse: GOTO Circ.Done
        IF Arc% > 0 AND Elipse > 0 THEN CIRCLE (X1%, Y1%), Rad%, DrawClr%, ArcBeg, ArcEnd, Elipse: GOTO Circ.Done
        CIRCLE (X1%, Y1%), Rad%, DrawClr% 'No arc, no elipse

        Circ.Done: Rad% = 0: Arc% = 0: Elipse = 0: c$ = "": DrawClr% = 0: GOTO Parse.Done
    ELSEIF L$ = "SLEEP" THEN
        SLEEP
    ELSEIF LEFT$(L$, 6) = "SLEEP " THEN
        SLEEP VAL(Parse$(MID$(L$, 7)))
    ELSEIF L$ = "PRINT" OR L$ = "?" THEN
        PRINT
    ELSEIF LEFT$(L$, 6) = "PRINT " OR LEFT$(L$, 1) = "?" THEN
        IF LEFT$(L$, 2) = "? " THEN L1$ = "PRINT " + MID$(L1$, 3): L$ = L1$
        IF LEFT$(L$, 1) = "?" THEN L1$ = "PRINT " + MID$(L1$, 2): L$ = L1$

        DIM retainCursor AS _BYTE
        IF RIGHT$(L$, 1) = ";" THEN
            retainCursor = true
            L$ = LEFT$(L$, LEN(L$) - 1)
            L1$ = LEFT$(L1$, LEN(L$))
        ELSE
            retainCursor = false
        END IF

        PRINT Parse(MID$(L1$, 7));
        IF NOT retainCursor THEN PRINT
    ELSEIF LEFT$(L$, 7) = "_TITLE " THEN
        temp$ = Parse$(MID$(L1$, 8))
        _TITLE temp$
    ELSEIF L$ = "CLS" THEN
        CLS
        IF debugging THEN
            i = _DEST
            _DEST _CONSOLE
            CLS
            _DEST i
        END IF
    ELSEIF L$ = "SYSTEM" OR L$ = "EXIT" THEN
        SYSTEM
    ELSEIF L$ = "END" THEN
        IF running THEN running = false
    ELSEIF LEFT$(L$, 6) = "INPUT " THEN
        DIM varName$, d$, d%
        varName$ = MID$(L1$, 7)
        varIndex = addVar(varName$)
        IF vars(varIndex).type = varTypeSTRING THEN
            INPUT "", d$
            strings(varIndex) = d$
        ELSE
            SELECT CASE vars(varIndex).type
                CASE varTypeINTEGER
                    INPUT "", d%
                    nums(varIndex) = d%
                CASE ELSE
                    INPUT "", d##
                    nums(varIndex) = d##
            END SELECT
        END IF
    ELSEIF L$ = "DO" THEN
        IF running THEN doLine = currentLine
    ELSEIF L$ = "LOOP" THEN
        IF running THEN
            loopLine = currentLine
            IF doLine > 0 THEN currentLine = doLine
        END IF
    ELSEIF L$ = "EXIT DO" THEN
        IF running THEN
            IF loopLine > 0 THEN
                currentLine = loopLine
            ELSE
                DO
                    currentLine = currentLine + 1
                    IF currentLine > UBOUND(program) THEN PRINT "DO without LOOP on line"; doLine: running = false: GOTO Parse.Done
                    L1$ = program(currentLine)
                    L1$ = LTRIM$(RTRIM$(L1$))
                    L$ = UCASE$(L1$)
                    IF L$ = "LOOP" THEN doLine = 0: EXIT DO
                LOOP
            END IF
        END IF
    ELSEIF LEFT$(L$, 7) = "SCREEN " THEN
        SCREEN VAL(Parse$(MID$(L$, 8)))
    ELSEIF L$ = "END IF" THEN
        IF running THEN
            IF ifLine = 0 THEN
                PRINT "END IF without IF - on line"; currentLine
                running = false
            END IF
        ELSE
            PRINT "Not valid in immediate mode."
        END IF
    ELSEIF LEFT$(L$, 3) = "IF " THEN
        IF NOT running THEN
            PRINT "Not valid in immediate mode."
        ELSE
            IF RIGHT$(L$, 5) <> " THEN" THEN GOTO syntaxerror
            DIM i$, i1$, i2$, s$, r AS _BYTE
            DIM i1##, i2##, i1isText AS _BYTE, i2isText AS _BYTE

            ifLine = currentLine

            i$ = MID$(L$, 4, LEN(L$) - 8)

            IF INSTR(i$, "=") THEN
                s$ = "="
            ELSEIF INSTR(i$, ">") THEN
                s$ = ">"
            ELSEIF INSTR(i$, "<") THEN
                s$ = "<"
            ELSE
                s$ = ""
            END IF

            i1$ = LEFT$(i$, INSTR(i$, s$) - 1)
            i2$ = MID$(i$, INSTR(i$, s$) + 1)

            i1$ = Parse$(i1$)
            i1## = VAL(i1$)
            IF isNumber(i1$) THEN i1isText = false ELSE i1isText = true
            i2$ = Parse$(i2$)
            i2## = VAL(i2$)
            IF isNumber(i2$) THEN i2isText = false ELSE i2isText = true

            IF i1isText <> i2isText THEN throwError 13: GOTO Parse.Done

            r = false
            SELECT CASE s$
                CASE "="
                    IF i1isText THEN
                        IF Parse$(i1$) = Parse$(i2$) THEN r = true
                    ELSE
                        IF i1## = i2## THEN r = true
                    END IF
                CASE ">"
                    IF i1isText THEN
                        IF Parse$(i1$) > Parse$(i2$) THEN r = true
                    ELSE
                        IF i1## > i2## THEN r = true
                    END IF
                CASE "<"
                    IF i1isText THEN
                        IF Parse$(i1$) < Parse$(i2$) THEN r = true
                    ELSE
                        IF i1## < i2## THEN r = true
                    END IF
            END SELECT

            IF r = false THEN
                DO
                    currentLine = currentLine + 1
                    IF currentLine > UBOUND(program) THEN PRINT "IF without END IF on line"; ifLine: running = false: GOTO Parse.Done
                    L1$ = program(currentLine)
                    L1$ = LTRIM$(RTRIM$(L1$))
                    L$ = UCASE$(L1$)
                    IF L$ = "END IF" THEN ifLine = 0: EXIT DO
                LOOP
            END IF
        END IF
    ELSEIF INSTR(L$, "=") > 0 THEN
        'Assignment
        varName$ = RTRIM$(LEFT$(L1$, INSTR(L1$, "=") - 1))
        varIndex = addVar(varName$) 'either add or acquire existing index

        IF vars(varIndex).protected THEN
            PRINT "Variable is protected";
            IF running THEN
                running = false
                PRINT " on line "; currentLine
            ELSE
                PRINT
            END IF

            GOTO Parse.Done
        END IF

        DIM v$, t$
        IF vars(varIndex).type = varTypeSTRING THEN
            v$ = RTRIM$(LTRIM$(MID$(L1$, INSTR(L1$, "=") + 1)))
            strings(varIndex) = Parse(v$)
        ELSE
            v$ = MID$(L1$, INSTR(L1$, "=") + 1)

            t$ = Parse(v$)

            IF vars(varIndex).type = varTypeINTEGER THEN
                nums(varIndex) = INT(VAL(t$))
            ELSE
                nums(varIndex) = VAL(t$)
            END IF
        END IF
    ELSE
        syntaxerror:
        IF LEN(L$) THEN PRINT "Syntax error";
        IF running THEN
            PRINT " on line"; currentLine
            running = false
        ELSE
            PRINT
        END IF
    END IF

    Parse.Done:

    IF externalLimit > 0 AND running THEN _LIMIT externalLimit
LOOP

'oops:
'IF MyBad THEN RESUME NEXT
'RESUME Parse.Done

FUNCTION addVar~& (varName$)
    DIM found AS _UNSIGNED LONG

    'check if var exists
    found = searchVar(varName$)

    IF found THEN addVar~& = found: EXIT FUNCTION

    totalVars = totalVars + 1
    IF totalVars > UBOUND(vars) THEN
        REDIM _PRESERVE vars(totalVars + 99) AS vartype
        REDIM _PRESERVE strings(totalVars + 99) AS STRING
        REDIM _PRESERVE nums(totalVars + 99) AS _FLOAT
    END IF

    vars(totalVars).name = varName$

    'type detection -----------------------------------------------------------
    vars(totalVars).type = detectType(varName$)
    '--------------------------------------------------------------------------

    vars(totalVars).scope = thisScope$
    addVar~& = totalVars
END FUNCTION

FUNCTION detectType%% (__varname$)
    DIM varname$

    varname$ = LTRIM$(RTRIM$(__varname$))

    detectType%% = varType_DEFAULT

    IF RIGHT$(varname$, 1) = "$" THEN detectType%% = varTypeSTRING

    IF RIGHT$(varname$, 3) = "~%%" THEN
        detectType%% = varType_UBYTE
    ELSEIF RIGHT$(varname$, 2) = "%%" THEN
        detectType%% = varType_BYTE
    ELSEIF RIGHT$(varname$, 2) = "~%" THEN
        detectType%% = varType_UINTEGER
    ELSEIF RIGHT$(varname$, 1) = "%" THEN
        detectType%% = varTypeINTEGER
    END IF

    IF RIGHT$(varname$, 1) = "!" THEN
        detectType%% = varTypeSINGLE
    ELSEIF RIGHT$(varname$, 2) = "##" THEN
        detectType%% = varType_FLOAT
    ELSEIF RIGHT$(varname$, 1) = "#" THEN
        detectType%% = varTypeDOUBLE
    END IF

    IF RIGHT$(varname$, 3) = "~&&" THEN
        detectType%% = varType_UINTEGER64
    ELSEIF RIGHT$(varname$, 2) = "&&" THEN
        detectType%% = varType_INTEGER64
    ELSEIF RIGHT$(varname$, 2) = "~&" THEN
        detectType%% = varType_ULONG
    ELSEIF RIGHT$(varname$, 1) = "&" THEN
        detectType%% = varTypeLONG
    END IF
END FUNCTION

FUNCTION searchVar~& (__varName$)
    DIM i AS _UNSIGNED LONG, found AS _BYTE
    DIM varName$

    varName$ = __varName$

    'check if var exists
    FOR i = 1 TO totalVars
        IF LCASE$(LTRIM$(RTRIM$(vars(i).name))) = LCASE$(LTRIM$(RTRIM$(varName$))) THEN
            found = true
            EXIT FOR
        END IF
    NEXT

    IF found THEN searchVar~& = i
END FUNCTION

FUNCTION load%% (file$)
    DIM ff AS INTEGER, l$
    ff = FREEFILE
    OPEN file$ FOR BINARY AS ff

    currentLine = 0
    DO
        IF EOF(ff) THEN EXIT DO
        LINE INPUT #ff, l$
        currentLine = currentLine + 1
        IF currentLine > UBOUND(program) THEN REDIM _PRESERVE program(currentLine + 999) AS STRING
        program(currentLine) = l$
    LOOP

    CLOSE ff

    IF currentLine > 0 THEN
        REDIM _PRESERVE program(currentLine) AS STRING
        currentLine = 0
        load%% = true
        loadedFile$ = file$
    ELSE
        load%% = false
    END IF
END FUNCTION

FUNCTION removeQuote$ (__text$)
    DIM text$

    text$ = __text$

    IF LEFT$(text$, 1) = CHR$(34) THEN
        text$ = MID$(text$, 2)
    END IF

    IF RIGHT$(text$, 1) = CHR$(34) THEN
        text$ = LEFT$(text$, LEN(text$) - 1)
    END IF

    removeQuote$ = text$
END FUNCTION

FUNCTION GetVal## (__c$, foundAsText AS _BYTE, textReturn$)
    DIM c$, sp AS _UNSIGNED LONG
    DIM varIndex AS _UNSIGNED LONG
    DIM temp##, temp$

    db_echo "entering getval(): " + __c$

    c$ = LTRIM$(RTRIM$(__c$))
    foundAsText = false

    IF LEFT$(c$, 1) = CHR$(34) THEN
        db_echo "literal string"
        foundAsText = true
        textReturn$ = removeQuote$(c$)
        EXIT FUNCTION
    END IF

    sp = INSTR(c$, CHR$(32))
    IF sp THEN
        temp$ = MID$(c$, sp + 1)
        temp## = VAL(temp$)
        c$ = LEFT$(c$, sp - 1)
    END IF

    varIndex = searchVar(c$)
    IF varIndex THEN
        IF vars(varIndex).protected THEN
            db_echo "returning QB64 function"
            db_echo "temp## =" + STR$(temp##)
            db_echo "temp$  =" + CHR$(34) + temp$ + CHR$(34)
            SELECT CASE RTRIM$(vars(varIndex).name)
                CASE "cos"
                    GetVal## = COS(temp##)
                CASE "val"
                    GetVal## = VAL(temp$)
                CASE "int"
                    GetVal## = INT(temp##)
                CASE "asc"
                    GetVal## = ASC(temp$)
                CASE "sin"
                    GetVal## = SIN(temp##)
                CASE "len"
                    GetVal## = LEN(temp$)
                CASE "rnd"
                    GetVal## = RND
                CASE "timer"
                    GetVal## = TIMER
                CASE "time$"
                    foundAsText = true
                    textReturn$ = TIME$
                CASE "date$"
                    foundAsText = true
                    textReturn$ = DATE$
                CASE "chr$"
                    foundAsText = true
                    textReturn$ = CHR$(temp##)
                CASE "str$"
                    foundAsText = true
                    textReturn$ = STR$(temp##)
                CASE "inkey$"
                    foundAsText = true
                    textReturn$ = INKEY$
                CASE "_width"
                    GetVal## = _WIDTH
                CASE "_height"
                    GetVal## = _HEIGHT
                CASE "_mousex"
                    GetVal## = _MOUSEX
                CASE "_mousey"
                    GetVal## = _MOUSEY
                CASE "_mousebutton"
                    GetVal## = _MOUSEBUTTON(temp##)
                CASE "_resize"
                    GetVal## = _RESIZE
                CASE "_resizewidth"
                    GetVal## = _RESIZEWIDTH
                CASE "_resizeheight"
                    GetVal## = _RESIZEHEIGHT
                CASE "_scaledwidth"
                    GetVal## = _SCALEDWIDTH
                CASE "_scaledheight"
                    GetVal## = _SCALEDHEIGHT
                CASE "_screenhide"
                    GetVal## = _SCREENHIDE
                CASE "_console"
                    GetVal## = _CONSOLE
                CASE "_blink"
                    GetVal## = _BLINK
                CASE "_fileexists"
                    GetVal## = _FILEEXISTS(temp$)
                CASE "_direxists"
                    GetVal## = _DIREXISTS(temp$)
                CASE "_devices"
                    GetVal## = _DEVICES
                CASE "_device$"
                    foundAsText = true
                    textReturn$ = _DEVICE$
                CASE "_deviceinput"
                    GetVal## = _DEVICEINPUT
                CASE "_lastbutton"
                    GetVal## = _LASTBUTTON
                CASE "_lastaxis"
                    GetVal## = _LASTAXIS
                CASE "_lastwheel"
                    GetVal## = _LASTWHEEL
                CASE "_button"
                    GetVal## = _BUTTON
                CASE "_buttonchange"
                    GetVal## = _BUTTONCHANGE
                CASE "_axis"
                    GetVal## = _AXIS
                CASE "_wheel"
                    GetVal## = _WHEEL
                CASE "_screenx"
                    GetVal## = _SCREENX
                CASE "_screeny"
                    GetVal## = _SCREENY
                CASE "_os$"
                    foundAsText = true
                    textReturn$ = _OS$
                CASE "_title$"
                    foundAsText = true
                    textReturn$ = _TITLE$
                CASE "_mapunicode"
                    GetVal## = _MAPUNICODE(temp##)
                CASE "_keydown"
                    GetVal## = _KEYDOWN(temp##)
                CASE "_keyhit"
                    GetVal## = _KEYHIT
                CASE "_windowhandle"
                    GetVal## = _WINDOWHANDLE
                CASE "_screenimage"
                    GetVal## = _SCREENIMAGE
                CASE "_freetimer"
                    GetVal## = _FREETIMER
                CASE "_fullscreen"
                    GetVal## = _FULLSCREEN
                CASE "_smooth"
                    GetVal## = _SMOOTH
                CASE "_windowhasfocus"
                    GetVal## = _WINDOWHASFOCUS
                CASE "_clipboard$"
                    foundAsText = true
                    textReturn$ = _CLIPBOARD$
                CASE "_clipboardimage"
                    GetVal## = _CLIPBOARDIMAGE
                CASE "_exit"
                    GetVal## = _EXIT
                CASE "_openhost"
                    GetVal## = _OPENHOST(temp$)
                CASE "_connected"
                    GetVal## = _CONNECTED(temp##)
                CASE "_connectionaddress", "_connectionaddress$"
                    foundAsText = true
                    textReturn$ = _CONNECTIONADDRESS$(temp##)
                CASE "_openconnection"
                    GetVal## = _OPENCONNECTION(temp##)
                CASE "_openclient"
                    GetVal## = _OPENCLIENT(temp$)
                CASE "environ$"
                    foundAsText = true
                    textReturn$ = ENVIRON$(temp$)
                CASE "_errorline"
                    GetVal## = lineThatErrored
                CASE "_inclerrorline"
                    'GetVal## = _INCLERRORLINE
                CASE "_acceptfiledrop"
                    GetVal## = _ACCEPTFILEDROP
                CASE "_totaldroppedfiles"
                    GetVal## = _TOTALDROPPEDFILES
                CASE "_droppedfile", "_droppedfile$"
                    foundAsText = true
                    textReturn$ = _DROPPEDFILE$
                CASE "_newimage"
                    'GetVal## = _newimage
                CASE "_loadimage"
                    GetVal## = _LOADIMAGE(temp$)
                CASE "_copyimage"
                    GetVal## = _COPYIMAGE(temp##)
                CASE "_source"
                    GetVal## = _SOURCE
                CASE "_dest"
                    GetVal## = _DEST
                CASE "_display"
                    GetVal## = _DISPLAY
                CASE "_pixelsize"
                    GetVal## = _PIXELSIZE
                CASE "_clearcolor"
                    GetVal## = _CLEARCOLOR
                CASE "_blend"
                    GetVal## = _BLEND
                CASE "_defaultcolor"
                    GetVal## = _DEFAULTCOLOR
                CASE "_backgroundcolor"
                    GetVal## = _BACKGROUNDCOLOR
                CASE "_palettecolor"
                    GetVal## = _PALETTECOLOR(temp##)
                CASE "_loadfont"
                    'GetVal## = _loadfont
                CASE "_fontwidth"
                    GetVal## = _FONTWIDTH
                CASE "_fontheight"
                    GetVal## = _FONTHEIGHT
                CASE "_font"
                    GetVal## = _FONT
                CASE "_printwidth"
                    GetVal## = _PRINTWIDTH(temp$)
                CASE "_printmode"
                    GetVal## = _PRINTMODE
                CASE "_rgba"
                    'GetVal## = _rgba
                CASE "_rgba32"
                    'GetVal## = _rgba32
                CASE "_rgb"
                    'GetVal## = _rgb
                CASE "_rgb32"
                    'GetVal## = _rgb32
                CASE "_red"
                    'GetVal## = _red
                CASE "_red32"
                    'GetVal## = _red32
                CASE "_green"
                    'GetVal## = _green
                CASE "_green32"
                    'GetVal## = _green32
                CASE "_blue"
                    'GetVal## = _blue
                CASE "_blue32"
                    'GetVal## = _blue32
                CASE "_alpha"
                    'GetVal## = _alpha
                CASE "_alpha32"
                    'GetVal## = _alpha32
                CASE "_mouseinput"
                    GetVal## = _MOUSEINPUT
                CASE "_mousewheel"
                    GetVal## = _MOUSEWHEEL
                CASE "freefile"
                    GetVal## = FREEFILE
                CASE "shell"
                    GetVal## = SHELL(temp$)
                CASE "_shellhide"
                    GetVal## = _SHELLHIDE(temp$)
                CASE "command$"
                    foundAsText = true
                    textReturn$ = COMMAND$(temp##)
                CASE "_commandcount"
                    GetVal## = _COMMANDCOUNT
                CASE "_sndrate"
                    GetVal## = _SNDRATE
                CASE "_sndopenraw"
                    GetVal## = _SNDOPENRAW
                CASE "_sndrawlen"
                    GetVal## = _SNDRAWLEN
                CASE "_sndlen"
                    GetVal## = _SNDLEN(temp##)
                CASE "_sndpaused"
                    GetVal## = _SNDPAUSED(temp##)
                CASE "_sndopen"
                    GetVal## = _SNDOPEN(temp$)
                CASE "_sndgetpos"
                    GetVal## = _SNDGETPOS(temp##)
                CASE "_sndplaying"
                    GetVal## = _SNDPLAYING(temp##)
                CASE "_sndcopy"
                    GetVal## = _SNDCOPY(temp##)
                CASE "seek"
                    GetVal## = SEEK(temp##)
                CASE "loc"
                    GetVal## = LOC(temp##)
                CASE "eof"
                    GetVal## = EOF(temp##)
                CASE "lof"
                    GetVal## = LOF(temp##)
                CASE "screen"
                    'GetVal## = screen
                CASE "point"
                    'GetVal## = point
                CASE "tab"
                    foundAsText = true
                    textReturn$ = TAB(temp##)
                CASE "spc"
                    foundAsText = true
                    textReturn$ = SPC(temp##)
                CASE "inp"
                    GetVal## = INP(temp##)
                CASE "pos"
                    GetVal## = POS(temp##)
                CASE "sgn"
                    GetVal## = SGN(temp##)
                CASE "lbound"
                    'GetVal## = lbound
                CASE "ubound"
                    'GetVal## = ubound
                CASE "oct$"
                    foundAsText = true
                    textReturn$ = OCT$(temp##)
                CASE "hex$"
                    foundAsText = true
                    textReturn$ = HEX$(temp##)
                CASE "exp"
                    GetVal## = EXP(temp##)
                CASE "fix"
                    GetVal## = FIX(temp##)
                CASE "cdbl"
                    GetVal## = CDBL(temp##)
                CASE "csng"
                    GetVal## = CSNG(temp##)
                CASE "_round"
                    GetVal## = _ROUND(temp##)
                CASE "cint"
                    GetVal## = CINT(temp##)
                CASE "clng"
                    GetVal## = CLNG(temp##)
                CASE "csrlin"
                    GetVal## = CSRLIN
                CASE "mki$"
                    foundAsText = true
                    textReturn$ = MKI$(temp##)
                CASE "mkl$"
                    foundAsText = true
                    textReturn$ = MKL$(temp##)
                CASE "mks$"
                    foundAsText = true
                    textReturn$ = MKS$(temp##)
                CASE "mkd$"
                    foundAsText = true
                    textReturn$ = MKD$(temp##)
                CASE "mksmbf$"
                    foundAsText = true
                    textReturn$ = MKSMBF$(temp##)
                CASE "mkdmbf$"
                    foundAsText = true
                    textReturn$ = MKDMBF$(temp##)
                CASE "_mk$"
                    foundAsText = true
                    'textReturn$ = _mk$
                CASE "cvsmbf"
                    GetVal## = CVSMBF(temp$)
                CASE "cvdmbf"
                    GetVal## = CVDMBF(temp$)
                CASE "cvi"
                    GetVal## = CVI(temp$)
                CASE "cvl"
                    GetVal## = CVL(temp$)
                CASE "cvs"
                    GetVal## = CVS(temp$)
                CASE "cvd"
                    GetVal## = CVD(temp$)
                CASE "_cv"
                    'GetVal## = _cv
                CASE "string$"
                    'foundAsText = true
                    'textReturn$ = string$(temp##)
                CASE "space$"
                    foundAsText = true
                    textReturn$ = SPACE$(temp##)
                CASE "instr"
                    'GetVal## = instr
                CASE "_instrrev"
                    'GetVal## = _instrrev
                CASE "mid$"
                    'foundAsText = true
                    'textReturn$ = mid$
                CASE "sqr"
                    GetVal## = SQR(temp##)
                CASE "tan"
                    GetVal## = TAN(temp##)
                CASE "atn"
                    GetVal## = ATN(temp##)
                CASE "log"
                    GetVal## = LOG(temp##)
                CASE "abs"
                    GetVal## = ABS(temp##)
                CASE "erl"
                    GetVal## = lineThatErrored
                CASE "err"
                    GetVal## = ERR
                CASE "ucase$"
                    foundAsText = true
                    textReturn$ = UCASE$(temp$)
                CASE "lcase$"
                    foundAsText = true
                    textReturn$ = LCASE$(temp$)
                CASE "left$"
                    'foundAsText = true
                    'textReturn$ = left$
                CASE "right$"
                    'foundAsText = true
                    'textReturn$ = right$
                CASE "ltrim$"
                    foundAsText = true
                    textReturn$ = LTRIM$(temp$)
                CASE "rtrim$"
                    foundAsText = true
                    textReturn$ = RTRIM$(temp$)
                CASE "_trim$"
                    foundAsText = true
                    textReturn$ = _TRIM$(temp$)
                CASE "_cwd$"
                    foundAsText = true
                    textReturn$ = _CWD$
                CASE "_startdir$"
                    foundAsText = true
                    textReturn$ = _STARTDIR$
                CASE "_dir$"
                    foundAsText = true
                    textReturn$ = _DIR$(temp$)
                CASE "_inclerrorfile$"
                    foundAsText = true
                    textReturn$ = _INCLERRORFILE$
                CASE "_atan2"
                    'GetVal## = _atan2
                CASE "_hypot"
                    'GetVal## = _hypot
                CASE "_pi"
                    IF temp$ = "" THEN temp## = 1
                    GetVal## = _PI(temp##)
                CASE "_desktopheight"
                    GetVal## = _DESKTOPHEIGHT
                CASE "_desktopwidth"
                    GetVal## = _DESKTOPWIDTH
                CASE "_screenexists"
                    GetVal## = _SCREENEXISTS
                CASE "_controlchr"
                    GetVal## = _CONTROLCHR
                CASE "_stricmp"
                    'GetVal## = _stricmp
                CASE "_strcmp"
                    'GetVal## = _strcmp
                CASE "_autodisplay"
                    GetVal## = _AUTODISPLAY
                CASE "_shr"
                    'GetVal## = _shr
                CASE "_shl"
                    'GetVal## = _shl
                CASE "_deflate$"
                    foundAsText = true
                    textReturn$ = _DEFLATE$(temp$)
                CASE "_inflate$"
                    foundAsText = true
                    textReturn$ = _INFLATE$(temp$)
                CASE "_readbit"
                    'GetVal## = _readbit
                CASE "_setbit"
                    'GetVal## = _setbit
                CASE "_resetbit"
                    'GetVal## = _resetbit
                CASE "_togglebit"
                    'GetVal## = _togglebit
            END SELECT
        ELSEIF vars(varIndex).type = varTypeSTRING THEN
            db_echo "found in strings()"
            foundAsText = true
            textReturn$ = strings(varIndex)
        ELSE
            GetVal## = nums(varIndex)
            db_echo "returning nums()"
        END IF
    ELSE
        db_echo "not found as var"
        IF detectType%%(c$) = varTypeSTRING THEN
            foundAsText = true
            textReturn$ = ""
            db_echo "returning an empty string"
        ELSE
            IF isNumber(c$) THEN
                GetVal## = VAL(c$)
                db_echo "returning val()"
            ELSE
                db_echo "returning 0"
            END IF
        END IF
    END IF
END FUNCTION

FUNCTION Parse$ (__inputExpr AS STRING)
    'Adapted from https://www.codeproject.com/Articles/1205435/Parsing-Mathematical-Expressions-in-VB-NET-Missi
    ' Call this routine to perform the actual mathematic expression parsing
    ' Comments retained from the original code are marked with OC.
    DIM t AS _UNSIGNED LONG, index AS _UNSIGNED LONG
    DIM totalStrings AS _UNSIGNED LONG
    DIM inputExpr AS STRING, temp$
    DIM returnAsText AS _BYTE, textReturn AS STRING
    REDIM oe(0) AS _UNSIGNED LONG
    REDIM strs(0) AS STRING

    db_echo "------------------ Parsing: " + __inputExpr

    inputExpr = "(" + __inputExpr + ")"

    t = 1
    'OC: Iterate through the characters of input string starting at the position of final character
    FOR index = LEN(inputExpr) - 1 TO 0 STEP -1
        'OC: For each character perform a check if its value is '('
        IF ASC(inputExpr, index + 1) = 40 OR index = 0 THEN
            DIM sb AS STRING
            sb = ""
            DIM n AS _UNSIGNED LONG
            'OC: Perform a check if this is the first character in string
            IF index = 0 THEN
                'OC: If so assign n variable to the value of variable index
                n = 1
            ELSE
                'OC: Otherwise assign n variable to the value of variable index + 1
                n = index + 1
            END IF

            DIM exists AS _BYTE
            DO
                exists = false
                DIM bracket AS _BYTE
                bracket = false
                'OC: Perform the iterations stepping forward into each succeeding character
                'OC: starting at the position n = index + 1 until we've found a character equal to ')'
                WHILE n < LEN(inputExpr) AND bracket = false
                    'OC: Check if the current character is not ')'.
                    IF ASC(inputExpr, n + 1) <> 41 THEN
                        'OC: If so, append it to the temporary string buffer
                        sb = sb + MID$(inputExpr, n + 1, 1)
                        'OC: Otherwise break the loop execution
                    ELSE
                        bracket = true
                    END IF
                    'OC: Increment the n loop counter variable by 1
                    n = n + 1
                WEND
                DIM r AS _UNSIGNED LONG
                r = 0
                'OC: Iterate through the array of positions
                WHILE r <= UBOUND(oe) AND exists = false
                    'OC: For each element perform a check if its value
                    'OC: is equal to the position of the current ')' character
                    IF oe(r) = n THEN
                        'OC: If so, append the character ')' to the temporary string buffer and break
                        'OC: the loop execution assigning the variable exists to the value 'true'
                        exists = true
                        sb = sb + ") "
                    END IF
                    r = r + 1
                WEND

                'OC: Repeat the following loop execution until we've found the character ')' at
                'OC: the New position which is not in the array of positions
            LOOP WHILE exists = true

            'OC: If the current character's ')' position has not been previous found,
            'OC: add the value of position to the array
            IF exists = false THEN
                REDIM _PRESERVE oe(UBOUND(oe) + 1)
                oe(t) = n
                t = t + 1
            END IF

            'OC: Add the currently obtained string containing a specific part of the expression to the array
            totalStrings = totalStrings + 1
            REDIM _PRESERVE strs(totalStrings)
            strs(totalStrings) = sb
        END IF
    NEXT

    'OC: Iterate through the array of the expression parts
    FOR index = 1 TO totalStrings
        'OC: Compute the result for the current part of the expression
        DIM Result AS STRING
        errorHappened = false
        Result = STR$(Compute(strs(index), returnAsText, textReturn))
        IF errorHappened THEN EXIT FUNCTION
        IF returnAsText THEN Result = " " + textReturn

        'OC: Iterate through all succeeding parts of the expression
        FOR n = index TO totalStrings
            'OC: For each part substitute the substring containing the current part of the expression
            'OC: with its numerical value without parentheses.
            strs(n) = Replace(strs(n), "(" + strs(index) + ")", Result, 0, 0)
        NEXT
    NEXT
    'OC: Compute the numerical value of the last part (e.g. the numerical resulting value of the entire expression)
    'OC: and return this value at the end of the following routine execution.
    errorHappened = false
    temp$ = STR$(Compute(strs(totalStrings), returnAsText, textReturn))
    IF errorHappened THEN EXIT FUNCTION
    IF returnAsText THEN
        Parse$ = textReturn
    ELSE
        Parse$ = temp$
    END IF
END FUNCTION

FUNCTION Compute## (expr AS STRING, foundAsText AS _BYTE, textReturn$)
    DIM i AS _UNSIGNED LONG, j AS _UNSIGNED LONG
    DIM l AS _UNSIGNED LONG, m AS _UNSIGNED LONG, lastIndex AS _UNSIGNED LONG
    DIM totalElements AS _UNSIGNED LONG
    DIM ch AS STRING, hasOperator%%
    DIM quote AS _BYTE
    DIM tempElement AS STRING, op1##, op2##, result##
    DIM txtop1$, txtop2$, txtresult$
    DIM getvalTxtRet1 AS _BYTE, getvalTxtResult1 AS STRING
    DIM getvalTxtRet2 AS _BYTE, getvalTxtResult2 AS STRING
    REDIM element(1000) AS STRING
    STATIC op(6) AS STRING, validOP$

    IF LEN(validOP$) = 0 THEN
        validOP$ = "^*/+-="
        FOR i = 1 TO LEN(validOP$)
            op(i) = MID$(validOP$, i, 1)
        NEXT
    END IF

    db_echo "* Entering Compute##(): " + expr

    'break down expr into element()
    FOR i = 1 TO LEN(expr)
        ch = MID$(expr, i, 1)
        IF ch = CHR$(34) THEN quote = NOT quote
        IF INSTR(validOP$, ch) THEN
            'this is an operator
            IF quote THEN
                tempElement = tempElement + ch
            ELSE
                IF LEN(tempElement) THEN GOSUB addElement
                tempElement = ch
                GOSUB addElement
                tempElement = ""
            END IF
        ELSE
            tempElement = tempElement + ch
        END IF
    NEXT
    IF LEN(tempElement) THEN GOSUB addElement

    IF debugging THEN
        DIM el$, tempElCount AS _UNSIGNED LONG
        el$ = ""
        tempElCount = 0
        FOR l = 1 TO totalElements
            IF LEN(_TRIM$(element$(l))) > 0 THEN
                tempElCount = tempElCount + 1
                el$ = el$ + element(l)
            END IF
        NEXT
        _ECHO "** Total elements:" + STR$(tempElCount) + " **"
        _ECHO el$
        _ECHO "     ***"
    END IF

    FOR i = 1 TO LEN(validOP$)
        FOR j = 1 TO totalElements
            IF element(j) = op(i) THEN
                hasOperator%% = true
                l = 1
                IF j - l > 0 THEN
                    DO UNTIL LEN(_TRIM$(element(j - l))) > 0 AND INSTR(validOP$, element(j - l)) = 0
                        l = l + 1
                        IF j - l < 1 THEN EXIT FUNCTION
                    LOOP
                END IF
                IF isNumber(element(j - l)) THEN
                    op1## = VAL(element(j - l))
                ELSE
                    op1## = GetVal(element(j - l), getvalTxtRet1, getvalTxtResult1)
                    IF getvalTxtRet1 THEN txtop1$ = getvalTxtResult1
                END IF
                db_echo "element(j - l) = " + element(j - l)
                m = 1
                IF j + m <= totalElements THEN
                    DO UNTIL LEN(_TRIM$(element(j + m))) > 0 AND INSTR(validOP$, element(j + m)) = 0
                        m = m + 1
                        IF j + m > totalElements THEN EXIT FUNCTION
                    LOOP
                END IF
                IF isNumber(element(j + m)) THEN
                    op2## = VAL(element(j + m))
                ELSE
                    op2## = GetVal(element(j + m), getvalTxtRet2, getvalTxtResult2)
                    IF getvalTxtRet2 THEN txtop2$ = getvalTxtResult2
                END IF
                db_echo "element(j + m) = " + element(j + m)
                db_echo "op1=" + STR$(op1##) + "; oper=" + op(i) + "; op2=" + STR$(op2##)
                db_echo "txtop1=" + txtop1$ + "; oper=" + op(i) + "; txtop2=" + txtop2$
                SELECT CASE op(i)
                    CASE "^"
                        IF getvalTxtRet1 OR getvalTxtRet2 THEN throwError 13: EXIT FUNCTION
                        foundAsText = false
                        result## = op1## ^ op2##
                    CASE "*"
                        IF getvalTxtRet1 OR getvalTxtRet2 THEN throwError 13: EXIT FUNCTION
                        foundAsText = false
                        result## = op1## * op2##
                    CASE "/"
                        IF getvalTxtRet1 OR getvalTxtRet2 THEN throwError 13: EXIT FUNCTION
                        foundAsText = false
                        result## = op1## / op2##
                    CASE "+"
                        IF getvalTxtRet1 AND getvalTxtRet2 THEN
                            txtresult$ = txtop1$ + txtop2$
                            foundAsText = true
                        ELSEIF NOT getvalTxtRet1 AND NOT getvalTxtRet2 THEN
                            result## = op1## + op2##
                            foundAsText = false
                        ELSE
                            throwError 13: EXIT FUNCTION
                        END IF
                    CASE "-"
                        IF getvalTxtRet1 OR getvalTxtRet2 THEN throwError 13: EXIT FUNCTION
                        result## = op1## - op2##
                        foundAsText = false
                    CASE "="
                        IF getvalTxtRet1 AND getvalTxtRet2 THEN
                            result## = (txtop1$ = txtop2$)
                            foundAsText = true
                        ELSEIF NOT getvalTxtRet1 AND NOT getvalTxtRet2 THEN
                            result## = (op1## = op2##)
                            foundAsText = false
                        ELSE
                            throwError 13: EXIT FUNCTION
                        END IF
                END SELECT
                db_echo "temp result## =" + STR$(result##)
                db_echo "temp txtresult$ =" + txtresult$
                element(j - l) = ""
                element(j + m) = ""
                IF foundAsText THEN
                    element(j) = txtresult$
                ELSE
                    element(j) = STR$(result##)
                END IF
                lastIndex = j
                IF debugging THEN
                    el$ = ""
                    tempElCount = 0
                    FOR l = 1 TO totalElements
                        IF LEN(_TRIM$(element$(l))) > 0 THEN
                            tempElCount = tempElCount + 1
                            el$ = el$ + element(l)
                        END IF
                    NEXT
                    _ECHO "** Total elements:" + STR$(tempElCount) + " **"
                    _ECHO el$
                    _ECHO "     ***"
                END IF
            END IF
        NEXT
    NEXT

    IF hasOperator%% = false AND totalElements = 1 THEN
        IF isNumber(element(1)) THEN
            op1## = VAL(element(1))
        ELSE
            op1## = GetVal(element(1), getvalTxtRet1, getvalTxtResult1)
        END IF
        IF getvalTxtRet1 THEN
            foundAsText = true
            textReturn$ = getvalTxtResult1
        ELSE
            foundAsText = false
            Compute## = op1##
        END IF
    ELSE
        IF foundAsText THEN
            textReturn$ = element(lastIndex)
        ELSE
            Compute## = VAL(element(lastIndex))
        END IF
    END IF

    EXIT FUNCTION
    addElement:
    totalElements = totalElements + 1
    IF totalElements > UBOUND(element) THEN
        REDIM _PRESERVE element(UBOUND(element) + 1000) AS STRING
    END IF
    element(totalElements) = tempElement
    RETURN
END FUNCTION

FUNCTION Replace$ (TempText$, SubString$, NewString$, CaseSensitive AS _BYTE, TotalReplacements AS LONG)
    DIM FindSubString AS LONG, Text$

    IF LEN(TempText$) = 0 THEN EXIT SUB

    Text$ = TempText$
    TotalReplacements = 0
    DO
        IF CaseSensitive THEN
            FindSubString = INSTR(FindSubString + 1, Text$, SubString$)
        ELSE
            FindSubString = INSTR(FindSubString + 1, UCASE$(Text$), UCASE$(SubString$))
        END IF
        IF FindSubString = 0 THEN EXIT DO
        IF LEFT$(SubString$, 1) = "\" THEN 'Escape sequence
            'Replace the Substring if it's not preceeded by another backslash
            IF MID$(Text$, FindSubString - 1, 1) <> "\" THEN
                Text$ = LEFT$(Text$, FindSubString - 1) + NewString$ + MID$(Text$, FindSubString + LEN(SubString$))
                TotalReplacements = TotalReplacements + 1
            END IF
        ELSE
            Text$ = LEFT$(Text$, FindSubString - 1) + NewString$ + MID$(Text$, FindSubString + LEN(SubString$))
            TotalReplacements = TotalReplacements + 1
        END IF
    LOOP

    Replace$ = Text$
END FUNCTION

FUNCTION isNumber%% (__a$)
    DIM i AS _UNSIGNED LONG
    DIM a AS _UNSIGNED _BYTE
    DIM D AS _UNSIGNED LONG, E AS _UNSIGNED LONG
    DIM dp AS _UNSIGNED LONG
    DIM a$

    a$ = _TRIM$(__a$)

    IF LEN(a$) = 0 THEN EXIT FUNCTION
    FOR i = 1 TO LEN(a$)
        a = ASC(MID$(a$, i, 1))
        IF a = 45 THEN
            IF (i = 1 AND LEN(a$) > 1) OR (i > 1 AND ((D > 0 AND D = i - 1) OR (E > 0 AND E = i - 1))) THEN _CONTINUE
            EXIT FUNCTION
        END IF
        IF a = 46 THEN
            IF dp = 1 THEN EXIT FUNCTION
            dp = 1
            _CONTINUE
        END IF
        IF a = 100 OR a = 68 THEN 'D
            IF D > 0 OR E > 0 THEN EXIT FUNCTION
            IF i = 1 THEN EXIT FUNCTION
            D = i
            _CONTINUE
        END IF
        IF a = 101 OR a = 69 THEN 'E
            IF D > 0 OR E > 0 THEN EXIT FUNCTION
            IF i = 1 THEN EXIT FUNCTION
            E = i
            _CONTINUE
        END IF
        IF a = 43 THEN '+
            IF (D > 0 AND D = i - 1) OR (E > 0 AND E = i - 1) THEN _CONTINUE
            EXIT FUNCTION
        END IF

        IF a >= 48 AND a <= 57 THEN _CONTINUE
        EXIT FUNCTION
    NEXT
    isNumber%% = true
END FUNCTION

SUB db_echo (text$)
    IF debugging THEN _ECHO text$
END SUB

SUB throwError (code AS INTEGER)
    IF running THEN PRINT "("; _TRIM$(STR$(_ERRORLINE)); ") "
    PRINT "Error #"; code;
    IF running THEN
        PRINT " on line"; currentLine
        lineThatErrored = currentLine
        running = false
    END IF
    errorHappened = true
END SUB
