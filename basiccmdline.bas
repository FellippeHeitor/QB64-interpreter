CONST true = -1, false = NOT true

$CONSOLE
_CONSOLE OFF

DIM SHARED debugging AS _BYTE
debugging = false

ON ERROR GOTO oops

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
DIM SHARED thisScope$, currentLine AS _UNSIGNED LONG
DIM varIndex AS _UNSIGNED LONG, i AS _UNSIGNED LONG
DIM doLine AS _UNSIGNED LONG, loopLine AS _UNSIGNED LONG
DIM ifLine AS _UNSIGNED LONG
DIM k AS LONG, externalLimit AS INTEGER
DIM temp$
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
varIndex = addVar("val"): vars(varIndex).protected = true
varIndex = addVar("int"): vars(varIndex).protected = true
varIndex = addVar("asc"): vars(varIndex).protected = true
varIndex = addVar("cos"): vars(varIndex).protected = true
varIndex = addVar("sin"): vars(varIndex).protected = true
varIndex = addVar("len"): vars(varIndex).protected = true
varIndex = addVar("rnd"): vars(varIndex).protected = true
varIndex = addVar("timer"): vars(varIndex).protected = true
varIndex = addVar("time$"): vars(varIndex).protected = true
varIndex = addVar("date$"): vars(varIndex).protected = true
varIndex = addVar("chr$"): vars(varIndex).protected = true
varIndex = addVar("inkey$"): vars(varIndex).protected = true
varIndex = addVar("_width"): vars(varIndex).protected = true
varIndex = addVar("_height"): vars(varIndex).protected = true
varIndex = addVar("_mousex"): vars(varIndex).protected = true
varIndex = addVar("_mousey"): vars(varIndex).protected = true
varIndex = addVar("_mousebutton(1)"): vars(varIndex).protected = true
varIndex = addVar("_mousebutton(2)"): vars(varIndex).protected = true

SCREEN CurrentSCREEN%
DO
    k = _KEYHIT

    WHILE _MOUSEINPUT: WEND

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
    IF LEFT$(L$, 5) = "LOAD " THEN
        tryWithExtension:
        IF NOT running THEN
            IF _FILEEXISTS(MID$(L1$, 6)) THEN
                loaded = load(MID$(L1$, 6))
                IF loaded THEN PRINT "Loaded."
            ELSE
                IF RIGHT$(LCASE$(MID$(L1$, 6)), 4) = ".bas" THEN
                    PRINT "File not found - "; MID$(L1$, 6)
                ELSE
                    L1$ = L1$ + ".bas"
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
        CHDIR MID$(L1$, 7)
    ELSEIF LEFT$(L$, 6) = "MKDIR " THEN
        MKDIR MID$(L1$, 7)
    ELSEIF LEFT$(L$, 5) = "KILL " THEN
        KILL MID$(L1$, 6)
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
                WIDTH , GetVal(p2$, 0, "")
            ELSE
                WIDTH GetVal(p1$, 0, ""), GetVal(p2$, 0, "")
            END IF
        ELSE
            WIDTH GetVal(p$, 0, "")
        END IF
    ELSEIF LEFT$(L$, 10) = "RANDOMIZE " THEN
        IF GetVal(MID$(L$, 11), 0, "") > 0 THEN
            RANDOMIZE GetVal(MID$(L$, 11), 0, "")
        END IF
    ELSEIF LEFT$(L$, 7) = "_LIMIT " THEN
        IF GetVal(MID$(L$, 8), 0, "") > 0 THEN
            externalLimit = GetVal(MID$(L$, 8), 0, "")
        END IF
    ELSEIF LEFT$(L$, 1) = "'" OR LEFT$(L$, 4) = "REM " OR L$ = "'" OR L$ = "REM" OR L$ = "" THEN
        'it's a comment.
    ELSEIF L$ = "KEY OFF" THEN
        KEY OFF
    ELSEIF L$ = "KEY ON" THEN
        KEY ON
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
                COLOR GetVal(c1$, 0, ""), GetVal(c2$, 0, "")
            ELSEIF LEN(c1$) > 0 AND LEN(c2$) = 0 THEN
                COLOR GetVal(c1$, 0, "")
            ELSEIF LEN(c1$) = 0 AND LEN(c2$) > 0 THEN
                COLOR , GetVal(c2$, 0, "")
            END IF
        ELSE
            COLOR GetVal(c$, 0, "")
        END IF
    ELSEIF LEFT$(L$, 7) = "LOCATE " THEN
        c$ = MID$(L$, 8)
        IF INSTR(c$, ",") THEN
            c1$ = LEFT$(c$, INSTR(c$, ",") - 1)
            c2$ = MID$(c$, INSTR(c$, ",") + 1)
            IF LEN(c1$) > 0 AND LEN(c2$) > 0 THEN
                LOCATE GetVal(c1$, 0, ""), GetVal(c2$, 0, "")
            ELSEIF LEN(c1$) > 0 AND LEN(c2$) = 0 THEN
                LOCATE GetVal(c1$, 0, "")
            ELSEIF LEN(c1$) = 0 AND LEN(c2$) > 0 THEN
                LOCATE , GetVal(c2$, 0, "")
            END IF
        ELSE
            LOCATE GetVal(c$, 0, "")
        END IF
    ELSEIF LEFT$(L$, 6) = "FILES " THEN
        DIM fileSpec$
        temp$ = doMath$(MID$(L1$, 7))
        FILES temp$
    ELSEIF L$ = "FILES" THEN
        FILES
    ELSEIF LEFT$(L$, 8) = "CIRCLE (" THEN
        IF CurrentSCREEN% = 0 THEN PRINT "Invalid mode.": running = false: GOTO Parse.Done
        Comma1% = INSTR(L$, ","): Comma2% = INSTR(Comma1% + 1, L$, ","): Comma3% = INSTR(Comma2% + 1, L$, ",")

        XPos1% = INSTR(L$, " (") + 2
        YPos1% = Comma1% + 1
        X1% = GetVal(MID$(L$, XPos1%, Comma1% - XPos1%), 0, "")
        Y1% = GetVal(MID$(L$, YPos1%, Comma2% - YPos1% - 1), 0, "")

        Rad% = GetVal(MID$(L$, Comma2% + 1, Comma3% - Comma2% - 1), 0, "")

        c$ = LTRIM$(RTRIM$(LEFT$(MID$(L$, Comma3% + 1), 3))) 'Color attribute (variable or constant)
        IF RIGHT$(c$, 1) = "," THEN c$ = LEFT$(c$, LEN(c$) - 1) 'If single-digit attribute

        IF INSTR("0123456789", LEFT$(c$, 1)) > 0 THEN DrawClr% = VAL(c$) ELSE DrawClr% = GetVal(c$, 0, "")

        EPos% = INSTR(L$, ", , , ")

        IF EPos% > 0 THEN
            EPos% = EPos% + 6: Elipse = GetVal(MID$(L$, EPos%), 0, "")
        ELSE
            Arc% = INSTR(Comma3% + 1, L$, ",")

            IF Arc% > 0 THEN
                Comma4% = Arc%
                Comma5% = INSTR(Comma4% + 1, L$, ",")

                ArcBeg = GetVal(MID$(L$, Comma4% + 1, Comma5% - Comma4% - 1), 0, "") ': PRINT "ArcBeg:"; ArcBeg;   '* * * * Test PRINT
                ArcEnd = GetVal(MID$(L$, Comma5% + 1), 0, "") ': PRINT " ArcEnd:"; ArcEnd;

                IF INSTR(Comma5% + 1, L$, ",") > 0 THEN EPos% = INSTR(Comma5% + 1, L$, ",") + 1: Elipse = GetVal(MID$(L$, EPos%), 0, "")
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
        IF GetVal(MID$(L$, 7), 0, "") > 0 THEN
            SLEEP GetVal(MID$(L$, 7), 0, "")
        END IF
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

        PRINT doMath(MID$(L1$, 7));
        IF NOT retainCursor THEN PRINT
    ELSEIF LEFT$(L$, 7) = "_TITLE " THEN
        temp$ = doMath$(MID$(L1$, 8))
        _TITLE temp$
    ELSEIF L$ = "CLS" THEN
        CLS
    ELSEIF L$ = "SYSTEM" OR L$ = "EXIT" THEN
        SYSTEM
    ELSEIF L$ = "END" THEN
        IF running THEN running = false
    ELSEIF LEFT$(L$, 6) = "INPUT " THEN
        DIM varName$, d$
        varName$ = MID$(L1$, 7)
        varIndex = addVar(varName$)
        IF vars(varIndex).type = varTypeSTRING THEN
            INPUT "", d$
            strings(varIndex) = d$
        ELSE
            INPUT "", d##
            nums(varIndex) = d##
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
        SELECT CASE VAL(MID$(L$, 8))
            CASE 0 TO 2, 7 TO 13
                CurrentSCREEN% = VAL(MID$(L$, 8))
                SCREEN CurrentSCREEN%
            CASE ELSE
                PRINT "Invalid mode."
        END SELECT
    ELSEIF LEFT$(L$, 6) = "SCREEN" THEN
        PRINT CurrentSCREEN%
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

            r = false
            SELECT CASE s$
                CASE "="
                    IF GetVal(i1$, 0, "") = GetVal(i2$, 0, "") THEN r = true
                CASE ">"
                    IF GetVal(i1$, 0, "") > GetVal(i2$, 0, "") THEN r = true
                CASE "<"
                    IF GetVal(i1$, 0, "") < GetVal(i2$, 0, "") THEN r = true
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
            strings(varIndex) = doMath(v$)
        ELSE
            v$ = MID$(L1$, INSTR(L1$, "=") + 1)

            t$ = doMath(v$)

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

oops:
IF MyBad THEN RESUME NEXT

PRINT
PRINT "("; _ERRORLINE; ") Error #"; ERR;
IF running THEN
    PRINT " on line"; currentLine
    running = false
ELSE
    PRINT
END IF
RESUME Parse.Done

FUNCTION addVar~& (varName$)
    DIM i AS _UNSIGNED LONG, found AS _BYTE

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
        detectType%% = varType_LONG
    END IF
END FUNCTION

FUNCTION searchVar~& (__varName$)
    DIM i AS _UNSIGNED LONG, found AS _BYTE
    DIM temp##, temp$, special AS _BYTE
    DIM varName$, isString AS _BYTE

    varName$ = __varName$

    DIM bracket1 AS LONG, bracket2 AS LONG
    bracket1 = INSTR(varName$, "(")
    IF bracket1 > 0 THEN
        FOR i = LEN(varName$) TO 1 STEP -1
            IF ASC(varName$, i) = 41 THEN
                bracket2 = i
                EXIT FOR
            END IF
        NEXT
    END IF

    IF bracket1 > 0 AND bracket2 > 0 THEN
        'array or function
        temp$ = MID$(varName$, bracket1 + 1, bracket2 - bracket1 - 1)
        IF LEFT$(temp$, 1) = CHR$(34) THEN
            'string literal
            temp$ = MID$(temp$, 2)
            IF RIGHT$(temp$, 1) = CHR$(34) THEN
                temp$ = LEFT$(temp$, LEN(temp$) - 1)
            END IF
            isString = true
        ELSEIF firstOperator(temp$) THEN
            temp## = VAL(doMath(MID$(varName$, bracket1 + 1, bracket2 - bracket1 - 1)))
        ELSE
            'a variable?
            DIM checkVar AS _UNSIGNED LONG
            checkVar = searchVar(temp$)
            IF checkVar THEN
                IF vars(checkVar).type = varTypeSTRING THEN
                    temp$ = strings(checkVar)
                    isString = true
                ELSE
                    temp## = nums(checkVar)
                END IF
            ELSE
                'number literal?
                IF LTRIM$(STR$(VAL(temp$))) = temp$ THEN
                    temp## = VAL(temp$)
                END IF
            END IF
        END IF

        SELECT CASE LCASE$(LTRIM$(RTRIM$(LEFT$(varName$, bracket1 - 1))))
            CASE "val"
                IF isString = false THEN ERROR 5
                temp## = VAL(temp$)
                varName$ = "val"
                special = true
            CASE "cos"
                IF isString THEN ERROR 5
                temp## = COS(temp##)
                varName$ = "cos"
                special = true
            CASE "len"
                IF isString = false THEN ERROR 5
                temp## = LEN(temp$)
                varName$ = "len"
                special = true
            CASE "asc"
                IF isString = false THEN ERROR 5
                temp## = ASC(temp$)
                varName$ = "asc"
                special = true
            CASE "sin"
                IF isString THEN ERROR 5
                temp## = SIN(temp##)
                varName$ = "sin"
                special = true
            CASE "int"
                IF isString THEN ERROR 5
                temp## = INT(temp##)
                varName$ = "int"
                special = true
            CASE "chr$"
                IF isString THEN ERROR 5
                temp$ = CHR$(temp##)
                varName$ = "chr$"
                special = true
        END SELECT
    ELSE
        'special cases
        IF LCASE$(LTRIM$(RTRIM$(varName$))) = "rnd" THEN
            temp## = RND
            special = true
        ELSEIF LCASE$(LTRIM$(RTRIM$(varName$))) = "timer" THEN
            temp## = TIMER
            special = true
        ELSEIF LCASE$(LTRIM$(RTRIM$(varName$))) = "time$" THEN
            temp$ = TIME$
            special = true
        ELSEIF LCASE$(LTRIM$(RTRIM$(varName$))) = "inkey$" THEN
            temp$ = INKEY$
            special = true
        ELSEIF LCASE$(LTRIM$(RTRIM$(varName$))) = "date$" THEN
            temp$ = DATE$
            special = true
        ELSEIF LCASE$(LTRIM$(RTRIM$(varName$))) = "_width" THEN
            temp## = _WIDTH
            special = true
        ELSEIF LCASE$(LTRIM$(RTRIM$(varName$))) = "_height" THEN
            temp## = _HEIGHT
            special = true
        ELSEIF LCASE$(LTRIM$(RTRIM$(varName$))) = "_mousex" THEN
            temp## = _MOUSEX
            special = true
        ELSEIF LCASE$(LTRIM$(RTRIM$(varName$))) = "_mousey" THEN
            temp## = _MOUSEY
            special = true
        ELSEIF LCASE$(LTRIM$(RTRIM$(varName$))) = "_mousebutton(1)" THEN
            temp## = _MOUSEBUTTON(1)
            special = true
        ELSEIF LCASE$(LTRIM$(RTRIM$(varName$))) = "_mousebutton(2)" THEN
            temp## = _MOUSEBUTTON(2)
            special = true
        END IF
    END IF

    'check if var exists
    FOR i = 1 TO totalVars
        IF LCASE$(LTRIM$(RTRIM$(vars(i).name))) = LCASE$(LTRIM$(RTRIM$(varName$))) THEN
            found = true
            EXIT FOR
        END IF
    NEXT

    IF found THEN searchVar~& = i
    IF special AND i <= UBOUND(vars) THEN
        nums(i) = temp##
        strings(i) = temp$
    END IF
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
    DIM c$, b1&, b2&, temp##

    IF debugging THEN _ECHO "entering getval(): " + __c$

    c$ = LTRIM$(RTRIM$(__c$))

    IF firstOperator(c$) > 1 THEN
        c$ = LEFT$(c$, firstOperator(c$) - 1)
        IF debugging THEN _ECHO "has operator"
    ELSE
        IF debugging THEN _ECHO "no operator"
    END IF

    IF LEFT$(c$, 1) = CHR$(34) THEN
        IF debugging THEN _ECHO "literal string"
        foundAsText = true
        textReturn$ = removeQuote$(c$)
        EXIT FUNCTION
    END IF

    IF VAL(c$) <> 0 THEN
        GetVal## = VAL(c$)
        IF debugging THEN _ECHO "returning val()"
    ELSE
        varIndex = searchVar(c$)
        IF debugging THEN _ECHO "searching as var: " + c$
        IF varIndex THEN
            IF vars(varIndex).type = varTypeSTRING THEN
                IF debugging THEN _ECHO "found in strings()"
                foundAsText = true
                textReturn$ = strings(varIndex)
            ELSE
                GetVal## = nums(varIndex)
                IF debugging THEN _ECHO "returning nums()"
            END IF
        ELSE
            IF debugging THEN _ECHO "not found as var"
            IF detectType%%(c$) = varTypeSTRING THEN
                foundAsText = true
                textReturn$ = ""
                IF debugging THEN _ECHO "returning an empty string"
            ELSE
                GetVal## = VAL(c$)
                IF debugging THEN _ECHO "returning val() anyway"
            END IF
        END IF
    END IF
END FUNCTION

FUNCTION doMath$ (__v$)
    DIM v$, v1$, v2$, temp$, continue AS _BYTE
    DIM v1##, v2##
    DIM returnAsText AS _BYTE, textReturn$
    DIM returnAsText2 AS _BYTE, textReturn2$
    DIM tempQuote$
    STATIC stack AS _UNSIGNED LONG

    stack = stack + 1
    v$ = __v$

    IF debugging THEN _ECHO STR$(stack) + " -------------- doing math on " + v$

    IF firstOperator(v$) > 1 THEN
        s$ = MID$(v$, firstOperator(v$), 1)
        IF debugging THEN _ECHO "found operator: " + s$
    ELSE
        v1## = GetVal(v$, returnAsText, textReturn$)
        IF returnAsText THEN
            doMath$ = textReturn$
        ELSE
            doMath$ = LTRIM$(RTRIM$(STR$(GetVal(v$, 0, ""))))
        END IF
        stack = stack - 1: EXIT FUNCTION
    END IF

    v1$ = LEFT$(v$, INSTR(v$, s$) - 1)
    v2$ = MID$(v$, firstOperator(v$) + 1)

    v1## = GetVal(v1$, returnAsText, textReturn$)
    v2## = GetVal(v2$, returnAsText2, textReturn2$)
    IF debugging THEN
        _ECHO "found    " + v1$ + " " + s$ + " " + v2$
        _ECHO "getval():" + STR$(v1##) + "," + STR$(v2##)
        IF returnAsText THEN _ECHO "getval(): v1$ as text = " + textReturn$
        IF returnAsText2 THEN _ECHO "getval(): v2$ as text = " + textReturn2$
    END IF

    SELECT CASE s$
        CASE "+"
            IF returnAsText THEN
                temp$ = textReturn$ + textReturn2$
            ELSE
                temp$ = STR$(v1## + v2##)
            END IF
        CASE "-"
            IF returnAsText OR returnAsText2 THEN ERROR 5
            temp$ = STR$(v1## - v2##)
        CASE "*"
            IF returnAsText OR returnAsText2 THEN ERROR 5
            temp$ = STR$(v1## * v2##)
        CASE "/"
            IF returnAsText OR returnAsText2 THEN ERROR 5
            temp$ = STR$(v1## / v2##)
        CASE "^"
            IF returnAsText OR returnAsText2 THEN ERROR 5
            temp$ = STR$(v1## ^ v2##)
        CASE "="
            IF returnAsText OR returnAsText2 THEN
                temp$ = STR$(textReturn$ = textReturn2$)
            ELSE
                temp$ = STR$(v1## = v2##)
            END IF
    END SELECT

    IF debugging THEN _ECHO "temp$=" + temp$

    DO WHILE firstOperator(v2$) > 1
        s$ = MID$(v2$, firstOperator(v2$), 1)
        v2$ = MID$(v2$, firstOperator(v2$) + 1)
        IF debugging THEN _ECHO "passing " + temp$ + s$ + v2$
        IF returnAsText THEN tempQuote$ = CHR$(34) ELSE tempQuote$ = ""
        temp$ = doMath$(tempQuote$ + temp$ + tempQuote$ + s$ + v2$)
        v2$ = MID$(v2$, firstOperator(v2$) + LEN(v2$))
    LOOP

    doMath$ = LTRIM$(RTRIM$(temp$))
    stack = stack - 1
END FUNCTION

FUNCTION inQuote%% (__text$, __position AS _UNSIGNED LONG)
    DIM text$, position AS _UNSIGNED LONG
    DIM openQuote AS _BYTE
    DIM i AS _UNSIGNED LONG

    text$ = __text$
    position = __position

    IF position > LEN(text$) THEN position = LEN(text$)

    FOR i = 1 TO position
        IF ASC(text$, i) = 34 THEN openQuote = NOT openQuote
    NEXT

    inQuote%% = openQuote
END FUNCTION

FUNCTION firstOperator& (v$)
    DIM i AS LONG, op$

    op$ = "+-*/^="

    FOR i = 1 TO LEN(v$)
        IF INSTR(op$, MID$(v$, i, 1)) THEN
            IF NOT inQuote%%(v$, i) THEN
                firstOperator = i
                EXIT FUNCTION
            END IF
        END IF
    NEXT
END FUNCTION
