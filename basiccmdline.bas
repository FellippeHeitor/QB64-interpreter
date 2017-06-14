CONST true = -1, false = NOT true
CONST debugging = false

ON ERROR GOTO oops

KEY 2, "LIST"
KEY 5, "RUN"
KEY ON

TYPE vartype
    name AS STRING * 40
    type AS _BYTE
    scope AS STRING * 50
    protected AS _BYTE
END TYPE

CONST varTypeFLOAT = 0
CONST varTypeSTRING = 1
CONST varTypeINTEGER = 2

REDIM SHARED vars(0) AS vartype
REDIM SHARED strings(0) AS STRING
REDIM SHARED nums(0) AS _FLOAT
REDIM SHARED program(0) AS STRING
DIM SHARED totalVars AS _UNSIGNED LONG
DIM SHARED thisScope$, currentLine AS _UNSIGNED LONG
DIM varIndex AS _UNSIGNED LONG, i AS _UNSIGNED LONG
DIM doLine AS _UNSIGNED LONG, loopLine AS _UNSIGNED LONG
DIM ifLine AS _UNSIGNED LONG
DIM k AS LONG, externalLimit AS INTEGER
DIM SHARED running AS _BYTE, loaded AS _BYTE, loadedFile$

thisScope$ = "MAIN MODULE"

IF _FILEEXISTS(COMMAND$) THEN
    loaded = load(COMMAND$)
    IF loaded THEN PRINT "Loaded. - "; COMMAND$
END IF

'internal variables (functions)
varIndex = addVar("rnd"): vars(varIndex).protected = true
varIndex = addVar("timer"): vars(varIndex).protected = true
varIndex = addVar("time$"): vars(varIndex).protected = true
varIndex = addVar("date$"): vars(varIndex).protected = true
varIndex = addVar("_width"): vars(varIndex).protected = true
varIndex = addVar("_height"): vars(varIndex).protected = true
varIndex = addVar("_mousex"): vars(varIndex).protected = true
varIndex = addVar("_mousey"): vars(varIndex).protected = true
varIndex = addVar("_mousebutton(1)"): vars(varIndex).protected = true
varIndex = addVar("_mousebutton(2)"): vars(varIndex).protected = true

SCREEN s%
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
    ELSEIF LEFT$(L$, 7) = "INSERT " AND VAL(MID$(L$, 8)) > 0 THEN
        IF loaded THEN
            IF VAL(MID$(L$, 8)) <= UBOUND(program) THEN
                REDIM _PRESERVE program(UBOUND(program) + 1)
                FOR i = UBOUND(program) - 1 TO VAL(MID$(L$, 8)) STEP -1
                    program(i + 1) = program(i)
                NEXT
                program(VAL(MID$(L$, 8))) = ""
            ELSE
                PRINT "Invalid line number -"; VAL(MID$(L$, 8))
            END IF
        ELSE
            PRINT "No program loaded."
        END IF
    ELSEIF L$ = "SAVE" OR LEFT$(L$, 5) = "SAVE " THEN
        IF LEN(L$) > 4 THEN loadedFile$ = MID$(L$, 6)

        IF loaded THEN
            IF loadedFile$ = "" THEN
                PRINT "You must specify a file name."
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
    ELSEIF L$ = "NEW" OR LEFT$(L$, 4) = "NEW " THEN
        IF LEN(L$) > 3 THEN loadedFile$ = MID$(L$, 5)
        loaded = true
        REDIM SHARED program(1) AS STRING
    ELSEIF L$ = "_DISPLAY" THEN
        _DISPLAY
    ELSEIF LEFT$(L$, 5) = "EDIT " AND VAL(MID$(L$, 6)) > 0 THEN
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
    ELSEIF L$ = "LIST" THEN
        IF loaded THEN
            FOR i = 1 TO UBOUND(program)
                PRINT program(i)
            NEXT
            PRINT "End of file - "; loadedFile$
        ELSE
            PRINT "No program loaded."
        END IF
    ELSEIF LEFT$(L$, 5) = "LIST " AND VAL(MID$(L$, 6)) > 0 THEN
        IF loaded THEN
            IF VAL(MID$(L$, 6)) <= UBOUND(program) THEN
                PRINT program(VAL(MID$(L$, 6)))
            ELSE
                PRINT "Invalid line number -"; VAL(MID$(L$, 6))
            END IF
        ELSE
            PRINT "No program loaded."
        END IF
    ELSEIF LEFT$(L$, 6) = "WIDTH " THEN
        DIM p1$, p2$
        p$ = MID$(L$, 7)
        IF INSTR(p$, ",") THEN
            p1$ = LEFT$(p$, INSTR(p$, ",") - 1)
            p2$ = MID$(p$, INSTR(p$, ",") + 1)
            IF LEN(p1$) = 0 THEN
                WIDTH , GetVal(p2$)
            ELSE
                WIDTH GetVal(p1$), GetVal(p2$)
            END IF
        ELSE
            WIDTH GetVal(p$)
        END IF
    ELSEIF LEFT$(L$, 10) = "RANDOMIZE " AND GetVal(MID$(L$, 11)) > 0 THEN
        RANDOMIZE GetVal(MID$(L$, 11))
    ELSEIF LEFT$(L$, 7) = "_LIMIT " AND GetVal(MID$(L$, 8)) > 0 THEN
        externalLimit = GetVal(MID$(L$, 8))
    ELSEIF LEFT$(L$, 1) = "'" OR LEFT$(L$, 4) = "REM " OR L$ = "'" OR L$ = "REM" OR L$ = "" THEN
        'it's a comment.
    ELSEIF L$ = "KEY OFF" THEN
        KEY OFF
    ELSEIF L$ = "KEY ON" THEN
        KEY ON
    ELSEIF L$ = "RUN" THEN
        IF loaded THEN
            currentLine = 0
            externalLimit = 0
            running = true
        ELSE
            PRINT "No program loaded."
        END IF
    ELSEIF LEFT$(L$, 6) = "COLOR " THEN
        DIM c$, c1$, c2$
        c$ = MID$(L$, 7)
        IF INSTR(c$, ",") THEN
            c1$ = LEFT$(c$, INSTR(c$, ",") - 1)
            c2$ = MID$(c$, INSTR(c$, ",") + 1)
            IF LEN(c1$) > 0 AND LEN(c2$) > 0 THEN
                COLOR GetVal(c1$), GetVal(c2$)
            ELSEIF LEN(c1$) > 0 AND LEN(c2$) = 0 THEN
                COLOR GetVal(c1$)
            ELSEIF LEN(c1$) = 0 AND LEN(c2$) > 0 THEN
                COLOR , GetVal(c2$)
            END IF
        ELSE
            COLOR GetVal(c$)
        END IF
    ELSEIF L$ = "FILES" THEN
        FILES
    ELSEIF LEFT$(L$, 8) = "CIRCLE (" THEN
        IF s% = 0 THEN PRINT "Invalid mode.": running = false: GOTO Parse.Done
        Comma1% = INSTR(L$, ","): Comma2% = INSTR(Comma1% + 1, L$, ","): Comma3% = INSTR(Comma2% + 1, L$, ",")

        XPos1% = INSTR(L$, " (") + 2
        YPos1% = Comma1% + 1
        X1% = GetVal(MID$(L$, XPos1%, Comma1% - XPos1%))
        Y1% = GetVal(MID$(L$, YPos1%, Comma2% - YPos1% - 1))

        Rad% = GetVal(MID$(L$, Comma2% + 1, Comma3% - Comma2% - 1))

        c$ = LTRIM$(RTRIM$(LEFT$(MID$(L$, Comma3% + 1), 3))) 'Color attribute (variable or constant)
        IF RIGHT$(c$, 1) = "," THEN c$ = LEFT$(c$, LEN(c$) - 1) 'If single-digit attribute

        IF INSTR("0123456789", LEFT$(c$, 1)) > 0 THEN DrawClr% = VAL(c$) ELSE DrawClr% = GetVal(c$)

        EPos% = INSTR(L$, ", , , ")

        IF EPos% > 0 THEN
            EPos% = EPos% + 6: Elipse = GetVal(MID$(L$, EPos%))
        ELSE
            Arc% = INSTR(Comma3% + 1, L$, ",")

            IF Arc% > 0 THEN
                Comma4% = Arc%
                Comma5% = INSTR(Comma4% + 1, L$, ",")

                ArcBeg = GetVal(MID$(L$, Comma4% + 1, Comma5% - Comma4% - 1)) ': PRINT "ArcBeg:"; ArcBeg;   '* * * * Test PRINT
                ArcEnd = GetVal(MID$(L$, Comma5% + 1)) ': PRINT " ArcEnd:"; ArcEnd;

                IF INSTR(Comma5% + 1, L$, ",") > 0 THEN EPos% = INSTR(Comma5% + 1, L$, ",") + 1: Elipse = GetVal(MID$(L$, EPos%))
            END IF
        END IF

        IF Arc% > 0 AND Elipse = 0 THEN CIRCLE (X1%, Y1%), Rad%, DrawClr%, ArcBeg, ArcEnd: GOTO Circ.Done
        IF Elipse > 0 AND Arc% = 0 THEN CIRCLE (X1%, Y1%), Rad%, DrawClr%, , , Elipse: GOTO Circ.Done
        IF Arc% > 0 AND Elipse > 0 THEN CIRCLE (X1%, Y1%), Rad%, DrawClr%, ArcBeg, ArcEnd, Elipse: GOTO Circ.Done
        CIRCLE (X1%, Y1%), Rad%, DrawClr% 'No arc, no elipse

        Circ.Done: Rad% = 0: Arc% = 0: Elipse = 0: c$ = "": DrawClr% = 0: GOTO Parse.Done
    ELSEIF L$ = "SLEEP" THEN
        SLEEP
    ELSEIF LEFT$(L$, 6) = "SLEEP " AND GetVal(MID$(L$, 7)) > 0 THEN
        SLEEP GetVal(MID$(L$, 7))
    ELSEIF LEFT$(L$, 6) = "PRINT " THEN
        DIM retainCursor AS _BYTE
        IF RIGHT$(L$, 1) = ";" THEN
            retainCursor = true
            L$ = LEFT$(L$, LEN(L$) - 1)
            L1$ = LEFT$(L1$, LEN(L$))
        ELSE
            retainCursor = false
        END IF

        q1 = INSTR(7, L1$, CHR$(34))
        IF q1 THEN
            q2 = INSTR(q1 + 1, L1$, CHR$(34))
            IF q2 THEN
                PRINT MID$(L1$, 8, q2 - q1 - 1);
                IF NOT retainCursor THEN PRINT
            ELSE
                GOTO syntaxerror
            END IF
        ELSE
            varIndex = searchVar(MID$(L1$, 7))
            IF varIndex THEN
                IF vars(varIndex).type = varTypeSTRING THEN
                    PRINT strings(varIndex);
                ELSE
                    PRINT nums(varIndex);
                END IF
                IF NOT retainCursor THEN PRINT
            ELSE
                DIM t$
                t$ = doMath(MID$(L1$, 7))
                IF LEN(t$) THEN PRINT t$; ELSE PRINT GetVal(MID$(L1$, 7));
                IF NOT retainCursor THEN PRINT
            END IF
        END IF
    ELSEIF L$ = "PRINT" THEN
        PRINT
    ELSEIF LEFT$(L$, 7) = "_TITLE " THEN
        q1 = INSTR(8, L1$, CHR$(34))
        IF q1 THEN
            q2 = INSTR(q1 + 1, L1$, CHR$(34))
            IF q2 THEN
                _TITLE MID$(L1$, 9, q2 - q1 - 1)
            ELSE
                GOTO syntaxerror
            END IF
        ELSE
            varIndex = searchVar(MID$(L1$, 8))
            IF varIndex THEN
                IF vars(varIndex).type = varTypeSTRING THEN
                    _TITLE strings(varIndex)
                ELSE
                    _TITLE STR$(nums(varIndex))
                END IF
            ELSE
                _TITLE MID$(L1$, 8)
            END IF
        END IF
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
        doLine = currentLine
    ELSEIF L$ = "LOOP" THEN
        loopLine = currentLine
        IF doLine > 0 THEN currentLine = doLine
    ELSEIF L$ = "EXIT DO" THEN
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
    ELSEIF LEFT$(L$, 7) = "SCREEN " THEN
        SELECT CASE VAL(MID$(L$, 8))
            CASE 0 TO 2, 7 TO 13
                s% = VAL(MID$(L$, 8))
                SCREEN s%
            CASE ELSE
                PRINT "Invalid mode."
        END SELECT
    ELSEIF LEFT$(L$, 6) = "SCREEN" THEN
        PRINT s%

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
                    IF GetVal(i1$) = GetVal(i2$) THEN r = true
                CASE ">"
                    IF GetVal(i1$) > GetVal(i2$) THEN r = true
                CASE "<"
                    IF GetVal(i1$) < GetVal(i2$) THEN r = true
            END SELECT

            IF NOT r THEN
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

        IF vars(varIndex).type = varTypeSTRING THEN
            strings(varIndex) = RTRIM$(LTRIM$(MID$(L1$, INSTR(L1$, "=") + 1)))
            IF LEFT$(strings(varIndex), 1) = CHR$(34) THEN
                strings(varIndex) = MID$(strings(varIndex), 2)
                IF RIGHT$(strings(varIndex), 1) = CHR$(34) THEN strings(varIndex) = LEFT$(strings(varIndex), LEN(strings(varIndex)) - 1)
            ELSE
                DIM varIndex2 AS _UNSIGNED LONG
                varIndex2 = searchVar(strings(varIndex))
                IF varIndex2 THEN
                    strings(varIndex) = strings(varIndex2)
                END IF
            END IF
        ELSE
            DIM v$, v1$, v2$
            v$ = MID$(L1$, INSTR(L1$, "=") + 1)

            t$ = doMath(v$)

            IF t$ = "" THEN
                IF vars(varIndex).type = varTypeINTEGER THEN
                    nums(varIndex) = INT(GetVal(v$))
                ELSE
                    nums(varIndex) = GetVal(v$)
                END IF
            ELSE
                nums(varIndex) = GetVal(t$)

                IF vars(varIndex).type = varTypeINTEGER THEN
                    nums(varIndex) = INT(nums(varIndex))
                END IF
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
    IF RIGHT$(varName$, 1) = "$" THEN vars(totalVars).type = varTypeSTRING

    IF RIGHT$(varName$, 3) = "~%%" THEN
        vars(totalVars).type = varType_UBYTE
    ELSEIF RIGHT$(varName$, 2) = "%%" THEN
        vars(totalVars).type = varType_BYTE
    ELSEIF RIGHT$(varName$, 2) = "~%" THEN
        vars(totalVars).type = varType_UINTEGER
    ELSEIF RIGHT$(varName$, 1) = "%" THEN
        vars(totalVars).type = varTypeINTEGER
    END IF

    IF RIGHT$(varName$, 1) = "!" THEN
        vars(totalVars).type = varTypeSINGLE
    ELSEIF RIGHT$(varName$, 2) = "##" THEN
        vars(totalVars).type = varType_FLOAT
    ELSEIF RIGHT$(varName$, 1) = "#" THEN
        vars(totalVars).type = varTypeDOUBLE
    END IF

    IF RIGHT$(varName$, 3) = "~&&" THEN
        vars(totalVars).type = varType_UINTEGER64
    ELSEIF RIGHT$(varName$, 2) = "&&" THEN
        vars(totalVars).type = varType_INTEGER64
    ELSEIF RIGHT$(varName$, 2) = "~&" THEN
        vars(totalVars).type = varType_ULONG
    ELSEIF RIGHT$(varName$, 1) = "&" THEN
        vars(totalVars).type = varType_LONG
    END IF
    '--------------------------------------------------------------------------

    vars(totalVars).scope = thisScope$
    addVar~& = totalVars
END FUNCTION

FUNCTION searchVar~& (varName$)
    DIM i AS _UNSIGNED LONG, found AS _BYTE
    DIM temp##, temp$, special AS _BYTE

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

FUNCTION GetVal## (__c$)
    DIM c$

    IF hasOperator(__c$) THEN
        c$ = LEFT$(__c$, firstOperator(__c$) - 1)
    ELSE
        c$ = __c$
    END IF

    IF VAL(c$) > 0 THEN
        GetVal## = VAL(c$)
    ELSE
        varIndex = searchVar(c$)
        IF varIndex THEN
            IF vars(varIndex).type = varTypeSTRING THEN
                GetVal## = VAL(strings(varIndex)) 'auto conversion
            ELSE
                GetVal## = nums(varIndex)
            END IF
        ELSE
            GetVal## = VAL(c$) 'maybe it was 0 anyway...
        END IF
    END IF
END FUNCTION

FUNCTION doMath$ (v$)
    DIM v1$, v2$, temp$, continue AS _BYTE

    IF debugging THEN PRINT "doing math on "; v$

    IF hasOperator(v$) THEN
        s$ = MID$(v$, firstOperator(v$), 1)
    ELSE
        doMath$ = v$
        EXIT FUNCTION
    END IF

    v1$ = LEFT$(v$, INSTR(v$, s$) - 1)
    v2$ = MID$(v$, firstOperator(v$) + 1)

    IF debugging THEN
        PRINT "found    "; v1$, s$, v2$
        PRINT "getval():"; GetVal(v1$), GetVal(v2$)
    END IF

    SELECT CASE s$
        CASE "+"
            temp$ = STR$(GetVal(v1$) + GetVal(v2$))
        CASE "-"
            temp$ = STR$(GetVal(v1$) - GetVal(v2$))
        CASE "*"
            temp$ = STR$(GetVal(v1$) * GetVal(v2$))
        CASE "/"
            temp$ = STR$(GetVal(v1$) / GetVal(v2$))
    END SELECT

    IF debugging THEN PRINT "temp$="; temp$

    DO WHILE hasOperator(v2$)
        s$ = MID$(v2$, firstOperator(v2$), 1)
        v2$ = MID$(v2$, firstOperator(v2$) + 1)
        IF debugging THEN PRINT "passing "; temp$ + s$ + v2$
        temp$ = doMath$(temp$ + s$ + v2$)
        v2$ = MID$(v2$, firstOperator(v2$) + LEN(v2$))
    LOOP

    doMath$ = LTRIM$(RTRIM$(temp$))
END FUNCTION

FUNCTION hasOperator%% (v$)
    hasOperator%% = INSTR(v$, "+") > 0 OR INSTR(v$, "-") > 0 OR INSTR(v$, "*") > 0 OR INSTR(v$, "/") > 0
END FUNCTION

FUNCTION firstOperator& (v$)
    DIM i AS LONG, op$

    op$ = "+-*/"

    FOR i = 1 TO LEN(v$)
        IF INSTR(op$, MID$(v$, i, 1)) THEN
            firstOperator = i
            EXIT FUNCTION
        END IF
    NEXT
END FUNCTION
