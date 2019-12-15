OPTION _EXPLICIT
CONST true = -1, false = 0
CONST debugging = true

$CONSOLE

_SCREENMOVE _SCREENX + 800, _SCREENY

DIM l$
DO
    LINE INPUT l$
    _ECHO "--------- new call ---------"
    PRINT Parse(l$)
LOOP

FUNCTION Parse$ (__inputExpr AS STRING)
    'Adapted from https://www.codeproject.com/Articles/1205435/Parsing-Mathematical-Expressions-in-VB-NET-Missi
    ' Call this routine to perform the actual mathematic expression parsing
    ' Comments retained from the original code.
    DIM t AS _UNSIGNED LONG, index AS _UNSIGNED LONG
    DIM totalStrings AS _UNSIGNED LONG
    DIM inputExpr AS STRING
    REDIM oe(0) AS _UNSIGNED LONG
    REDIM strings(0) AS STRING

    inputExpr = "(" + __inputExpr + ")"

    t = 1
    ' Iterate through the characters of input string starting at the position of final character
    FOR index = LEN(inputExpr) - 1 TO 0 STEP -1
        ' For each character perform a check if its value is '('
        IF ASC(inputExpr, index + 1) = 40 OR index = 0 THEN
            DIM sb AS STRING
            sb = ""
            DIM n AS _UNSIGNED LONG
            ' Perform a check if this is the first character in string
            IF index = 0 THEN
                ' If so assign n variable to the value of variable index
                n = 1
                IF debugging THEN _ECHO "Beginning of expression reached; n =" + STR$(n)
            ELSE
                ' Otherwise assign n variable to the value of variable index + 1
                n = index + 1
                IF debugging THEN _ECHO "'(' found at" + STR$(index) + "; n =" + STR$(n)
            END IF

            DIM exists AS _BYTE
            DO
                exists = false
                DIM bracket AS _BYTE
                bracket = false
                ' Perform the iterations stepping forward into each succeeding character
                ' starting at the position n = index + 1 until we've found a character equal to ')'
                WHILE n < LEN(inputExpr) AND bracket = false
                    ' Check if the current character is not ')'.
                    IF ASC(inputExpr, n + 1) <> 41 THEN
                        ' If so, append it to the temporary string buffer
                        sb = sb + MID$(inputExpr, n + 1, 1)
                        ' Otherwise break the loop execution
                    ELSE
                        bracket = true
                    END IF
                    ' Increment the n loop counter variable by 1
                    n = n + 1
                WEND
                DIM r AS _UNSIGNED LONG
                r = 0
                ' Iterate through the array of positions
                WHILE r <= UBOUND(oe) AND exists = false
                    ' For each element perform a check if its value
                    ' is equal to the position of the current ')' character
                    IF oe(r) = n THEN
                        ' If so, append the character ')' to the temporary string buffer and break
                        ' the loop execution assigning the variable exists to the value 'true'
                        exists = true
                        sb = sb + ") "
                        'n = n + 1
                    END IF
                    r = r + 1
                WEND

                ' Repeat the following loop execution until we've found the character ')' at
                ' the New position which is not in the array of positions
            LOOP WHILE exists = true

            ' If the current character's ')' position has not been previous found,
            ' add the value of position to the array
            IF exists = false THEN
                REDIM _PRESERVE oe(UBOUND(oe) + 1)
                oe(t) = n
                t = t + 1
            END IF

            ' Add the currently obtained string containing a specific part of the expression to the array
            totalStrings = totalStrings + 1
            REDIM _PRESERVE strings(totalStrings)
            strings(totalStrings) = sb
            IF debugging THEN _ECHO "Substring stored: " + sb
        END IF
    NEXT

    ' Iterate through the array of the expression parts
    FOR index = 1 TO totalStrings
        ' Compute the result for the current part of the expression
        DIM Result AS STRING
        IF debugging THEN _ECHO "Computing: " + strings(index)
        Result = STR$(Compute(strings(index)))

        ' Iterate through all succeeding parts of the expression
        FOR n = index TO totalStrings
            ' For each part substitute the substring containing the current part of the expression
            ' with its numerical value without parentheses.
            IF debugging THEN _ECHO "Passing substring to Replace(): " + strings(n)
            strings(n) = Replace(strings(n), "(" + strings(index) + ")", Result, 0, 0)
            IF debugging THEN _ECHO "           Result of Replace(): " + strings(n)
        NEXT
    NEXT
    ' Compute the numerical value of the last part (e.g. the numerical resulting value of the entire expression)
    ' and return this value at the end of the following routine execution.
    Parse$ = STR$(Compute(strings(totalStrings)))
END FUNCTION

FUNCTION Compute## (expr AS STRING)
    DIM i AS _UNSIGNED LONG, j AS _UNSIGNED LONG
    DIM l AS _UNSIGNED LONG, m AS _UNSIGNED LONG, lastIndex AS _UNSIGNED LONG
    DIM totalElements AS _UNSIGNED LONG
    DIM ch AS STRING, hasOperator%%
    DIM tempElement AS STRING, op1##, op2##, result##
    REDIM element(1000) AS STRING
    STATIC op(6) AS STRING, validOP$

    validOP$ = "^*/+-="
    FOR i = 1 TO LEN(validOP$)
        op(i) = MID$(validOP$, i, 1)
    NEXT

    IF debugging THEN _ECHO "* Entering Compute##(): " + expr

    'break down expr into element()
    FOR i = 1 TO LEN(expr)
        ch = MID$(expr, i, 1)
        IF INSTR(validOP$, ch) THEN
            'this is an operator
            IF LEN(tempElement) THEN GOSUB addElement
            tempElement = ch
            GOSUB addElement
            tempElement = ""
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
                DO UNTIL LEN(_TRIM$(element(j - l))) > 0 AND INSTR(validOP$, element(j - l)) = 0
                    l = l + 1
                    IF j - l < 1 THEN EXIT FUNCTION
                LOOP
                op1## = VAL(element(j - l))
                IF debugging THEN _ECHO "element(j - l) = " + element(j - l)
                m = 1
                DO UNTIL LEN(_TRIM$(element(j + m))) > 0 AND INSTR(validOP$, element(j + m)) = 0
                    m = m + 1
                    IF j + m > totalElements THEN EXIT FUNCTION
                LOOP
                op2## = VAL(element(j + m))
                IF debugging THEN _ECHO "element(j + m) = " + element(j + m)
                IF debugging THEN _ECHO "op1=" + STR$(op1##) + "; oper=" + op(i) + "; op2=" + STR$(op2##)
                SELECT CASE op(i)
                    CASE "^"
                        result## = op1## ^ op2##
                    CASE "*"
                        result## = op1## * op2##
                    CASE "/"
                        result## = op1## / op2##
                    CASE "+"
                        result## = op1## + op2##
                    CASE "-"
                        result## = op1## - op2##
                    CASE "="
                        result## = op1## = op2##
                END SELECT
                IF debugging THEN _ECHO "temp result## =" + STR$(result##)
                element(j - l) = ""
                element(j + m) = ""
                element(j) = STR$(result##)
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
        Compute## = VAL(expr)
    ELSE
        Compute## = VAL(element(lastIndex))
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
