'adapted from https://www.qb64.org/forum/index.php?topic=1642.msg108603#msg108603
'by STxAxTIC

' Define fundamental structures.
TYPE Vector
    X AS INTEGER
    Y AS INTEGER
END TYPE

TYPE Cell
    Identity AS LONG
    Pointer AS LONG
    Lagger AS LONG
    Content AS STRING * 1
END TYPE

DIM SHARED ChainLimit AS LONG
DIM SHARED BOC AS LONG ' Beginning of chain.
DIM SHARED EOC AS LONG ' End of chain.
ChainLimit = 128000
BOC = -1
EOC = ChainLimit

' Define text window properties.
DIM SHARED WindowHeight AS LONG
DIM SHARED WindowWidth AS LONG
DIM SHARED VisibleLines AS LONG
DIM SHARED TopIndent AS LONG
DIM SHARED LeftIndent AS LONG
DIM SHARED TextHeight AS LONG
DIM SHARED TextWidth AS LONG
DIM SHARED HScroll AS LONG
DIM SHARED TextWrapping AS LONG
DIM SHARED TextFormatting AS LONG
DIM SHARED InsertKey AS LONG

TopIndent = 1
LeftIndent = 1
WindowWidth = 90
WindowHeight = 30
TextHeight = WindowHeight - 2 * TopIndent
TextWidth = WindowWidth - 2 * LeftIndent
HScroll = 1
TextWrapping = 2 'none
TextFormatting = 1
InsertKey = -1

' Initiate text inside window.
DIM SHARED StartIndex AS LONG
DIM SHARED LineAsMapped(TextHeight) AS STRING
DIM SHARED Cursor1 AS Vector
DIM SHARED Cursor2 AS Vector
DIM SHARED ID1 AS LONG
DIM SHARED ID2 AS LONG
DIM SHARED ExitEditor AS _BYTE

'' Auxiliary 2D text grid.
'DIM SHARED GOLSwitch
'DIM SHARED AuxGrid(TextWidth, TextHeight, 2) AS STRING
'GOLSwitch = -1

' Create memory space for string.
DIM SHARED TheChain(ChainLimit) AS Cell

DIM SHARED DEBUG$
