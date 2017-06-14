CLS
COLOR 3, 1
PRINT "Hello!"
COLOR 15, 6
PRINT "It's me!"
COLOR 7, 0
PRINT "What's your name?"
INPUT name$
PRINT "Enter a color for the text:"
INPUT fg
PRINT "Enter a color for the bg:"
INPUT bg
COLOR fg, bg
PRINT
PRINT "Listen,"
PRINT name$
PRINT "..."
PRINT "I'll sleep for 2 seconds..."
SLEEP 2
COLOR 7, 0
END
