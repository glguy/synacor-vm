# Advent of Code 2015 - Day 1
SET A welcome_msg
CALL printstring

CALL getline # loads linebuffer, sets A
SET B A # save line

CALL part1 # returns count in A
CALL printnumber
OUT '\n'

HALT

#####################################################
# part 1, find the difference in the count of ( and )
#
# Arguments: A, pointer to zero-terminated string
# Returns:   A, count of '(' minus count of ')'
part1:            PUSH B # accumulator
                  PUSH C # conditional
                  PUSH D # character
                  SET B 0

part1_loop:       RMEM D A
                  JT D part1_check_up # check for \0
                  SET A B
                  POP D
                  POP C
                  POP B
                  RET

part1_check_up:   ADD A A 1
                  EQ C D '('
                  JF C part1_check_down
                  ADD B B 1
                  JMP part1_loop

part1_check_down: EQ C D ')'
                  JF C part1_loop
                  ADD B B -1
                  JMP part1_loop

### END OF part1 #######################################


### printstring ########################################
# Print a zero-terminated string stored in memory at <A>
# On return <A> will point to the null terminator.
#
# Arguments: A, pointer to zero-terminated string
# Results:   None
printstring:            PUSH A
                        PUSH B # character buffer

printstringloop:        RMEM B A
                        JF B printstringdone
                        OUT B
                        ADD A A 1
                        JMP printstringloop

printstringdone:        POP B
                        POP A
                        RET
###END OF printstring####



### printnumber ########################################
# Print a number in signed, decimal notation
#
# Arguments: A, number to print
# Results:   None
printnumber:            PUSH A # Number to print
                        PUSH B # Temporary condition
                        PUSH C # Output character

                        GT B A 0x3fff # test for negative
                        JF B printnumber_positive

                        OUT '-'
                        MULT A A -1


printnumber_positive:   GT B    10 A
                        JT B printnumber_1
                        GT B   100 A
                        JT B printnumber_10
                        GT B  1000 A
                        JT B printnumber_100
                        GT B 10000 A
                        JT B printnumber_1000

printnumber_10000:      SET C '0'
printnumber_10000_loop: GT B 10000 A
                        JT B printnumber_10000_done
                        ADD A A -10000
                        ADD C C 1
                        JMP printnumber_10000_loop
printnumber_10000_done: OUT C

printnumber_1000:       SET C '0'
printnumber_1000_loop:  GT B 1000 A
                        JT B printnumber_1000_done
                        ADD A A -1000
                        ADD C C 1
                        JMP printnumber_1000_loop
printnumber_1000_done:  OUT C

printnumber_100:        SET C '0'
printnumber_100_loop:   GT B 100 A
                        JT B printnumber_100_done
                        ADD A A -100
                        ADD C C 1
                        JMP printnumber_100_loop
printnumber_100_done:   OUT C

printnumber_10:         SET C '0'
printnumber_10_loop:    GT B 10 A
                        JT B printnumber_10_done
                        ADD A A -10
                        ADD C C 1
                        JMP printnumber_10_loop
printnumber_10_done:    OUT C

printnumber_1:          ADD C A '0'
                        OUT C

POP C
POP B
POP A
RET
### END OF printnumber #################################




### getline ############################################
# Get a line of text and store it in linebuffer
#
# Note: As expected this uses a static line buffer.
#
# Returns: A, pointer to zero-terminated line buffer
getline:
PUSH B # next input char
PUSH C # used for conditions
SET A linebuffer

getlineloop:
IN B
EQ C B '\n' # check for newline
JT C endlinedone
WMEM A B  # store character in buffer
ADD A A 1 # advance buffer pointer
JMP getlineloop

endlinedone:
WMEM A 0 # write null terminator
SET A linebuffer
POP C
POP B
RET

### END OF getline #####################################


### DATA ###############################################
welcome_msg: "Advent 2015 - Day 1\n"
             "Please input parentheses to stdin on a single line.\n\0"
linebuffer:
