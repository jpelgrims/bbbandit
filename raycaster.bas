REM $DYNAMIC

DECLARE create_world (width AS INTEGER, height AS INTEGER, world() AS INTEGER)
DECLARE draw_vertical_line (x AS INTEGER, drawStart AS INTEGER, drawEnd AS INTEGER, screen_height AS INTEGER, colour AS INTEGER)

DIM SHARED map_width AS INTEGER
DIM SHARED map_height AS INTEGER

DIM posX AS DOUBLE
DIM posY AS DOUBLE

DIM dirX AS DOUBLE
DIM dirY AS DOUBLE

DIM planeX AS DOUBLE
DIM planeY AS DOUBLE

DIM running AS INTEGER
DIM startTime AS DOUBLE

DIM moveSpeed AS DOUBLE
DIM rotSpeed AS DOUBLE
moveSpeed = 0.01
rotSpeed# = 0.01


running = 1
startTime = TIMER

screen_width = 320
screen_height = 200

DIM world(1, 1) AS INTEGER


posX = 3: posY = 3
dirX = -1: dirY = 0
planeX = 0: planeY = 0.66

'CALL create_world(24, 24, world())
CALL load_map("map.txt", world())
CALL screen_setup(screen_width, screen_height)

WHILE running = 1

    FOR x = 0 TO screen_width - 1
        DIM cameraX AS DOUBLE
        DIM rayDirX AS DOUBLE
        DIM rayDirY AS DOUBLE

        cameraX = 2 * x / screen_width - 1

        rayDirX = dirX + planeX * cameraX
        rayDirY = dirY + planeY * cameraX

        DIM mapX AS INTEGER
        DIM mapY AS INTEGER

        mapX = INT(posX)
        mapY = INT(posY)

        DIM sideDistX AS DOUBLE
        DIM sideDistY AS DOUBLE

        DIM deltaDistX AS DOUBLE
        DIM deltaDistY AS DOUBLE

        deltaDistX = ABS(1 / rayDirX)
        deltaDistY = ABS(1 / rayDirY)

        DIM perpWallDist AS DOUBLE

        DIM stepX AS INTEGER
        DIM stepY AS INTEGER


        DIM hit AS INTEGER
        DIM side AS INTEGER
        hit = 0

        ' Calculate step and initial sideDist
        IF (rayDirX < 0) THEN
            stepX = -1
            sideDistX = (posX - mapX) * deltaDistX
        ELSE
            stepX = 1
            sideDistX = (mapX + 1.0 - posX) * deltaDistX
        END IF

        IF (rayDirY < 0) THEN
            stepY = -1
            sideDistY = (posY - mapY) * deltaDistY
        ELSE
            stepY = 1
            sideDistY = (mapY + 1.0 - posY) * deltaDistY
        END IF

        ' Do DDA
        WHILE hit = 0
            IF sideDistX < sideDistY THEN
                sideDistX = sideDistX + deltaDistX
                mapX = mapX + stepX
                side = 0
            ELSE
                sideDistY = sideDistY + deltaDistY
                mapY = mapY + stepY
                side = 1
            END IF

            ' Check if ray didn't leave map
            IF mapX = map_width - 1 THEN
                hit = 1
            ELSEIF mapY = map_height - 1 THEN
                hit = 1
                ' Check for ray hitting a wall
            ELSEIF world(mapX, mapY) > 0 THEN
                hit = 1
            END IF
        WEND

        ' Calculate distance of perpendicualr ray (no euclidean distance to avoid fisheye effect)
        IF side = 0 THEN
            perpWallDist = (mapX - posX + (1 - stepX) / 2) / rayDirX
        ELSE
            perpWallDist = (mapY - posY + (1 - stepY) / 2) / rayDirY
        END IF


        DIM lineHeight AS INTEGER
        ' Calculate height of line to draw on screen
        lineHeight = screen_height / perpWallDist

        ' Calculate lowest and highest pixel to fill in current stripe
        DIM drawStart AS INTEGER

        drawStart = -lineHeight / 2 + screen_height / 2

        IF drawStart < 0 THEN
            drawStart = 0
        END IF

        DIM drawEnd AS INTEGER
        drawEnd = lineHeight / 2 + screen_height / 2
        IF drawEnd >= screen_height THEN
            drawEnd = screen_height - 1
        END IF

        DIM colour AS INTEGER
        SELECT CASE world(mapX, mapY)
            CASE 1
                colour = 7
            CASE ELSE
                colour = 2
        END SELECT

        ' different brightness for x and y side
        IF side = 1 THEN
            colour = 8
        END IF

        CALL draw_vertical_line(x, drawStart, drawEnd, screen_height, colour)
    NEXT

    PCOPY 1, 0

    IF IS_PRESSED("UP") THEN
        posX = posX + dirX * moveSpeed
        posY = posY + dirY * moveSpeed
    END IF

    IF IS_PRESSED("DOWN") THEN
        posX = posX - dirX * moveSpeed
        posY = posY - dirY * moveSpeed
    END IF

    IF IS_PRESSED("RIGHT") THEN
        posX = posX + planeX * moveSpeed
        posY = posY + planeY * moveSpeed
    END IF

    IF IS_PRESSED("LEFT") THEN
        posX = posX - planeX * moveSpeed
        posY = posY - planeY * moveSpeed
    END IF

    IF IS_PRESSED("ESCAPE") OR IS_PRESSED("Q") THEN
        SYSTEM
    END IF

    ' Mouse viewing using QB64 features
    _MOUSEHIDE
    DO WHILE _MOUSEINPUT
        m = 4
        IF _MOUSEMOVEMENTX < 0 THEN
            oldDirX = dirX
            dirX = dirX * COS(rotSpeed * m) - dirY * SIN(rotSpeed * m)
            dirY = oldDirX * SIN(rotSpeed * m) + dirY * COS(rotSpeed * m)
            oldPlaneX = planeX
            planeX = planeX * COS(rotdSpeed * m) - planeY * SIN(rotSpeed * m)
            planeY = oldPlaneX * SIN(rotSpeed * m) + planeY * COS(rotSpeed * m)
        ELSEIF _MOUSEMOVEMENTX > 0 THEN
            oldDirX = dirX
            dirX = dirX * COS(-rotSpeed * m) - dirY * SIN(-rotSpeed * m)
            dirY = oldDirX * SIN(-rotSpeed * m) + dirY * COS(-rotSpeed * m)
            oldPlaneX = planeX
            planeX = planeX * COS(-rotSpeed * m) - planeY * SIN(-rotSpeed * m)
            planeY = oldPlaneX * SIN(-rotSpeed * m) + planeY * COS(-rotSpeed * m)
        END IF

    LOOP
    _MOUSEMOVE screen_width / 2, screen_height / 2 ' Lock mouse pointer to application by moving it to the middle of the screen

    endTime# = TIMER(.001)

    'DO WHILE endTime# - startTime < (1 / 60)
    '    endTime# = TIMER(.001)
    'LOOP

    ' It might be that SLEEP doesn't allow more than 1/18th of a second, check this out later
    frameTime# = endTime# - startTime
    IF frameTime# < (1 / 60) THEN
        SLEEP (1 / 60) - frameTime#
    END IF

WEND


SUB draw_vertical_line (x AS INTEGER, drawStart AS INTEGER, drawEnd AS INTEGER, screen_height AS INTEGER, colour AS INTEGER)
    LINE (x, 0)-(x, drawStart), 9
    LINE (x, drawStart)-(x, drawEnd), colour
    LINE (x, drawEnd)-(x, screen_height), 6
END SUB


SUB create_world (map_width AS INTEGER, map_height AS INTEGER, world() AS INTEGER)
    ' create a world, store it in a multimdimensional array and return it

    REDIM world(map_width - 1, map_height - 1) AS INTEGER

    FOR y = 0 TO map_height - 1
        FOR x = 0 TO map_width - 1
            world(x, y) = 0
        NEXT
    NEXT

    FOR i = 0 TO map_height - 1
        world(i, 0) = 1
        world(i, map_width - 1) = 1
    NEXT

    FOR i = 0 TO map_width - 1
        world(0, i) = 1
        world(map_height - 1, i) = 1
    NEXT

    world(17, 10) = 1
    world(16, 10) = 1
    world(15, 10) = 1

END SUB


SUB screen_setup (screen_width AS INTEGER, screen_height AS INTEGER)
    SCREEN 7, 0, 1, 0
    _FULLSCREEN 'QB64 full screen mode, also works with alt+enter
END SUB


FUNCTION IS_PRESSED (key_name AS STRING)
    DIM key_code AS INTEGER

    SELECT CASE key_name
        CASE "UP": key_code = 72
        CASE "DOWN": key_code = 80
        CASE "LEFT": key_code = 75
        CASE "RIGHT": key_code = 77
        CASE "ESCAPE": key_code = 1
        CASE "Q": key_code = 16
        CASE ELSE: key_code = 0
    END SELECT

    '---------
    ' This piece of code is an improvement (by Joe Huber) of the original multikey demo code written by Eric Carr
    '---------

    STATIC FIRST_TIME, KEYS(), SC(), DU()

    IF FIRST_TIME = 0 THEN
        DIM KEYS(255), SC(255), DU(255)
        FOR E = 0 TO 127
            SC(E) = E: DU(E) = 1
        NEXT
        FOR E = 128 TO 255
            SC(E) = E - 128: DU(E) = 0
        NEXT
        FIRST_TIME = -1
    END IF

    I$ = INKEY$ ' So the keyb buffer don't get full     \routine/ \
    I = INP(&H60) ' Get keyboard scan code from port 60h   \lines/
    OUT &H61, INP(&H61) OR &H82: OUT &H20, &H20 '         \!!!/   |
    KEYS(SC(I)) = DU(I) ' This says what keys are pressed        \!

    '---------

    IS_PRESSED = KEYS(key_code)

END FUNCTION

SUB load_map (filename AS STRING, world() AS INTEGER)
    OPEN filename FOR INPUT AS 1
    IF errorflag <> 0 THEN
        errorflag = 0
        CLOSE
        PRINT "File not found"
        SLEEP 2
        END
    END IF

    ' Search the map dimensions
    map_height = 0
    map_width = 0

    DO WHILE NOT EOF(1)
        LINE INPUT #1, l$

        IF l$ <> "" THEN
            map_height = map_height + 1
        END IF


        IF map_width = 0 AND LEN(l$) > 0 THEN
            map_width = LEN(l$)
        END IF
    LOOP

    REDIM world(map_width - 1, map_height - 1) AS INTEGER
    DIM text_line AS STRING

    SEEK #1, 1 ' Rewind cursor to beginning of file
    FOR i = 0 TO map_height - 1
        LINE INPUT #1, text_line
        FOR letter = 1 TO map_width
            world(letter - 1, i) = VAL(MID$(text_line, letter, 1))
            PRINT letter - 1, i, VAL(MID$(text_line, letter, 1))
        NEXT
    NEXT

END SUB
