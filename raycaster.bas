REM $DYNAMIC

DECLARE create_world (width AS INTEGER, height AS INTEGER, world() AS INTEGER)
DECLARE draw_vertical_line (x AS INTEGER, drawStart AS INTEGER, drawEnd AS INTEGER, screen_height AS INTEGER, colour AS INTEGER)
DECLARE add_color (red AS INTEGER, green AS INTEGER, blue AS INTEGER)
DECLARE load_palette

DIM SHARED map_width AS INTEGER
DIM SHARED map_height AS INTEGER
DIM SHARED colorPalette(255) AS PaletteColor
DIM SHARED nrOfColors AS INTEGER

DIM posX AS DOUBLE
DIM posY AS DOUBLE

DIM dirX AS DOUBLE
DIM dirY AS DOUBLE

DIM planeX AS DOUBLE
DIM planeY AS DOUBLE

DIM running AS INTEGER
DIM startTime AS DOUBLE

DIM textureWidth AS INTEGER
DIM textureHeight AS INTEGER

textureWidth = 32
textureHeight = 32

DIM moveSpeed AS DOUBLE
DIM rotSpeed AS DOUBLE
moveSpeed = 0.05
rotSpeed# = 0.01


running = 1
startTime = TIMER

screen_width = 320
screen_height = 200

DIM buffer(screen_height, screen_width) AS INTEGER
DIM world(1, 1) AS INTEGER
DIM texture1(32, 32) AS INTEGER
DIM texture2(32, 32) AS INTEGER

CALL screen_setup(screen_width, screen_height)
CALL load_image("test.bmp", texture1())
CALL load_image("test2.bmp", texture2())

CALL load_palette

posX = 3: posY = 3
dirX = -1: dirY = 0
planeX = 0: planeY = 0.66

'CALL create_world(24, 24, world())
CALL load_map("map.txt", world())


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

        ' Texturing calculations

        DIM textureNr AS INTEGER
        DIM wallX AS DOUBLE 'Where wall was hit
        textureNr = world(mapX, mapY) - 1

        IF side = 0 THEN
            wallX = posY + perpWallDist * rayDirY
        ELSE
            wallX = posX + perpWallDist * rayDirX
        END IF

        wallX = wallX - INT(wallX)

        DIM texX AS INTEGER
        DIM texY AS INTEGER
        texX = INT(wallX * textureWidth)

        IF side = 0 AND rayDir > 0 THEN
            texX = textureWidth - texX - 1
        END IF

        IF side = 1 AND rayDirY < 0 THEN
            texX = textureWidth - texX - 1
        END IF

        DIM colour AS INTEGER

        FOR y = 0 TO drawStart - 1
            buffer(y, x) = 3
        NEXT

        FOR y = drawStart TO screen_height - 1
            buffer(y, x) = 4
        NEXT


        DIM height AS INTEGER
        height = 32
        IF lineHeight < 32 THEN
            height = lineHeight
        END IF

        FOR y = drawStart TO drawEnd - 1
            d% = y - screen_height * 0.5 + lineHeight * 0.5
            texY = ((d% * textureHeight) / lineHeight)


            IF textureNr = 0 THEN
                colour = texture1(texX, texY)
            ELSE
                colour = texture2(texX, texY)
            END IF

            IF side = 1 THEN
                colour = colour * 1.5
            END IF
            buffer(y, x) = colour
        NEXT

    NEXT

    'CALL draw_buffer(buffer())
    FOR x = 0 TO screen_width - 1
        FOR y = 0 TO screen_height - 1
            PSET (x, y), buffer(y, x)
        NEXT
    NEXT

    PCOPY 1, 0

    ' Clear buffer
    FOR x = 0 TO screen_width - 1
        FOR y = 0 TO screen_height - 1
            buffer(y, x) = 0
        NEXT
    NEXT




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
    '_MOUSEMOVE screen_width / 2, screen_height / 2 ' Lock mouse pointer to application by moving it to the middle of the screen

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


SUB draw_buffer (buffer() AS INTEGER)
    FOR x = 0 TO screen_width - 1
        FOR y = 0 TO screen_height - 1
            'PSET (x, y), buffer(y, x)
            PSET (x, y)
        NEXT
    NEXT
END SUB


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
    'SCREEN 0
    SCREEN 13, 0, 1, 0
    '_FULLSCREEN 'QB64 full screen mode, also works with alt+enter
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
        NEXT
    NEXT

END SUB


TYPE BMPEntry ' Description
    ID AS STRING * 2 ' File ID("BM" text or 19778 AS Integer)
    Size AS LONG ' Total Size of the file
    Res1 AS INTEGER ' Reserved 1 always
    Res2 AS INTEGER ' Reserved 2 always
    Offset AS LONG ' Start offset of image pixel data
END TYPE


TYPE BMPHeader 'BMP header also used in Icon and Cursor files(.ICO and .CUR)
    Hsize AS LONG ' Info header size (always 40)
    PWidth AS LONG ' Image width
    PDepth AS LONG ' Image height (doubled in icons)
    Planes AS INTEGER ' Number of planes (normally 1)
    BPP AS INTEGER ' Bits per pixel(palette 1, 4, 8, 24)
    Compression AS LONG ' Compression type(normally 0)
    ImageBytes AS LONG ' (Width + padder) * Height
    Xres AS LONG ' Width in PELS per metre(normally 0)
    Yres AS LONG ' Depth in PELS per metre(normally 0)
    NumColors AS LONG ' Number of Colors(normally 0)
    SigColors AS LONG ' Significant Colors(normally 0)
    Pal AS STRING * 1024 'Stored as &amp;lt;Blue, Green, Red, 0&amp;gt;
END TYPE


TYPE PaletteColor
    Red AS INTEGER
    Green AS INTEGER
    Blue AS INTEGER
END TYPE

' Function that stores a color and returns the color code
FUNCTION add_color (red AS INTEGER, green AS INTEGER, blue AS INTEGER)
    DIM newColor AS PaletteColor
    newColor.Red = red
    newColor.Green = green
    newColor.Blue = blue

    colorCode% = 0
    stored% = 0
    FOR c = 0 TO nrOfColors - 1

        IF colorPalette(c).Red = newColor.Red AND colorPalette(c).Green = newColor.Green AND colorPalette(c).Blue = newColor.Blue THEN
            ' Color is already stored in palette
            stored% = 1
            colorCode% = c
        END IF
    NEXT

    IF stored% = 0 THEN
        colorPalette(nrOfColors) = newColor
        colorCode% = nrOfColors
        nrOfColors = nrOfColors + 1
    END IF

    add_color = colorCode%
END SUB


SUB load_palette
    FOR c = 0 TO nrOfColors - 1
        colorValue% = colorPalette(c).Red + (colorPalette(c).Green * 256) + (colorPalette(c).Blue * 65536)
        PALETTE c, colorValue%
    NEXT
END SUB


SUB load_image (filename AS STRING, imgdata() AS INTEGER)
    DIM ENT AS BMPEntry
    DIM BMP AS BMPHeader

    OPEN filename FOR BINARY AS #1
    GET #1, 1, ENT
    GET #1, , BMP

    REDIM imgdata(BMP.PWidth, BMP.PDepth) AS INTEGER

    ' Find out what this does
    ' Without this code the colors are incorrect
    ' loads palette into memory, iamge palette is stored in BMP.pal
    a$ = " "

    colorOffset% = nrOfColors

    SEEK #1, ENT.Offset + 1 - 1024
    FOR Colr = 0 TO 255
        GET #1, , a$: Blu = ASC(a$) \ 4
        GET #1, , a$: Grn = ASC(a$) \ 4
        GET #1, , a$: Red = ASC(a$) \ 4

        colorCode% = add_color(Red, Grn, Blu)

        GET #1, , a$ '--- skip unused spacer byte
    NEXT Colr

    ' Read image data and store it in imgdata
    SEEK #1, ENT.Offset + 1
    byte$ = " "
    FOR y = BMP.PDepth TO 0 STEP -1
        FOR x = 0 TO BMP.PWidth - 1
            GET #1, , byte$
            imgdata(x, y) = colorOffset% + ASC(byte$)
        NEXT
    NEXT
    CLOSE #1
END SUB
