REM $DYNAMIC

DECLARE create_world (width AS INTEGER, height AS INTEGER, world() AS INTEGER)
DECLARE draw_vertical_line (x AS INTEGER, drawStart AS INTEGER, drawEnd AS INTEGER, screen_height AS INTEGER, colour AS INTEGER)
DECLARE add_color (red AS INTEGER, green AS INTEGER, blue AS INTEGER)
DECLARE load_palette

DIM SHARED map_width, map_height AS INTEGER
DIM SHARED colorPalette(0 TO 512) AS PaletteColor
DIM SHARED nrOfColors AS INTEGER
nrOfColors = 0

'BLACK = add_color(0, 0, 0)
'DARK_GREY = add_color(10, 10, 10)
'LIGHT_GREY = add_color(20, 20, 20)

DIM posX, posY AS DOUBLE
DIM dirX, dirY AS DOUBLE
DIM planeX, planeY AS DOUBLE

DIM running AS INTEGER
DIM startTime AS DOUBLE

DIM textureWidth, textureHeight AS INTEGER

textureWidth = 32
textureHeight = 32

DIM moveSpeed, rotSpeed AS DOUBLE
moveSpeed = 0.05
rotSpeed# = 0.01


running = 1
startTime = TIMER

screen_width = 320
screen_height = 200

DIM buffer(screen_height, screen_width) AS INTEGER
DIM world(1, 1) AS INTEGER
DIM tilesheet(1, 1, 1) AS INTEGER

_TITLE "Basic Battle Bandit 3D"
PRINT "BASIC BATTLE BANDIT 3D"
PRINT ""
PRINT "Press any key to start..."
SLEEP

CALL screen_setup(screen_width, screen_height)

' Show console for debugging purposes
$CONSOLE

CALL set_palette
CALL load_tilesheet("tilesheet.bmp", 32, tilesheet())

'CALL load_palette

log ("testing")

posX = 3: posY = 3
dirX = -1: dirY = 0
planeX = 0: planeY = 0.66

'CALL create_world(24, 24, world())
CALL load_map("map.txt", world())

' Avoid flickering with fast pset by waiting until screen fully drawn
WAIT &H3DA, 8
WAIT &H3DA, 8, 8


DIM zBuffer(0 TO screen_width) AS INTEGER

'CALL shift_palette(0, 0, 0)

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

        ' Calculate distance of perpendicular ray (no euclidean distance to avoid fisheye effect)
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

        drawStart = -lineHeight / 2 + (screen_height / 2)

        IF drawStart < 0 THEN
            drawStart = 0
        ELSEIF drawStart >= screen_height THEN
            drawStart = screen_height - 1
        END IF

        DIM drawEnd AS INTEGER
        drawEnd = lineHeight / 2 + (screen_height / 2)
        IF drawEnd >= screen_height THEN
            drawEnd = screen_height - 1
        ELSEIF drawEnd < 0 THEN
            drawEnd = 0
        END IF

        'Wall Texturing calculations

        DIM textureNr AS INTEGER
        DIM wallX AS DOUBLE 'Where wall was hit
        textureNr = world(mapX, mapY)

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

        DIM height AS INTEGER
        height = 32
        IF lineHeight < 32 THEN
            height = lineHeight
        END IF

        FOR y = drawStart TO drawEnd - 1
            d% = y - screen_height * 0.5 + lineHeight * 0.5
            texY = ((d% * textureHeight) / lineHeight)


            colour = tilesheet(textureNr, texX, texY)

            ' Make tesxture darker somehow
            IF side = 1 THEN
                colour = tilesheet(textureNr + 1, texX, texY)
            END IF
            buffer(y, x) = colour
        NEXT

        ' zbuffer for sprite casting
        zBuffer(x) = perpWallDist

        ' Floor & ceiling casting

        DIM floorXWall, floorYWall AS DOUBLE

        IF (side = 0 AND rayDirX > 0) THEN
            floorXWall = mapX
            floorYWall = mapY + wallX
        ELSEIF (side = 0 AND rayDirX < 0) THEN
            floorXWall = mapX + 1
            floorYWall = mapY + wallX
        ELSEIF (side = 1 AND rayDirY > 0) THEN
            floorXWall = mapX + wallX
            floorYWall = mapY
        ELSE
            floorXWall = mapX + wallX
            floorYWall = mapY + 1
        END IF

        DIM distWall, currentDist, weight AS DOUBLE
        DIM currentFloorX, currentFloorY AS DOUBLE
        DIM floorTexX, floorTexY AS INTEGER

        distWall = perpWallDist

        IF (drawEnd < 0) THEN
            drawEnd = screen_height
        END IF

        FOR y = drawEnd + 1 TO screen_height - 1
            currentDist = screen_height / (2.0 * y - screen_height) 'make lookup table

            weight = currentDist / distWall

            ' tile coordinates
            currentFloorX = weight * floorXWall + (1.0 - weight) * posX
            currentFloorY = weight * floorYWall + (1.0 - weight) * posY

            ' pixel coordinates for texture
            floorTexX = INT(currentFloorX * textureWidth) MOD textureWidth
            floorTexY = INT(currentFloorY * textureHeight) MOD textureHeight

            ' floor
            buffer(y, x) = tilesheet(4, floorTexX, floorTexY)

            ' ceiling
            buffer(screen_height - y, x) = tilesheet(5, floorTexX, floorTexY)
        NEXT

    NEXT

    'CALL draw_buffer(buffer())
    FOR x = 0 TO screen_width - 1
        FOR y = 0 TO screen_height - 1
            ' Reduce flickering


            ' Fast PSET
            DEF SEG = &HA000
            POKE y * 320& + x, buffer(y, x)
            DEF SEG



            'PSET (x, y), buffer(y, x)
        NEXT
    NEXT

    'PCOPY 1, 0

    ' Clear buffer
    FOR x = 0 TO screen_width - 1
        FOR y = 0 TO screen_height - 1
            buffer(y, x) = 0
        NEXT
    NEXT


    DIM nextPosX AS DOUBLE
    DIM nextPosY AS DOUBLE
    nextPosX = posX
    nextPosY = posY

    IF IS_PRESSED("UP") THEN
        nextPosX = posX + dirX * moveSpeed
        nextPosY = nextPosY + dirY * moveSpeed
    END IF

    IF IS_PRESSED("DOWN") THEN
        nextPosX = nextPosX - dirX * moveSpeed
        nextPosY = nextPosY - dirY * moveSpeed
    END IF

    IF IS_PRESSED("RIGHT") THEN
        nextPosX = nextPosX + planeX * moveSpeed
        nextPosY = nextPosY + planeY * moveSpeed
    END IF

    IF IS_PRESSED("LEFT") THEN
        nextPosX = nextPosX - planeX * moveSpeed
        nextPosY = nextPosY - planeY * moveSpeed
    END IF

    IF walkable(nextPosX, nextPosY, world()) = 1 THEN
        posX = nextPosX
        posY = nextPosY
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


FUNCTION walkable (x AS DOUBLE, y AS DOUBLE, world() AS INTEGER)
    walkable = 1
    wallHitDist = 0.1

    IF world(INT(x + wallHitDist), INT(y + wallHitDist)) <> 0 THEN
        walkable = 0
    ELSEIF world(INT(x - wallHitDist), INT(y - wallHitDist)) <> 0 THEN
        walkable = 0
    ELSEIF world(INT(x + wallHitDist), INT(y - wallHitDist)) <> 0 THEN
        walkable = 0
    ELSEIF world(INT(x - wallHitDist), INT(y + wallHitDist)) <> 0 THEN
        walkable = 0
    END IF

END FUNCTION


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

    red = INT(red / 4)
    green = INT(green / 4)
    blue = INT(blue / 4)

    ' Check if palette is not full
    IF nrOfColors = 256 THEN
        PRINT "ERROR: The palette cannot contain more than 256 colors"
        PCOPY 1, 0
        SLEEP
        SYSTEM
    END IF
    colorCode% = 0
    stored% = 0
    FOR c = 0 TO nrOfColors - 1

        IF colorPalette(c).Red = red AND colorPalette(c).Green = green AND colorPalette(c).Blue = blue THEN
            ' Color is already stored in palette
            stored% = 1
            colorCode% = c
            EXIT FOR
        END IF
    NEXT

    IF stored% = 0 THEN
        colorPalette(nrOfColors).Red = red
        colorPalette(nrOfColors).Blue = blue
        colorPalette(nrOfColors).Green = green
        colorCode% = nrOfColors
        nrOfColors = nrOfColors + 1
    END IF


    add_color = colorCode%
END SUB

SUB load_palette
    FOR c = 0 TO nrOfColors - 1
        'colorValue% = colorPalette(c).Red + (colorPalette(c).Green * 256) + (colorPalette(c).Blue * 65536)
        'PALETTE c + 1, colorValue%  ' is slow
        OUT &H3C8, c ' color palette nr
        OUT &H3C9, colorPalette(c).Red ' red
        OUT &H3C9, colorPalette(c).Green ' green
        OUT &H3C9, colorPalette(c).Blue ' blue
        OUT &H3C7, 0
    NEXT
END SUB

SUB load_tilesheet (filename AS STRING, tilesize AS INTEGER, tilesheet() AS INTEGER)
    DIM tiledata(1, 1) AS INTEGER
    CALL load_image(filename, tiledata())

    DIM ENT AS BMPEntry
    DIM BMP AS BMPHeader

    OPEN filename FOR BINARY AS #1
    GET #1, 1, ENT
    GET #1, , BMP
    CLOSE #1

    nrOfTiles% = (BMP.PWidth / tilesize) * (BMP.PDepth / tilesize)

    DIM sheetWidth, sheetHeight AS INTEGER
    sheetWidth = BMP.PWidth / tilesize
    sheetHeight = BMP.PDepth / tilesize
    REDIM tilesheet(nrOfTiles%, 0 TO tilesize, 0 TO tilesize) AS INTEGER

    FOR y = 0 TO sheetHeight - 1
        FOR x = 0 TO sheetWidth - 1
            tileNr% = x + (sheetWidth * y)
            FOR i = 0 TO tilesize - 1
                FOR j = 0 TO tilesize - 1
                    tilesheet(tileNr%, j, i) = tiledata(j + (tilesize * x), i + (tilesize * y))
                NEXT
            NEXT
        NEXT
    NEXT

    testx% = tilesheet(4, 0, 0)
    testx% = tilesheet(5, 0, 0)

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

    ' Colors are stored in biutmap starting from 0
    ' Need a mapping from bitmap color nr to palette color nr
    DIM bitmapToPalette(0 TO BMP.NumColors) AS INTEGER

    SEEK #1, ENT.Offset + 1 - 1024
    FOR Colr = 0 TO BMP.NumColors
        GET #1, , a$: Blu = ASC(a$) \ 4
        GET #1, , a$: Grn = ASC(a$) \ 4
        GET #1, , a$: Red = ASC(a$) \ 4

        'colorCode% = add_color(Red, Grn, Blu)
        colorCode% = rgb_to_palette(Red, Grn, Blu)
        'PRINT Red, Grn, Blu
        'PRINT colorCode%
        'PCOPY 1, 0
        'SLEEP

        ' Store mapping
        bitmapToPalette(Colr) = colorCode%

        GET #1, , a$ '--- skip unused spacer byte
    NEXT Colr

    ' Read image data and store it in imgdata
    SEEK #1, ENT.Offset + 1
    byte$ = " "
    FOR y = BMP.PDepth TO 0 STEP -1
        FOR x = 0 TO BMP.PWidth - 1
            ' byte$ contains bitmap color nr
            ' use bitmapToPalette mapping to convert to palette color nr
            GET #1, , byte$
            imgdata(x, y) = bitmapToPalette(ASC(byte$))
        NEXT
    NEXT
    CLOSE #1
END SUB

FUNCTION SHIFT% (var%, numshifts%)
    mulval% = 2 ^ numshifts%
    var% = var% * mulval%
    SHIFT% = var%
END FUNCTION

FUNCTION color_blend ()
END FUNCTION


FUNCTION rgb_to_palette% (red AS INTEGER, green AS INTEGER, blue AS INTEGER)
    paletteNr% = 0
    smallestDiff& = 1000000

    FOR i = 0 TO nrOfColors
        diff& = SQR((red - colorPalette(i).Red) ^ 2 + (green - colorPalette(i).Green) ^ 2 + (blue - colorPalette(i).Blue) ^ 2)
        IF diff& < smallestDiff& THEN
            smallestDiff& = diff&
            paletteNr% = i
        END IF
    NEXT



    rgb_to_palette = paletteNr%
END FUNCTION

SUB set_palette_color (index AS INTEGER, red AS INTEGER, green AS INTEGER, blue AS INTEGER)
    colorPalette(index).Red = red
    colorPalette(index).Green = green
    colorPalette(index).Blue = blue
END SUB

SUB set_palette
    DIM s AS INTEGER
    FOR i = 0 TO 16
        s = 64 / 16
        CALL set_palette_color(0 + i, i * s, i * s, i * s) ' Greyscale

        CALL set_palette_color(16 + i, i * s, 0, 0) ' Red
        CALL set_palette_color(32 + i, 0, i * s, 0) 'Green
        CALL set_palette_color(48 + i, 0, 0, i * s) ' Blue

        CALL set_palette_color(64 + i, 0, i * s, i * s) ' Teal?
        CALL set_palette_color(80 + i, i * s, 0, i * s) ' Purple
        CALL set_palette_color(96 + i, i * s, i * s, 0) ' Teal?

        CALL set_palette_color(128 + i, 0, i * s / 2, i * s)
        CALL set_palette_color(144 + i, 0, i * s, i * s / 2)

        CALL set_palette_color(160 + i, i * s / 2, i * s, 0)
        CALL set_palette_color(176 + i, i * s, i * s / 2, 0)

        CALL set_palette_color(192 + i, i * s / 2, 0, i * s)
        CALL set_palette_color(208 + i, i * s, 0, i * s / 2)

        CALL set_palette_color(224 + i, i * s / 2, i * s, i * s)
        CALL set_palette_color(240 + i, i * s, i * s / 2, i * s)

    NEXT

    nrOfColors = 255
    CALL load_palette
END SUB

SUB shift_palette (red AS INTEGER, green AS INTEGER, blue AS INTEGER)
    FOR c = 0 TO nrOfColors - 1
        colorPalette(c).Red = colorPalette(c).Red + red
        colorPalette(c).Green = colorPalette(c).Green + green
        colorPalette(c).Blue = colorPalette(c).Blue + blue

        IF colorPalette(c).Red >= 64 THEN
            colorPalette(c).Red = 63
        END IF

        IF colorPalette(c).Green >= 64 THEN
            colorPalette(c).Green = 63
        END IF

        IF colorPalette(c).Blue >= 64 THEN
            colorPalette(c).Blue = 63
        END IF

    NEXT
    CALL load_palette
END SUB

SUB log (message AS STRING)
    _DEST _CONSOLE
    PRINT message
    _DEST 0
END SUB
