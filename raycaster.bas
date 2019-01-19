DECLARE create_world (width AS INTEGER, height AS INTEGER, world() AS INTEGER)
DECLARE draw_vertical_line (x AS INTEGER, drawStart AS INTEGER, drawEnd AS INTEGER, colour AS INTEGER)

DIM map_width AS INTEGER
DIM map_height AS INTEGER

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

map_width = 24
map_height = 24

screen_width = 320
screen_height = 200

DIM world(map_width - 1, map_height - 1) AS INTEGER


posX = 22: posY = 12
dirX = -1: dirY = 0
planeX = 0: planeY = 0.66

CALL create_world(map_width, map_height, world())
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

        ' Calmmcualte lowest and highest pixel to fill in current stripe
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

        LINE (x, 0)-(x, drawStart), 9
        LINE (x, drawStart)-(x, drawEnd), colour
        LINE (x, drawEnd)-(x, screen_height), 6
    NEXT

    PCOPY 1, 0

    keypress$ = read_key$

    IF keypress$ = "UP" THEN
        posX = posX + dirX * moveSpeed
        posY = posY + dirY * moveSpeed
    ELSEIF keypress$ = "DOWN" THEN
        posX = posX - dirX * moveSpeed
        posY = posY - dirY * moveSpeed
    ELSEIF keypress$ = "RIGHT" THEN
        oldDirX = dirX
        dirX = dirX * COS(-rotSpeed) - dirY * SIN(-rotSpeed)
        dirY = oldDirX * SIN(-rotSpeed) + dirY * COS(-rotSpeed)
        oldPlaneX = planeX
        planeX = planeX * COS(-rotSpeed) - planeY * SIN(-rotSpeed)
        planeY = oldPlaneX * SIN(-rotSpeed) + planeY * COS(-rotSpeed)
    ELSEIF keypress$ = "LEFT" THEN
        oldDirX = dirX
        dirX = dirX * COS(rotSpeed) - dirY * SIN(rotSpeed)
        dirY = oldDirX * SIN(rotSpeed) + dirY * COS(rotSpeed)
        oldPlaneX = planeX
        planeX = planeX * COS(rotdSpeed) - planeY * SIN(rotSpeed)
        planeY = oldPlaneX * SIN(rotSpeed) + planeY * COS(rotSpeed)
    END IF

    endTime# = TIMER(.001)

    frameTime# = endTime - startTime
    IF frameTime# < 1 / 60 THEN
        'SLEEP 1 / 60 - frameTime#
    END IF



WEND

' use this function to also draw ceiling and floor
FUNCTION draw_vertical_line (x, drawStart, drawEnd, colour)
END FUNCTION

SUB create_world (map_width AS INTEGER, map_height AS INTEGER, world() AS INTEGER)
    ' create a world, store it in a multimdimensional array and return it

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

FUNCTION read_key$
    DIM S AS INTEGER
    DIM return_key AS STRING
    S = 0

    'DO
    '    S = INP(&H60)
    'LOOP UNTIL LEN(S)

    S = INP(&H60)

    SELECT CASE S
        CASE 72: return_key = "UP"
        CASE 80: return_key = "DOWN"
        CASE 75: return_key = "LEFT"
        CASE 77: return_key = "RIGHT"
        CASE 16, 1: SYSTEM
        CASE ELSE: return_key = ""
    END SELECT
    read_key = return_key
END FUNCTION

SUB screen_setup (screen_width AS INTEGER, screen_height AS INTEGER)
    SCREEN 7, 0, 1, 0
END SUB


