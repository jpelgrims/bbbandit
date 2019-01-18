map_width = 20
map_height = 20

DIM world(map_width - 1, map_height - 1) AS INTEGER
world = create_world(map_width, map_height)

PRINT world(0, 0)
PRINT world(15, 0)
PRINT world(19, 0)

DECLARE create_world (w(map_width-1,map_height-1) as integer)
FUNCTION create_world (map_width AS INTEGER, map_height AS INTEGER)
    ' create a world, store it in a multimdimensional array and return it
    DIM world(map_width - 1, map_height - 1) AS INTEGER

    FOR y = 0 TO map_height - 1
        FOR x = 0 TO map_width - 1
            world(x, y) = 0
        NEXT
    NEXT

    FOR i = 0 TO map_width - 1
        world(i, 0) = 1
    NEXT

    create_world = world
END FUNCTION
