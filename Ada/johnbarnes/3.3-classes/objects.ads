

package Objects is
    -- We declare the object type as abstract. This way, the programmer can't
    -- declare instances directly, but still declare types that inherit from it.
    type Object is abstract tagged
        record
            Coord_X : Float;
            Coord_Y : Float;
        end record;

    -- Since the new `Distance' function accepts a class wide parameter, it
    -- won't be overwritten by types that inherit from `Object'. This makes
    -- sense, since there is no point in overwriting the distance function.
    function Distance(O : Object'Class) return Float;

    -- We know specify that objects have an `Area' function, but, since it's
    -- abstract, each type that inherits from `Object' will have to overwrite
    -- it.
    function Area(O : Object) return Float is abstract;
end Objects;
