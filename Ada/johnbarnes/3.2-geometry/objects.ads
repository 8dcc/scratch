

package Objects is
    -- We could specify that the members of the `Object' type are private, while
    -- publicly specifying that it's tagged with:
    --
    --     type Object is tagged private;
    --
    -- Then, we could use some constructor/initialization function that hides
    -- the implementation.
    --
    -- However, we will specify the members publicly so it can be initialized
    -- from `Main'.
    type Object is tagged
        record
            Coord_X : Float;
            Coord_Y : Float;
        end record;

    function Distance(O : Object) return Float;
    function Area(O : Object) return Float;
end Objects;
