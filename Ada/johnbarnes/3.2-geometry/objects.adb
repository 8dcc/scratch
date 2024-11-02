
-- The exercise says that we should assume that `Sqrt' and `Pi' are directly
-- visible, but `Pi' is inside `Ada.Numerics' and `Sqrt' is inside
-- `Ada.Numerics.Elementary_Functions.Sqrt'.

with Ada.Numerics.Elementary_Functions;

package body Objects is
    function Distance(O : Object) return Float is
        use Ada.Numerics.Elementary_Functions;
    begin
        -- Notice how Ada has a built-in power operator: A ** B
        return Sqrt(O.Coord_X ** 2 + O.Coord_Y ** 2);
    end Distance;

    function Area(O : Object) return Float is
    begin
        -- Basic `Object' doesn't have Area, but will be implemented by
        -- inherited types (like `Circle').
        return 0.0;
    end Area;
end Objects;
