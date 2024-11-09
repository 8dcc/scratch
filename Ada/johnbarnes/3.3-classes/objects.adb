
with Ada.Numerics.Elementary_Functions;

package body Objects is
    -- Notice how we don't implement an `Area' function for the `Object' type,
    -- since we declared the function as abstract in "objects.ads".

    function Distance(O : Object'Class) return Float is
        use Ada.Numerics.Elementary_Functions;
    begin
        return Sqrt(O.Coord_X ** 2 + O.Coord_Y ** 2);
    end Distance;
end Objects;
