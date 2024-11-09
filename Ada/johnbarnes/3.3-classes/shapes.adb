
with Ada.Numerics;
with Ada.Numerics.Elementary_Functions;

package body Shapes is
    -- We have to implement the `Area' abstract function for `Point'. See
    -- comment in "shapes.ads".
    function Area(P : Point) return Float is
    begin
        return 0.0;
    end Area;

    function Area(C : Circle) return Float is
        use Ada.Numerics;
    begin
        return Pi * C.Radius ** 2;
    end Area;

    function Area(T : Triangle) return Float is
        use Ada.Numerics.Elementary_Functions;
        S : Float;
    begin
        S := (T.Side_A + T.Side_B + T.Side_C) / 2.0;
        return Sqrt(S * (S - T.Side_A) * (S - T.Side_B) * (S - T.Side_C));
    end Area;
end Shapes;
