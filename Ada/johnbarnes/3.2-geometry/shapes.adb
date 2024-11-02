
-- See note in objects.adb

with Ada.Numerics;
with Ada.Numerics.Elementary_Functions;

package body Shapes is
    function Area(C : Circle) return Float is
        use Ada.Numerics;
    begin
        -- As expected, operators use the normal precedence rules, so the
        -- exponent is evaluated before the multiplication.
        return Pi * C.Radius ** 2;
    end Area;

    function Area(T : Triangle) return Float is
        use Ada.Numerics.Elementary_Functions;

        -- Semiperimeter, see below.
        S : Float;
    begin
        -- Calculate the area of the `Triangle' from its sides, using Heron's
        -- formula.
        S := (T.Side_A + T.Side_B + T.Side_C) / 2.0;
        return Sqrt(S * (S - T.Side_A) * (S - T.Side_B) * (S - T.Side_C));
    end Area;
end Shapes;
