
with Objects; use Objects;

package Shapes is
    type Point is new Object with null record;

    -- We now have to declare an `Area' function for `Point', since
    -- `Object.Area' is abstract, and we can't use it directly.
    function Area(P : Point) return Float;

    type Circle is new Object with
        record
            Radius : Float;
        end record;

    function Area(C : Circle) return Float;


    type Triangle is new Object with
        record
            Side_A : Float;
            Side_B : Float;
            Side_C : Float;
        end record;

    function Area(T : Triangle) return Float;
end Shapes;
