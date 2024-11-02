
with Objects; use Objects;

package Shapes is
    -- See note in "objects.ads" on why we don't make the members private in
    -- this specific example.
    type Circle is new Object with
        record
            Radius : Float;
        end record;

    -- We don't need to specify a `Distance' function for `Circle', since it can
    -- use the same process as an `Object'.
    function Area(C : Circle) return Float;

    -- The `Point' type inherits from `Object', but doesn't extend it (neither
    -- in record members nor in functions).
    type Point is new Object with null record;

    type Triangle is new Object with
        record
            Side_A : Float;
            Side_B : Float;
            Side_C : Float;
        end record;

    function Area(T : Triangle) return Float;
end Shapes;
