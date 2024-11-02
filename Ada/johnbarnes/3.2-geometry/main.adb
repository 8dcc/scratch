
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

with Objects;
with Shapes;

procedure Main is
    O : Objects.Object;
    P : Shapes.Point;
    C : Shapes.Circle;
    T : Shapes.Triangle;

    -- We can declare nested procedures.
    procedure Print_Object_Data(Obj_Name : String;
                                Distance : Float;
                                Area : Float) is
    begin
        Put("[" & Obj_Name & "] Distance:");
        Put(Distance);
        New_Line;
        Put("[" & Obj_Name & "] Area:");
        Put(Area);
        New_Line;
    end Print_Object_Data;
begin
    -- Specify coordinates of `Object'.
    O := (1.0, 2.0);

    -- Members of `Point' will inherit the values from `O'. Note how we can't
    -- simply cast the `Object' with:
    --
    --     P := Shapes.Point(O);
    --
    -- Because even though `Point' extends `Object' with nothing, it's still
    -- extending it, so it's considered a "downward conversion", which needs to
    -- be done with an "extension aggregate", like so:
    P := (O with null record);

    -- The circle won't inherit the values from `O'; so each member is
    -- specified, including the coordinates inherited from `Object'.
    C := (3.0, 4.0, 7.5);

    -- Triangle, inherits values from `O', but also extends it by supplying each
    -- side.
    T := (O with Side_A => 1.5, Side_B => 3.0, Side_C => 2.5);

    -- Print some values from each object.
    Put("Printing values:");
    New_Line;

    Print_Object_Data("O", Objects.Distance(O), Objects.Area(O));

    -- Note how there are no `Distance' or `Area' functions for `Point', but we
    -- still need to use `Shapes.FUNC' because that's were the `Point' type was
    -- defined. We can't specify `Objects.FUNC', but it's what will be used
    -- under the hood.
    Print_Object_Data("P", Shapes.Distance(P), Shapes.Area(P));

    Print_Object_Data("C", Shapes.Distance(C), Shapes.Area(C));
    Print_Object_Data("T", Shapes.Distance(T), Shapes.Area(T));
end Main;
