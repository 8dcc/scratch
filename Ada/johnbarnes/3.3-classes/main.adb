
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

with Shapes;
with Objects;

procedure Main is
    P : Shapes.Point;
    C : Shapes.Circle;
    T : Shapes.Triangle;

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
    -- NOTE: For a more commented version, see also the previous excercise, 3.2.
    P := (1.0, 2.0);

    C := (3.0, 4.0, 7.5);

    -- The triangle now inherits the properties from the `Point', rather than
    -- from the `Object'.
    --
    -- Alternatively, we could initialize the whole triangle with:
    --
    --     T := (Coord_X => 1.0, Coord_Y => 2.0, Side_A => 1.5, Side_B => 3.0,
    --           Side_C => 2.5);
    T := (Objects.Object(P) with Side_A => 1.5, Side_B => 3.0, Side_C => 2.5);

    Put("Printing values:");
    New_Line;

    Print_Object_Data("P", Objects.Distance(P), Shapes.Area(P));
    Print_Object_Data("C", Objects.Distance(C), Shapes.Area(C));
    Print_Object_Data("T", Objects.Distance(T), Shapes.Area(T));
end Main;
