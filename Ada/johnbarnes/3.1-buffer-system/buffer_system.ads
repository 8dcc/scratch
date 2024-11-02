
package Buffer_System is
    type Buffer is private;
    procedure Load(B : out Buffer; S : in String);
    procedure Get(B : in out Buffer; C : out Character);
private
    Max: constant Integer := 80;
    type Buffer is
        record
            Data : String(1 .. Max);
            Start : Integer := 1;
            Finish : Integer := 0;
        end record;
end Buffer_System;
