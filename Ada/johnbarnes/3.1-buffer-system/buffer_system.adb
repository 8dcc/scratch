
package body Buffer_System is
    procedure Load(B : out Buffer; S : in String) is
    begin
        B.Start := 1;
        B.Finish := S'Length;
        B.Data(B.Start .. B.Finish) := S;
    end Load;

    procedure Get(B : in out Buffer; C : out Character) is
    begin
        C := B.Data(B.Start);
        B.Start := B.Start + 1;
    end Get;
end Buffer_System;
