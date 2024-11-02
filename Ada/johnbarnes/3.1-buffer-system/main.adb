with Buffer_System;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
    Buf : Buffer_System.Buffer;
    Str : constant String := "Hello, world.";
    C : Character;
begin
    Buffer_System.Load(Buf, Str);

    for I in 1 .. Str'Length loop
        Buffer_System.Get(Buf, C);
        Put(C);
        Put(' ');
    end loop;
    New_Line;
    Put("Done.");
end Main;
