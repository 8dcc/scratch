
package body Buffer_System is
    procedure Load(B : out Buffer; S : in String) is
    begin
        -- Raise an exception if the input string doesn't fit in the buffer, or
        -- if the buffer has unread data.
        if S'Length > Max or B.Start <= B.Finish then
            raise Error;
        end if;

        B.Start := 1;
        B.Finish := S'Length;
        B.Data(B.Start .. B.Finish) := S;
    end Load;

    procedure Get(B : in out Buffer; C : out Character) is
    begin
        if Is_Empty(B) then
            raise Error;
        end if;

        C := B.Data(B.Start);
        B.Start := B.Start + 1;
    end Get;

    function Is_Empty(B : in Buffer) return Boolean is
    begin
        -- I interpret an empty buffer as a buffer that doesn't have any data
        -- that can be returned by `Get'.
        return B.Start > B.Finish;
    end Is_Empty;
end Buffer_System;
