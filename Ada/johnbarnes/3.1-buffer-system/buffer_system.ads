
package Buffer_System is
    -- If this exception is raised, the Ada Run Time System will print the
    -- following error message:
    --
    --   raised BUFFER_SYSTEM.ERROR : buffer_system.adb:123
    --
    -- Each exception can be identified by its context, there is no need to
    -- define any error code.
    Error: exception;

    type Buffer is private;
    procedure Load(B : out Buffer; S : in String);
    procedure Get(B : in out Buffer; C : out Character);
    function Is_Empty(B : in Buffer) return Boolean;
private
    Max: constant Integer := 80;
    type Buffer is
        record
            Data : String(1 .. Max);
            Start : Integer := 1;
            Finish : Integer := 0;
        end record;
end Buffer_System;
