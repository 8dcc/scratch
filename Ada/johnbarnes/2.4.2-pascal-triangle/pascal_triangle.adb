with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Pascal_Triangle is
    -- The height and width (of the last row) of Pascal's triangle is the same.
    Size: constant Integer := 10;

    -- Equivalent to C's:
    --   int arr[SZ][SZ];
    -- The array sizes are specified in the same order as C (outer->inner).
    Pascal_Matrix: array (0 .. Size, 0 .. Size) of Integer;
begin
    -- Fill the matrix. The elements of the matrix that we don't fill are
    -- undefined, so we should not print them.
    Pascal_Matrix(0, 0) := 1;
    for Y in 1 .. Size loop
        Pascal_Matrix(Y, 0) := 1;
        for X in 1 .. Y-1 loop
            Pascal_Matrix(Y, X) := Pascal_Matrix(Y-1, X-1) +
                                   Pascal_Matrix(Y-1, X);
        end loop;
        Pascal_Matrix(Y, Y) := 1;
    end loop;

    -- Print the matrix
    for Y in 0 .. Size loop
        for X in 0 .. Y loop
            Put(Pascal_Matrix(Y, X), Width => 0);
            Put(" ");
        end loop;
        New_Line;
    end loop;
end Pascal_Triangle;
