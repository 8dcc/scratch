with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Case_Statement is
   N: Integer;

   -- We can initialize variables in the declarations
   I: Integer := 1;
begin
   loop
      Put("Enter an integer value: ");
      Get(N);

      -- Put_Line only accepts strings, but Put accepts more types.
      --
      -- We specify "Width => 0" because by default the number form of Put adds
      -- padding to the number.
      Put("Got number: ");
      Put(N, Width => 0);

      -- Print the newline after the number. The function takes no parameters,
      -- so we CAN'T type the emtpy parentheses.
      New_Line;

      case N is
         when 0..10 =>
            Put("Is between 0 and 10, inclusive; and ");

            if N = 0 then
               -- Even as well, and could be checked on the `case', but wanted
               -- to make a `elsif' condition.
               -- Note how a single '=' is used for comparisons.
               Put_Line("is zero.");
            elsif N mod 2 = 0 then
               Put_Line("is an even number.");
            else
               Put_Line("is an odd number.");
            end if;
         when 100 =>
            Put_Line("Is exactly 100.");
            -- Note the lack of `break', since there is no fall through
         when others =>
            -- Uses `when others' instead of `default'
            Put("Is not in a handled range. Printing close values.");

            -- First iteration, with a `while' loop
            I := N - 5;
            while I < N loop
               -- The Integer'Image part is used for converting the integer to a
               -- string. It inserts a space before the number. We can also use
               -- I'image.
               --
               -- The `&' operator is used for string concatenation.
               Put(Integer'Image(I) & ",");

               -- There is no increment operator
               I := I + 1;
            end loop;

            -- Print the number itself
            Put(" [");
            Put(N, Width => 0);
            Put(']');

            -- Alternative iteration form, with a `for' loop.
            -- We also use J'Image as mentioned above.
            for J in N+1..N+5 loop
               Put("," & J'Image);
            end loop;

            -- Finally, print the newline
            New_Line;
      end case;
   end loop;
end Case_Statement;
