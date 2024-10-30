with Ada.Text_IO;
with Ada.Integer_Text_IO;

-- NOTE I would have used a Print_Date procedure, but since I currently don't
-- know how to declare types globally (I think they need to be inside a
-- package), I will just print it directly from the main function.

procedure Date_Record is
    -- Note the lack of double-quotes. We are basically defining a C enum.
    type Month_Name is
        (January, February, March, April, May, June, July, August, September,
         October, November, December);

    type Date is
       record
           Day : Positive;
           Month : Month_Name;
           Year : Positive;
       end record;

    Today : constant Date := (Day => 24, Month => May, Year => 1819);
begin
    Ada.Integer_Text_IO.Put(Today.Day, Width => 0);
    Ada.Text_IO.Put("-" & Month_Name'Image(Today.Month) & "-");
    Ada.Integer_Text_IO.Put(Today.Year, Width => 0);
    Ada.Text_IO.New_Line;
end Date_Record;
