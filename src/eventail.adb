with Ada.Command_Line;
with Ada.Containers; use Ada.Containers;
with Ada.Text_IO;    use Ada.Text_IO;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Repositories; use Repositories;

procedure Eventail is
   R     : Repository_Type := (others => <>);
   Input : Project_File_Vectors.Vector;
begin
   for I in 1 .. Ada.Command_Line.Argument_Count loop
      Input.Append (Create (+Ada.Command_Line.Argument (I)));
   end loop;

   Load (Input, R);

   Put_Line ("This is Eventail!");
   Put_Line
     (Count_Type'Image (R.Project_Groups.Length) & " projects are loaded");
end Eventail;
