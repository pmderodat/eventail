with Ada.Command_Line;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Bundling;     use Bundling;
with Repositories; use Repositories;

procedure Eventail is
   R     : Repository_Type := (others => <>);
   Input : Project_File_Vectors.Vector;
begin
   for I in 1 .. Ada.Command_Line.Argument_Count loop
      Input.Append (Create (+Ada.Command_Line.Argument (I)));
   end loop;

   Load (Input, R);
   Bundle (R);
end Eventail;
