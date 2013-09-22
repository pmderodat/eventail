with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNAT.Strings;

package body Repositories is

   procedure Report_Error (Msg : String);

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error (Msg : String) is
   begin
      --  TODO??? generate appropriate warning/error

      Put_Line (Standard_Error, "Error while loading projects: " & Msg);
   end Report_Error;

   ----------
   -- Load --
   ----------

   procedure Load
     (Project_Roots : Project_File_Vectors.Vector;
      Repository    : out Repository_Type)
   is

      GNAT_Version  : GNAT.Strings.String_Access;
      Project_Group : Project_Group_Access;

   begin
      --  Initialize the project environment if needed

      if Repository.Env = null then
         Initialize (Repository.Env);
         Repository.Env.Set_Path_From_Gnatls ("gnatls", GNAT_Version);
         GNAT.Strings.Free (GNAT_Version);
      end if;

      --  Then load all project files and add the to the repository

      for Prj_File of Project_Roots loop
         Project_Group := new Project_Group_Type;
         Repository.Project_Groups.Insert (Prj_File, Project_Group);

         --  TODO??? handle GNATCOLL.Projects.Invalid_Project errors.

         Project_Group.Tree.Load
           (Root_Project_Path => Prj_File,
            Env               => Repository.Env,
            Errors            => Report_Error'Access);
      end loop;
   end Load;

   procedure Free (Repository : out Repository_Type) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Project_Group_Type, Project_Group_Access);

   begin
      --  First, free all project groups
      for Prj of Repository.Project_Groups loop
         Free (Prj);
      end loop;
      Repository.Project_Groups.Clear;

      --  Finally, free the project environment
      Free (Repository.Env);
   end Free;

end Repositories;
